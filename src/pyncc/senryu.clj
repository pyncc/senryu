(ns pyncc.senryu
  "A not-very-serious haiku generator"
  (:require
   [clojure.string :as str])
  (:import
   java.util.Scanner))

;; Steps
;; Take in lines (one at a time, in case they are infinite)
;; Take in words from this line, one at a time
;; categorize these words by syllables
;; exclude words with very large syllable counts
;; retain phrases, that is do not randomly collect them, keep track of relative ordering.

;; Do a kind of scanning that retains various (possibly overlapping)
;;   boundaries of syllable collections
;;   such that 'every so often' a process can go scan to
;;   find non-overlapping sequences of 5-7-5 syllable counts

;; Goals:
;; Efficient such that its possible to consume and emit infinite sequences
;; Delightful, delicious
;; Well-structured code, –- I can come back and piece together why certain decisions were made
;; Semi-sensical output based on keeping phrases together (if possible) but being thrifty
;;   such that words may be skipped if they block the overall syllable count

(def syllable-boundary-match (re-pattern "[^aeuioyAEUIOY][aeuioyAEUIOY]"))

(def vowel? #{\a \e \u \i \o \y \A \E \U \I \O \Y})

(defn word->syllables
  "Return an integer of an English word's (approximate) syllable count."
  [word]
  (if (< (count word) 6)
    1
    (reduce
     +
     0
     (remove
      nil?
      [;; if starts with a vowel, add one
       (when (vowel? (first word))
         1)
       ;; count consonant-vowel boundaries
       (count (re-seq syllable-boundary-match word))
       ;; decrease count for ending with e
       (when (str/ends-with? word "e")
         -1)]))
       ;; count certain vowel pairs, such as ia or ie?

    #_(-> word
        ;; if starts with a vowel, add one
          #_(subs 1) ; trim off first letter to avoid double-counting
          (->> (re-seq syllable-boundary-match)) ; basic heuristic of syllables in words
          (count) ; count the syllable boundary matches
          #_(inc)))) ; include the first sound in the word in the count

(defn naive-grouping
  [syllable-groups word-syllables]
  (loop [acc []
         group []
         word (first word-syllables)
         words (rest word-syllables)
         syllable (first syllable-groups)
         syllables (rest syllable-groups)]
    (if (and syllable word)
      ;; does placing curr word into curr-group "fit" based on syllable count?
      ;; does curr-group's sum exactly match curr-syllable?
      (let [group-total (reduce + (map second group))]
        (cond
          ;; current word fits
          (< (+ group-total (second word)) syllable)
          (recur acc
                 (conj group word)
                 (first words)
                 (rest words)
                 syllable
                 syllables)

          ;; current word meets needs
          (= syllable (+ group-total (second word)))
          (recur (conj acc (conj group word))
                 []
                 (first words)
                 (rest words)
                 (first syllables)
                 (rest syllables))

          ;; this word doesn't fit, ignore it and keep trying until you run out
          ;; of words or syllables
          ;; could consider adding it to a discard bin and returning with
          ;; 'remaining words' vector

          :else
          (recur acc
                 group
                 (first words)
                 (rest words)
                 syllable
                 syllables)))
      ;; this being recusive allows some interesting explorations based on
      ;; subsequences or recombinations

      ;; done, no more usable inputs, but is it correct/sufficient?
      ;; if not, return nothing
      (when (= (count acc) (count syllable-groups))
        [acc
         (if word
           (conj words word)
           words)]))))

(defn find-stanza
  "Given a set of words, find sequences of exactly the given syllable counts, returning
  a stanza (set of tuples) and vector of remaining word-syllable tuples.

  (find-stanza [2 1 1]
               [[\"haikus\" 2]
                [\"are\" 1]
                [\"hard\" 1]
                [\"refrigerator\" 5]])

  => [[[\"haikus\"] [\"are\"] [\"hard\"]]
      [\"refrigerator\" 5]]"
  [syllables word-syllables]
  ;; various bucket filling/box packing algorithms go here
  (or
    ;; pass one
   (naive-grouping syllables word-syllables)

   ;; retry the whole thing with different orderings of syllables,
   (comment
     (let [[stanza remaining-words] (naive-grouping (sort syllables) word-syllables)]
       ;; reorder the stanza by original syllable ordering
       :return))))

(defn- exclude-syllables
  "Remove words with syllables that exceed the desired grouping's max"
  [syllables]
  (remove (fn [[_ syllable]] (< (apply max syllables) syllable))))

(defn- emit-stanzas
  "Returns a stateful transducer to consume words and output poem stanzas."
  [syllable-grouping]
  (let [temp (atom [])]
    (fn grouping*
      [rf]
      (fn grouping-xform
        ([] (rf))
        ([result]
         (rf result))
        ([result input]
         (let [curr (swap! temp conj input)]
           (when-let [[stanza extra-words] (find-stanza syllable-grouping curr)]
             (reset! temp extra-words)
             (rf result stanza))))))))

(defn render-match
  "Takes a full set of syllable groups, returns a formatted
  string representing those groups as a senryu"
  [word-groups]
  (str "...\n"
       (str/join
        "\n"
        (map #(apply str (cons "> " (str/join " " (map first %))))
             word-groups))))

(def calculate-syllables
  "Return tuples of [word syllable-count-of-that-word]"
  (map (juxt identity word->syllables)))

(defn calculate
  "Returns a Reducible of stanza groupings, given a lazy sequence of words and
  a grouping of syllable counts.

  (calculate [1 3 1] (\"a\" \"penniless\" \"fool\"))"
  [syllable-grouping words]
  (eduction calculate-syllables
            (exclude-syllables syllable-grouping)
            (emit-stanzas syllable-grouping)
            #_(map render-match)
            words))

(comment
  (->> ["a" "penniless" "fool"]
       (calculate [1 3 1])
       (first)
       (render-match)
       (println))

  (require '[pyncc.data :as data])
  (calculate [5 7 5] data/example)
  (calculate [5 7 5] data/words)

  (map (juxt identity word->syllables) data/example)
  (map (juxt identity word->syllables) data/words)
  (find-stanza [2 1 1] [["haikus" 2] ["are" 1] ["hard" 1] ["refrigerator" 5]])
  (calculate [2 1 1] ["haikus" "are" "hard" "refrigerator"])
  (println (render-match [[["a" 1] ["b" 1] ["c" 1]] [["Z" 10]]])))
