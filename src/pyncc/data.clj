(ns pyncc.data
  (:require
   [pyncc.senryu :as senryu]
   [clojure.string :as str]))

(def words
  ["haikus"
   "are"
   "hard"
   "so"
   "I"
   "can"
   "try"
   "to"
   "write"
   "refrigerator"])

(def example
  ["haikus"
   "are"
   "easy"
   "but"
   "sometimes"
   "they"
   "don't"
   "make"
   "sense"
   "refrigerator"])

(def words*
  [["haikus" 2]
   ["are" 1]
   ["hard" 1]
   ["so" 1]
   ["I" 1]
   ["can" 1]
   ["try" 1]
   ["to" 1]
   ["write" 3]
   ["refrigerator" 5]])

(def correct-words*
  "Queried from https://syllablecounter.net/count"
  [["alias" 1]
   ["grief" 1]
   ["balance" 2]
   ["springy" 2]
   ["alternative" 4]
   ["write" 1]
   ["bright" 1]
   ["aliens" 2]
   ["brightly" 2]])

(def summing-xform
  (let [c (atom 0)]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (swap! c + input)
         (println "summing" @c input (conj result [input @c]))
         (conj result [input @c]))))))

(defn summing
  []
  (let [c (atom 0)]
    (fn
      [input]
      (swap! c + input)
      (println "summing" @c input)
      [input @c])))

(defn naive-grouping
  [syllable-groups word-syllables]
  (loop [acc []
         group []
         word (first word-syllables)
         words (rest word-syllables)
         syllable (first syllable-groups)
         syllables (rest syllable-groups)]
    #_(println "loop" acc group word words syllable syllables)
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

          ;; this word doesn't fit, keep trying until you run out of words or syllables

          ;; alternatively, if the current word doesn't fit,
          ;; try next (eventually all) syllables
          ;; this would require backtracking though...

          ;; an  alternative to that approach would have better contiguity characteristics:
          ;; retry the whole thing with different orderings of syllables, from the outside not internally to this function...
          :else
          (recur acc
                 group
                 (first words)
                 (rest words)
                 syllable
                 syllables)))
      ;; this being recusive allows some interesting explorations based on
      ;; subsequences or recombinations

      ;; done, no more usable inputs,
      ;; but is it correct/sufficient?
      (when (= (count acc) (count syllable-groups))
        [acc
         (if word
           (conj words word)
           words)]))))

;; this implementation does not consider incremental
;; changes, it will simply (re)compute each time using all inputs
(defn step!
  "Given a set of words, find (non-overlapping) sequences
  of exactly the given syllable counts.

  words with syllables greater than max inputs will be filtered out

  considerations, special rules
  (its not clear to me yet if this function should handle all these or if
   a subsequent function should handle these)
  * priority is given to the largest syllables, that is the shortest subsequence
    will be returned for the highest syllable
  * blocking words, ones that cause the subsequence to exceed syllable count will be
    elided.


  [[\"haikus\" 2]
   [\"are\" 1]
   [\"hard\" 1]
   [\"so\" 1]
   [\"I\" 1]
   [\"can\" 1]
   [\"try\" 1]
   [\"to\" 1]
   [\"write\" 1]
   [\"less\" 1]
   [\"refrigerator\" 5]]

  [5 7 5]
  =>
  [
  [[\"haikus\" 2]
  [\"are\" 1]
  [\"hard\" 1]]
[[\"so\" 1]
[\"I\" 1]
[\"can\" 1]
[\"try\" 1]
[\"to\" 1]
[\"write\" 1]
[\"less\" 1]]
[[\"refrigerator\" 5]]
  ]"
  [syllables word-syllables]
  ;; various bucket filling/box packing algorithms go here
  (or
    ;; pass one
   (naive-grouping syllables word-syllables))

  #_(when (< 4 (reduce + (map second word-syllables)))
      word-syllables))

;; or, the above function could return a sequence of all possible subsequences that satisfy the list of syllable count sums.
;; the main issue with that is non-overlapping, a 5-syllable subsequence could share words with a 7-syllable subsequence
;; should just implement the naive version first and have more info about how to calculate or commnicate non-overlapping-ness.

(defn exclude-syllables
  "Remove words with syllables that exceed the desired grouping's max"
  [syllables]
  (remove (fn [[_ syllable]] (< (apply max syllables) syllable))))

;; try to see what holding a temporary collection looks like
;; this will group ordered inputs by their collective sum
(defn grouping
  [syllable-grouping]
  (let [temp (atom [])]
    (fn grouping*
      [rf]
      (fn grouping-xform
        ([] (rf))
    ;; probably need to "flush" temp to result here
        ([result]
         #_(println "grouping, result" result)
         (rf result))
        ([result input]
         (let [curr (swap! temp conj input)]
           (when-let [[found extra-words] (step! syllable-grouping curr)]
             (reset! temp extra-words)
             (rf result found))))))))

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
  (map (juxt identity senryu/word->syllables)))

(defn calculate
  "Returns a Reducible of stanza groupings, given a lazy sequence of words and
  a grouping of syllable counts.

  (calculate [1 3 1] (\"a\" \"penniless\" \"fool\"))"
  [syllable-grouping words]
  (eduction calculate-syllables
            (exclude-syllables syllable-grouping)
            (grouping syllable-grouping)
            (map render-match)
            words))

(comment
  (calculate [5 7 5] example)
  (map (juxt identity senryu/word->syllables) example)
  (println (render-match [[["a" 1] ["b" 1] ["c" 1]] [["Z" 10]]])))
