(ns pyncc.senryu
  "A not-very-serious haiku generator. Technically these are senryu since they
  are non-traditional limmerick styles."
  (:require
   [pyncc.syllable :as syllable]
   [clojure.string :as str]))

;; Goals:
;; Efficient such that its possible to consume and emit infinite sequences
;; Delightful, delicious
;; Well-structured code, – I can come back and piece together why
;; certain decisions were made and make changes easily

(defn naive-grouping
  "A _naive_ approach at grouping words based on syllable counts. Returns nil if no stanza is found.

  Syllable-groups is a vector of syllables to group by.
  Word-syllables is a vector of (word, syllable) tuples.

  Returns a tuple of (stanza, extra-words),
  where stanza is a vector of word vectors based on the index of syllable-groups
    and each syllable in syllable-groups is evaluated in order
    and extra-words is a collection of unused (word, syllable) tuples

  Example:
    syllable-groups [3 4 2]
    word-syllables [[\"words\" 1] [\"are\" 1] [\"ok\" 1] [\"refrigerator\" 5]
                    [\"but\" 1] [\"pictures\" 2]  [\"speak\" 1] [\"volumes\" 2]]]
  {:stanza [[\"words\" \"are\" \"ok\"] [\"but\" \"pictures\" \"speak\"] [\"volumes\"]]
   :extra-words [[\"refrigerator\" 5]]}

  This implementation will attempt to:
    * retain phrases
    * exclude a word if it doesn't fit into the current syllable-count being evaluated"
  [syllable-groups word-syllables]
  (loop [stanza []
         syll-group []
         word-syll (first word-syllables)
         word-sylls (rest word-syllables)
         syllable (first syllable-groups)
         syllables (rest syllable-groups)]
    (if (and syllable word-syll)
      (let [group-total (reduce + (map second syll-group))
            [word word-syllable] word-syll]
        (cond
          ;; current word fits
          (< (+ group-total word-syllable) syllable)
          (recur stanza
                 (conj syll-group word-syll)
                 (first word-sylls)
                 (rest word-sylls)
                 syllable
                 syllables)

          ;; current word meets needs
          (= syllable (+ group-total word-syllable))
          (recur (conj stanza (conj (mapv first syll-group) word))
                 []
                 (first word-sylls)
                 (rest word-sylls)
                 (first syllables)
                 (rest syllables))

          ;; this word doesn't fit, ignore it and keep trying until you run out
          ;; of words or syllables

          :else
          (recur stanza
                 syll-group
                 (first word-sylls)
                 (rest word-sylls)
                 syllable
                 syllables)))

      ;; no more usable inputs, but is it correct/sufficient?
      ;; if not, return nothing
      (when (= (count stanza) (count syllable-groups))
        {:stanza stanza
         :extra-words (if word-syll
                        (conj word-sylls word-syll)
                        word-sylls)}))))

(defn find-stanza
  "Given a set of words, find sequences of exactly the given syllable counts, returning
  a stanza (set of tuples) and vector of remaining word-syllable tuples.

  (find-stanza [2 1 1]
               [[\"haikus\" 2]
                [\"are\" 1]
                [\"hard\" 1]
                [\"refrigerator\" 5]])

  => {:stanza [[\"haikus\"] [\"are\"] [\"hard\"]]
      :extra-words [[\"refrigerator\" 5]]}"
  [syllables word-syllables]
  ;; various bucket filling/box packing algorithms go here
  (or
    ;; pass one
   (naive-grouping syllables word-syllables)

   ;; retry the whole thing with different orderings of syllables,
   (comment
     (let [{:keys [stanza remaining-words]} (naive-grouping (sort syllables) word-syllables)]
       stanza))))

(defn- exclude-syllables
  "A transducer function to remove (words, syllable) tuples for syllables that
  exceed the desired grouping's max"
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
        ([result] (rf result))
        ([result input]
         (let [curr (swap! temp conj input)]
           (when-let [{:keys [stanza extra-words]}
                      (find-stanza syllable-grouping curr)]
             (reset! temp extra-words)
             (rf result stanza))))))))

(defn render-match
  "Takes a full set of syllable groups, returns a formatted
  string representing those groups as a senryu"
  [word-groups]
  (str "...\n"
       (str/join
        "\n"
        (map #(apply str (cons "> " (str/join " " %)))
             word-groups))))

(def calculate-syllables
  "Return tuples of [word syllable-count-of-that-word]"
  (map (juxt identity syllable/word->syllables)))

(defn calculate
  "Returns a Reducible of stanza groupings, given a sequence of words and
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

  (find-stanza [2 1 1] [["haikus" 2] ["are" 1] ["hard" 1] ["refrigerator" 5]])
  (calculate [2 2] ["haikus" "are" "hard" "refrigerator"])
  (println (render-match [[["a" 2] ["b" 1] ["c" 1]] [["Z" 10]]])))
