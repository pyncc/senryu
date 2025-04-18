(ns pyncc.syllable
  (:require
   [clojure.string :as str]))

(def syllable-boundary-match (re-pattern "[^aeuioyAEUIOY][aeuioyAEUIOY]"))

(def vowel? #{\a \e \u \i \o \y \A \E \U \I \O \Y})

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

