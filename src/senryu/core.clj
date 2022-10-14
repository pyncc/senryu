(ns senryu.core
  "A not-very-serious haiku generator")


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

(def syllable-boundary-match (re-pattern "[^aeuioAEUIO][aeuioAEUIO]"))


(defn word->syllable
  "Return an integer count of a string's English language syllable count."
  [word]
  (if (< (count word) 5)
    1
    (->> (subs word 1) ; trim off first letter to avoid double-counting
         (re-seq syllable-boundary-match) ; basic heuristic of syllables in words
         (count) ; count the syllable boundary matches
         (inc) ; include the first sound in the word in the count
         )))
