(ns senryu.core
  "Not very serious haiku")


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
