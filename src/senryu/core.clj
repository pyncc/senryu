(ns senryu.core
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

;; upon adding new word, review existing word-counts

(defn update-state-from-word
  "Update collector with counts, possibly including boundaries of various
  syllable aggregations."
  [collector word]
  (let [syllables (word->syllables word)]))
       ;; (swap! collector update :counts conj (word->syllables word))
       ;; tracking: syllable collections of 5 and 7
       ;; determine overlapping syllable sequences as index or range coordinates
       ;; {:fives #{[0 1 2 3]}
       ;;  :sevens #{[1 2 3 4 5]}}
       ;; to give the best chance of finding a solution

;; interesting to think about:
;; ? can this, would this ever emit more than one stanza at a time?
;; : since the evaluation is happening each word, probably not since
;;   previous steps would have 'greedily' emited a solution previously
;;   if possible
(defn emit-stanza
  "Scan words, boundaries and emit stanza(s?)"
  [collector])
  ;; find non-overlapping sequences from syllable counts
  ;; and try to put stanzas together
  ;; once we have two 5s and one 7, emit the stanza
  ;; and clear these from the tracking state

(defn process
  [input]
  (let [collector (atom {:counts []})]
    (->> (iterator-seq (Scanner. input))
         ;; compute word-at-a-time from word stream
         ;; emit stanza-at-a-time
         (map (partial update-state-from-word collector))

         ;; this runs each word, alternatively this could be decoupled somehow...
         ;; we also want to make sure there is a flush step, where emit-stanza
         (keep (fn [_] (emit-stanza collector))))))
