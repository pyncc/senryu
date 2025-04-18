(ns pyncc.data)

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
   ["write" 1]
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
