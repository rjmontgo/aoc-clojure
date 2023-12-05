(ns day1.part2
  (:require [clojure.string :refer [split join]]))

(defn add-to-trie [trie x]
  (assoc-in trie x (merge (get-in trie x) {:val x :terminal true})))

(defn in-trie? [trie x]
  "Returns true if the value x exists in the specified trie."
  (:terminal (get-in trie x) false))

(defn prefix-matches [trie prefix]
  "Returns a list of matches with the prefix specified in the trie specified."
  (keep :val (tree-seq map? vals (get-in trie prefix))))

(defn build-trie [coll]
  "Builds a trie over the values in the specified seq coll."
  (reduce add-to-trie {} coll))

(def trie (build-trie
             ["one"
              "two"
              "three"
              "four"
              "five"
              "six"
              "seven"
              "eight"
              "nine"]))
(def rev-trie (build-trie
               ["eno" 
                "owt" 
                "eerht"
                "ruof"
                "evif"
                "xis"  
                "neves"
                "thgie"
                "enin"
                ]))

(def valmap {"one" 1
               "two" 2
               "three" 3
               "four" 4
               "five" 5
               "six" 6
               "seven" 7
               "eight" 8
               "nine" 9
               
               "eno" 1
               "owt" 2
               "eerht" 3
               "ruof" 4
               "evif" 5
               "xis" 6
               "neves" 7
               "thgie" 8
               "enin" 9})

(defn my-is-digit [c]
  (let [min (int \1)
        max (int \9)
        val (int c)]
    (>= max val min)))

(defn find-word [string trie]
  "finds the first valid word within a trie and returns the remainder of a string or nil"
  (loop [sub nil
         s string
         c (first s)]
    (cond
      (my-is-digit c) (- (int c) (int \0)) 
      (in-trie? trie (str sub c)) (valmap (str sub c))
      (not (empty? (prefix-matches trie (str sub c)))) (recur (str sub c) (rest s) (first (rest s)))
      (not (empty? (prefix-matches trie (str c)))) (recur c (rest s) (first (rest s)))
      :else (recur nil (rest s) (first (rest s))))))

(defn match-string [prefix string trie]
  (cond
    (in-trie? trie (str prefix (first string))) (valmap (str prefix (first string)))
    (not-empty (prefix-matches trie (str prefix (first string)))) (match-string (str prefix (first string)) (rest string) trie)
    :else nil)
  )

(defn parse-line [string trie]
  (cond
    (my-is-digit (first string)) (- (int (first string)) (int \0))
    (not-empty (prefix-matches trie (str (first string)))) (if-let [res (match-string (first string) (rest string) trie)] 
                                                       res 
                                                       (parse-line (rest string) trie))
    :else (parse-line (rest string) trie)))


(defn read-file []
  (let [s (split (slurp "input.txt") #"\n")]
    (map (fn [line]
           (Integer/parseInt (str (parse-line line trie) (parse-line (reverse line) rev-trie)))) s)))

(defn rel []
  (use 'day1.part2 :reload-all))
