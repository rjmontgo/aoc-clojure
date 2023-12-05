(ns day1.part1
  (:require [clojure.string :refer [split join]]))
#_
(
 *Description*
 read the lines of a file. return the sum of the calibration points
 The calibration point of a particular line is the concatenated val
                                                                  
 Ex.  asdfasdf2asdfasdf3asdfasdf is 23                             
                                                                  
 Need to ignore all other numbers except for the first and last one


 The clojure way to do this would be to construct a lazy seq of the lines
 reduce over it each time summing the first and last value of the line.
)



(defn filter-input [s]
  (map (fn [elem]
         (vec (filter (fn [char] 
                        (and (>= (int char) (int \0)) 
                             (<= (int char) (int \9))))
                      elem)))
       s))



(defn read-file []
  (let [s (split (slurp "input.txt") #"\n")]
    (reduce + 
            (map
             (fn [elem] 
               (Integer/parseInt (join [(first elem) (last elem)])))
             (filter-input s)))))




;; util for quick reload
(defn rel []
  (use 'day1.part1 :reload-all))


