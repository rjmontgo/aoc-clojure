(ns day3.core
  (:require [clojure.string :refer [split]]))

(defn rel []
  (use 'day3.core :reload-all))

(defn is-symbol [char]
  (and (not= \. char) (not (Character/isDigit char))))

(def input (slurp "input.txt"))
(def test-input
  "......
123*21
*2....")

;; split each char and join them on Character/isDigit
(def mat
     (vec (map (fn [row] (split row #"")) (split input #"\n"))))

(def test-mat
  (vec (map (fn [row] (split row #"")) (split test-input #"\n"))))

(def ROWS (count mat))
(def COLS (count (first mat)))

;; I need to find each digit in the list
;; when I find a digit check if any surrounding index is a symbol
;; if it is then we want to capture the entire number [from left to right]
;; and continue iterating after the end of the number

;; return the interval of number indices
;; or get the index of all the symbols and check around each of those
;; or keep track of the number as you loop through it and if you find
;; a symbol you know to add the whole number to the list of numbers

;; conditions
;; if it is a '.' or a symbol, ignore it
;; if it is a digit then we can glob the digit

(defn adj-symbol [row col mat]
  "Returns true if there is a symbol adjacent to the row or col"
    (some identity (let [directions [[1 0] [1 1] [0 1] [-1 0] [-1 -1] [0 -1] [1 -1] [-1 1]]
                      ROWS (count mat)
                      COLS (count (first mat))]
       (map (fn [dir]
                (let [irow (+ row (first dir))
                           icol (+ col (second dir))]
                  (and (< -1 irow ROWS)
                       (< -1 icol COLS)
                       (is-symbol (nth (mat irow) icol)))
                  ))
            directions))))

(defn find-nums [s row mat]
  (let [length (count s)]
    (loop [idx 0
           number []
           add-number false
           number-list []]
      (if-not (< idx length)
        (do
          (if (and (not (empty? number)) add-number)
            (conj number-list (Integer/parseInt (apply str number)))
            number-list))
        (let [char (nth s idx)]
          (cond
            (Character/isDigit char) (if (adj-symbol row idx mat)
                                       (recur (+ idx 1) (conj number char) true number-list)
                                       (recur (+ idx 1) (conj number char) add-number number-list))
            (not-empty number)
            (if add-number
              (recur (+ idx 1) [] false (conj number-list (Integer/parseInt (apply str number))))
              (recur (+ idx 1) [] false number-list))
            :else (recur (+ idx 1) number false number-list)))))))

(defn part1 [mat]
  (reduce (fn [output elem]
            (+ output (reduce + elem)))
          0
          (map-indexed (fn [idx item]
                         (find-nums item idx mat)) mat)))



;; part2: find all the *'s and check adjacent to them
;; if there are 2 numbers adjacent then we know we've found a gear ratio

;; the problem is we may land in the middle of a number, so we need to know if
;; we've visited the same number twice
;; overlap function -> if the row is different, ignore. if col1high < col2low /
;; if the numbers are the same then the start/end will be the same for every captured number
;; so when looking at adjacent numbers we need to store [num row cstart]

(defn get-*-coords [mat]
      (set (reduce concat #{} (map-indexed
                               (fn [row-idx row]
                                   (reduce conj #{} (keep-indexed
                                                     (fn [index col]
                                                         (when (= col "*")
                                                           [row-idx index]))
                                                     row)
                                           ))
                               mat))))

(defn overlap--unused [range1 range2]
  "[row col-start col-end]"
  (let [r1 (first range1)
        r2 (first range2)
        c1s (second range1)
        c2s (second range2)
        c1e (nth range1 2)
        c2e (nth range2 2)]
    (and (= r1 r2) (or (< c1s c2s c1e) (< c2s c1s c2e)))))

(defn idx-is-digit [row col mat]
  (let [ROWS (count mat)
        COLS (count (first mat))]
    (and (< -1 row ROWS)
         (< -1 col COLS)
         (Character/isDigit (nth ((mat row) col) 0)))))

(defn glob-number [row col mat]
  (let [start (loop [cur col]
                (if (idx-is-digit row (- cur 1) mat) (recur (- cur 1))
                  cur))]
    (loop [cur start
           number [((mat row) start)]]
      (if (idx-is-digit row (+ cur 1) mat) (recur (+ cur 1) (conj number ((mat row) (+ cur 1))))
        [(Integer/parseInt (apply str number)) start]))))

(defn get-adj-nums [row col mat]
  "takes a coordinate and returns all adjacent numbers"
  (let [directions [[1 0] [1 1] [0 1] [-1 0] [-1 -1] [0 -1] [1 -1] [-1 1]]
        ROWS (count mat)
        COLS (count (first mat))]
    (set (keep (fn [dir]
               (let [r (+ (first dir) row)
                     c (+ (second dir) col)]
                 (if (idx-is-digit r c mat)
                   (glob-number r c mat)
                   nil)))
           directions))))

(defn get-numsets [coords mat]
  (keep (fn [numset] (when (= 2 (count numset)) numset))
        (map (fn [coord]
               (let [row (first coord)
                     col (second coord)]
                 (get-adj-nums row col mat)))
             coords)))

(defn part2 [mat]
      (let [coords (get-*-coords mat)]
        (reduce (fn [out numset]
                  (let [r1 (first (first numset))
                        r2 (first (second numset))]
                    (+ out (* r1 r2)))) 0 (get-numsets coords mat))))
