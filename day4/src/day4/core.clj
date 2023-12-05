(ns day4.core
  (:require [clojure.string :refer [split-lines]]
            [clojure.set :refer [intersection]]))

(defn rel []
  "reload the repl"
  (use 'day4.core :reload-all))

(defn parse-card [card]
  (println card)
  (let [[_ numbers] (re-find #"Card.*: (.*)" card)
        [our-numbers winning-numbers] (split numbers #"\|")]
    ;; how does a map over lazy-seq no when it's done? when it returns nil?
    {:winning-numbers (map parse-long (re-seq #"\d+" winning-numbers))
     :our-numbers (map parse-long (re-seq #"\d+" our-numbers))} ))

(defn calculate-card-score
  [{:keys [winning-numbers our-numbers]}]
  (let [win-count (count (filter (set winning-numbers) our-numbers))]
    (if (pos? win-count)
      (reduce * 1 (repeat (dec win-count) 2))
      0)) )

(defn sum-wins
  [cards]
  (apply + (map calculate-card-score (map parse-card (split-lines cards)))))

(def input (slurp "input.txt"))

