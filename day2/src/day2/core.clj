(ns day2.core
  (:require [clojure.string :refer [split]]))

(def MAX_BLUE 14)
(def MAX_GREEN 13)
(def MAX_RED 12)

(defn read-file []
  (map (fn [game-str]
         ; take the game string and remove the game number
         ; then split each game into strings of blue green and red by ;
          (split game-str #": "))
       (split (slurp "input.txt") #"\n")))

(defn map-game-counts [game-result]
  (map (fn [game-res]
         (reduce
          (fn [output elem]
           (let [num-word-pair (split elem #" ")
                 num (Integer/parseInt (first num-word-pair))
                 keyw (keyword (second num-word-pair))]
             (assoc output keyw num)))
          {}
          (split game-res #", ")))
       game-result))

(defn map-games [game-list]
      (map (fn
            [game-pair]
            (let [game-id
                  (reverse (take-while
                            (fn [char]
                                (Character/isDigit char))
                            (reverse (first game-pair))))
                  games (split (second game-pair) #"; ")]
              [(Integer/parseInt (apply str game-id))
               (map-game-counts games)])) game-list))


(defn parse-games-1 []
      "Parses a game string and returns the sum of game ids that
       are less than the max defined at the top"
  (reduce (fn [output game]
            (let [game-id (first game)
                  game-results (second game)]
              (if (every? (fn [game]
                            (and (or (nil? (game :red)) (<= (game :red) MAX_RED))
                                 (or (nil? (game :blue)) (<= (game :blue) MAX_BLUE))
                                 (or (nil? (game :green)) (<= (game :green) MAX_GREEN))))
                          game-results)
                (+ output game-id)
                output)))
          0
          (map-games (read-file))))

(defn parse-games-2 []
  "Parses a game and determines the minimum amount needed and multiplies them
   together to form a sum"
  (reduce (fn [output game]
            (let [game-results (second game)
                  game-map (reduce (fn [output game]
                                     (assoc output
                                            :green (max (output :green) (game :green 0))
                                            :red (max (output :red) (game :red 0))
                                            :blue (max (output :blue) (game :blue 0))))
                                   {:red 0 :green 0 :blue 0}
                                   game-results)]
              (+ output (* (game-map :red) (game-map :blue) (game-map :green)))))
          0
          (map-games (read-file))))

(defn rel []
      (use 'day2.core :reload-all))
