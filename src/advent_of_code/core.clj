(ns advent-of-code.core
  (:gen-class))

(defn string->number-list
  [string]
  (map #(Integer/parseInt %) (clojure.string/split string #"")))

(defn get-solution
  [incrementer coll]
  (let [coll-count (count coll)]
    (map 
      (fn [index]
        (let [current (nth coll index)
              comparee (nth coll (mod (+ index incrementer) coll-count))]
          (if (= current comparee) current 0)))
      (range coll-count))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (string->number-list "123")))


