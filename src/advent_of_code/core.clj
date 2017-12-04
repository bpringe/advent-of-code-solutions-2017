(ns advent-of-code.core
  (:require [clojure.java.io :as io]])
  (:gen-class))

;;=====================================
;;============ Problem 1 ==============
;;=====================================

(defn string->number-vector
  [string re-delimiter]
  (into []
    (map 
      #(Integer/parseInt %) 
      (clojure.string/split string re-delimiter))))

(defn get-solution
  [incrementer coll]
  (let [coll-count (count coll)]
    (map 
      (fn [index]
        (let [current (nth coll index)
              comparee (nth coll (mod (+ index incrementer) coll-count))]
          (if (= current comparee) current 0)))
      (range coll-count))))

;;=====================================
;;============ Problem 2 ==============
;;=====================================

;; get lines from file by file name
;; must have :resource-paths ["resources"] set in project.clj and input file in resource folder

(defn get-lines
  [file-name]
  (with-open [rdr (io/reader (io/resource file-name))]
    (doall (line-seq rdr))))

(defn get-difference-min-max
  [nums]
  (- (apply max nums) (apply min nums)))
    
(defn get-solution-problem-2-part-1
  []
  (reduce
  	(fn [sum line]
      (+ sum 
      	(get-difference-min-max (string->number-vector line #" +"))))
  	0
    (get-lines "problem_2_input")))
    
(defn get-quotient
  [row]
  (let [sorted-row (sort > row)]
    (loop [j 0
           k 1]
      (if (= 0 (mod (nth sorted-row j) (nth sorted-row k)))
        (/ (nth sorted-row j) (nth sorted-row k))
        (if (not= k (dec (count sorted-row)))
          (recur j (inc k))
          (recur (inc j) (+ j 2)))))))

(defn get-solution-problem-2-part-2
  []
  (->> (get-lines "problem_2_input")
       (map (fn [line]
          (string->number-vector #"\t" line)))
       (map get-quotient)
       (reduce +)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, world!")

