(ns findmin.core
  (:use [clojure.set :as set]
        [clojure.test])
  (:gen-class))

(set! *warn-on-reflection* true)

(defmethod print-method clojure.lang.PersistentQueue
  [q, w]
  (print-method '<- w) (print-method (seq q) w) (print-method '-< w))

(def calc-set
  (memoize (fn [k a b c r i]
             (if (zero? i)
               a
               (rem (+ (* b (calc-set k a b c r (dec i))) c)
                    r)))))

(defn calc-all-set [k a b c r]
  (for [i (range 0 k)]
    (calc-set k a b c r i)))

(defn adj-tally [m key f]
  (if (contains? m key)
    (assoc m key (f (m key)))
    (assoc m key 1)))

(defn missing-ints
  "Given a map of with integer keys return a set of missing keys up to max.
   e.g. (missing-ints {1 2, 4 10} 6) => #{0 2 3 5 6} "
  [m max]
  (apply sorted-set
         (set/difference (set (range (inc max))) (set (keys m)))))

(defn fix-zeroes [nextmin remin freqs zeroes]
  (let [zeroes (if (zeroes nextmin) (disj zeroes nextmin) zeroes)
        zeroes (if (zero? (freqs remin)) (conj zeroes remin) zeroes)]
    zeroes))

(defn adjust-tallies [nextmin remmin freqs zeroes]
  (let [freqs (-> freqs
                  (adj-tally remmin dec)
                  (adj-tally nextmin inc))
        zeroes (fix-zeroes nextmin remmin freqs zeroes)] 
    [freqs zeroes]))

(defn shift-coll [[coll freqs zeroes]]
  (let [nextmin (first zeroes)
        remmin (peek coll)
        [freqs zeroes] (adjust-tallies nextmin remmin freqs zeroes)]
    [(conj (pop coll) nextmin)
     freqs zeroes]))

(defn missing-min [coll k]
  (let [freqs (frequencies coll)]
    (iterate (fn [args] (shift-coll args))
             [(apply conj clojure.lang.PersistentQueue/EMPTY coll)
              freqs
              (missing-ints freqs (int k))])))

(defn find-min [n k a b c r]
  (let [coll (calc-all-set k a b c r) 
        n (if (> n (* 2 k))
            (dec (+ (rem n k)))
            n)
        lastcyc (ffirst (drop k (missing-min coll k)))]
    (first (drop (dec n) (cycle lastcyc)))))

(defn test-main []
  (is (= 8 (find-min 97 39 34 37 656 97))) ; => 8
  (is (= 38 (find-min 186 75 68 16 539 186)))
  (is (= 41 (find-min 137 49 48 17 461 137)))
  (is (= 40 (find-min 98 59 6 30 524 98)))
  (is (= 12 (find-min 46 18 7 11 9 46)))
  (println "Tests past"))

(defn parse-file [filename]
  (let [line-pairs (partition 2 (rest (clojure.string/split-lines
                                       (slurp filename))))]
    (doseq [[idx tup] (map-indexed vector line-pairs)]
      (let [[n k a b c r] (flatten
                           (for [line tup]
                             (map read-string (re-seq #"\d+" line))))]
        (println (str "Case #" (inc idx) ": " (find-min n k a b c r)))))))

(defn -main
  [& args]
  (if-let [filename (first args)]
    (parse-file filename)
    (test-main)))
