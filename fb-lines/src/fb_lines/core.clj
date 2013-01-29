(ns fb-lines.core
  (:gen-class))

(def alphabet (set "abcdefghijklmnopqrstuvwxyz"))

(defn calc-line [line]
  (let [ranks (reverse 
               (sort-by second 
                        (frequencies 
                         (filter alphabet
                                 (clojure.string/lower-case line)))))]
    (apply + (for [[[_ mult] score] (zipmap ranks (range 26 1 -1))]
               (* mult score)))))

(defn lines-from-file [filename]
  (clojure.string/split-lines (slurp filename)))

(defn -main
  [& args]
  (let [scores (map calc-line (rest (lines-from-file (first args))))]
    (doseq [[idx  score] (map-indexed vector scores)]
      (println (str "Case #" (inc idx) ": " score)))))
