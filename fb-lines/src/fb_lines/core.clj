(ns fb-lines.core
  (:gen-class)
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger doseq> cf inst
                                        letfn> override-method dotimes>]
             :as t]))

(ann alphabet (t/Set Character))
(def alphabet (set "abcdefghijklmnopqrstuvwxyz"))

(ann clojure.core/sort-by [[Any -> Boolean] t/Seqable -> (t/Seqable String)])
(ann clojure.string/lower-case [String -> String])
(ann calc-line [String -> Number])
(defn calc-line [line]
  (let [ranks (reverse
               (sort-by second
                        (frequencies
                         (filter alphabet
                                 (clojure.string/lower-case line)))))
        zipranks (zipmap ranks (range 26 1 -1))
        _ (ann-form zipranks (t/Map '[Character AnyInteger] AnyInteger))]
    (apply + (for> :- Number
                   [[[_ mult] score] :- '['[Character AnyInteger] AnyInteger] zipranks]
               (* mult score)))))

(ann lines-from-file [String -> (t/Seqable String)])
(defn lines-from-file [filename]
  (clojure.string/split-lines (slurp filename)))

(ann -main ['[String] -> Any])
(defn -main
  [& args]
  (let [scores (map calc-line (rest (lines-from-file (first args))))]
    (doseq [[idx  score] (map-indexed vector scores)]
      (println (str "Case #" (inc idx) ": " score)))))
