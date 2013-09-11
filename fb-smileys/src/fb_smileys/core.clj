(ns fb-smileys.core
  (:use [clojure.test])
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger doseq> cf inst
                                        letfn> override-method dotimes> loop>]
             :as t])
  (:gen-class))

(ann reg java.util.regex.Pattern)
(def reg #"(\(.*?)(:\))?(:\()?(\))")

(ann clojure.string/replace [String String String -> String])
(ann rm-smileys [String -> String])
(defn rm-smileys [line]
  (-> line
    (clojure.string/replace ":)" "")
    (clojure.string/replace ":(" "")))

(ann clojure.core/neg? [AnyInteger -> Boolean])
(ann even-parens? [String -> Boolean])
(defn even-parens? [line]
  (loop> [score :- AnyInteger 0
          chars :- (U (t/Seq java.lang.Character) nil) (seq line)]
    (case (first chars)
      \( (recur (inc score) (rest chars))
      \) (if (#'neg? (dec score))
           false
           (recur (dec score) (rest chars)))
      nil (zero? score)
      (recur score (rest chars)))))

(ann  balanced? [String -> Boolean])
(defn balanced? [line]
  (if-let [matches (re-find reg line)]
    (let [m1 (first matches)
          newmatch (subs m1 1 (dec (count m1)))
          newline (clojure.string/replace-first line m1 newmatch)]
      (balanced? newline))
    (even-parens? (rm-smileys line))))

(ann ^:no-check tests [ -> Boolean])
(defn tests []
  (is (= false (balanced? ")(")))
  (is (= true (balanced? "hacker cup: started :):)" )))
  (is (= true (balanced? "i am sick today (:())" )))
  (is (= false (balanced? ":((")))
  (is (= true (balanced? "(:)"))))

(ann yesno-map (HMap Boolean String))
(def yesno-map
  {true "YES" false "NO"})

(ann ^:no-check parse-file [String -> Any])
(defn parse-file [filename]
  (doseq [[idx line] (map-indexed vector (rest (clojure.string/split-lines (slurp filename))))]
    (println (str "Case #" (inc idx) ": " (yesno-map (balanced? line))))))

(ann -main [String * -> Any])
(defn -main
  [& args]
  (if-let [filename (first args)]
    (parse-file filename)
    (tests)))
