(ns fb-smileys.core
  (:require [clojure.core.typed :as t :refer [ann inst AnyInteger]]
            [clojure.test :refer :all])
  (:gen-class))

(ann rm-smileys [String -> String])
(defn rm-smileys [line]
  (-> line
    (clojure.string/replace ":)" "")
    (clojure.string/replace ":(" "")))

(ann even-parens? [String -> Boolean])
(defn even-parens? [line]
  (t/loop [score :- AnyInteger 0
           chars :- (t/Option (t/Seq java.lang.Character)) (seq line)]
    (case (first chars)
      \( (recur (inc score) (rest chars))
      \) (if (#'neg? (dec score))
           false
           (recur (dec score) (rest chars)))
      nil (zero? score)
      (recur score (rest chars)))))

(ann  balanced? [String -> Boolean])
(defn balanced? [line]
  (if-let [matches (re-find  #"(\(.*?)(:\))?(:\()?(\))" line)]
    (let [m1 (str (first matches)) ;; ensure countable, not char
          newmatch (subs m1 1 (dec (count m1)))
          newline (clojure.string/replace-first line m1 newmatch)]
      (balanced? newline))
    (even-parens? (rm-smileys line))))

(ann yesno-map (t/Map Boolean String))
(def yesno-map
  {true "YES" false "NO"})

(t/ann clojure.string/split-lines [String -> (t/Vec String)])
(t/ann clojure.core/slurp [String -> String])
(t/ann parse-file [String -> (t/Seq Boolean)])
(defn parse-file [filename]
  (let [text (slurp filename)]
    (t/for [line :- String,
            (rest (clojure.string/split-lines text))]
      :- Boolean
      (balanced? line))))

(t/ann  results-to-str [(t/Seqable Boolean) -> (t/Seq t/Str)])
(defn results-to-str [results]
  (t/for [[idx line] :- '[AnyInteger Boolean]
          (map-indexed (inst vector t/Any AnyInteger Boolean) results)]
    :- String
    (str "Case #" (inc idx) ":" (yesno-map line))))

(ann ^:no-check tests [ -> t/Any])
(defn tests []
  (is (= false (balanced? ")(")))
  (is (= true (balanced? "hacker cup: started :):)" )))
  (is (= true (balanced? "i am sick today (:())" )))
  (is (= false (balanced? ":((")))
  (is (= true (balanced? "(:)"))))

(ann -main [String * -> t/Any])
(defn -main
  [& args]
  (if-let [filename (first args)]
    (println (clojure.string/join
              (interleave (results-to-str (parse-file filename))
                          (repeat "\n"))))
    (tests)))
