(ns fb-smileys.core
  (:use [clojure.test])
  (:gen-class))

(def reg #"(\(.*?)(:\))?(:\()?(\))")

(defn rm-smileys [line]
  (-> line
    (clojure.string/replace ":)" "")
    (clojure.string/replace ":(" "")))

(defn even-parens? [line]
  (loop [score 0 chars (seq line)]
    (case (first chars)
      \( (recur (inc score) (rest chars))
      \) (if (neg? (dec score))
           false
           (recur (dec score) (rest chars)))
      nil (zero? score)
      (recur score (rest chars)))))

(defn balanced? [line]
  (if-let [matches (re-find reg line)]
    (let [m1 (first matches)
          newmatch (subs m1 1 (dec (count m1)))
          newline (clojure.string/replace-first line m1 newmatch)]
      (balanced? newline))
    (even-parens? (rm-smileys line))))

(defn tests []
  (is (= false (balanced? ")(")))
  (is (= true (balanced? "hacker cup: started :):)" )))
  (is (= true (balanced? "i am sick today (:())" )))
  (is (= false (balanced? ":((")))
  (is (= true (balanced? "(:)"))))

(def yesno-map
  {true "YES" false "NO"})

(defn parse-file [filename]
  (doseq [[idx line] (map-indexed vector (rest (clojure.string/split-lines (slurp filename))))]
    (println (str "Case #" (inc idx) ": " (yesno-map (balanced? line))))))

(defn -main
  [& args]
  (if-let [filename (first args)]
    (parse-file filename)
    (tests)))
