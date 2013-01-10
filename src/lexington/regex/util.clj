(ns ^{ :doc "Regular Expression Helpers"
       :author "Yannick Scherer" }
  lexington.regex.util)

;; ## Escape / Unescape 

(def rx-escape-char? (set "[]()+*?^$\\\""))

(defn rx-escape
  "Escape special Regular Expression characters in a String."
  [s]
  (->> s
    (mapcat
      (fn [c]
        (if (rx-escape-char? c)
          [\\ c]
          [c])))
    (apply str)))

(defn rx-unescape
  "Unescape string produced by applying `str` to a java.util.regex.Pattern."
  [s]
  (->> s
    (partition 2 1 [nil])
    (mapcat
      (fn [[a b]]
        (when-not (and (= a \\) (rx-escape-char? b))
          [a])))
    (apply str)))

;; ## Others

(defn rx-state-name-seq
  "Create lazy seq of state names `:q0`, `:q1`, ... using the given range
   (specified exactly as if using `range`)."
  [& range-args]
  (map
    (fn [i]
      (keyword (str "q" i)))
    (apply range range-args)))

(defn symbol=
  "Compare two symbols/strings."
  [a b]
  (let [sa (if (symbol? a) (name a) (str a))
        sb (if (symbol? b) (name b) (str b))]
    (= sa sb)))

