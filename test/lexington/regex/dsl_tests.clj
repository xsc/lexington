(ns ^{ :doc "Tests for Lexington's Regular Expression DSL."
       :author "Yannick Scherer" }
  lexington.regex.dsl-tests
  (:use clojure.test
        lexington.regex.core))

(deftest rx-expand-simple-tests
  (testing "rx-expand: literals"
    (are [e] (= (rx-expand e) e)
         \a :a 0 "hello"))
  (testing "rx-expand: choice"
    (are [e] (= (rx-expand e) `(rx-choice ~e))
         [\a \b \c]
         [:a :b :c]
         [0 1 2]))
  (testing "rx-expand: inverse choice"
    (are [e] (= (rx-expand e) `(rx-not ~(vec (rest e))))
         ['not \a \b \c]
         ['not :a :b :c]
         ['not 0 1 2]))
  (testing "rx-expand: sequence"
    (are [e] (= (rx-expand e) `(rx-concat ~@e))
         '(\a \b \c)
         '(:a :b :c)
         '(0 1 2)
         '("hello" "world")))
  (testing "rx-expand: union"
    (are [e] (= (rx-expand e) (->> e
                                (filter (complement #(= % '|)))
                                (reverse)
                                (reduce 
                                  (fn [r x]
                                    `(rx-union ~x ~r)))))
         '(\a | \b)
         '(\a | \b | \c)
         '(:a | :b)
         '(0  | 1  | 2)
         '("hello" | "world")))
  (testing "rx-expand: star"
    (are [e] (= (rx-expand e) `(rx-star ~(first e)))
         '(\a *)
         '(:a *)
         '(0 *)
         '("hello" *)))
  (testing "rx-expand: plus"
    (are [e] (= (rx-expand e) `(rx-plus ~(first e)))
         '(\a +)
         '(:a +)
         '(0 +)
         '("hello" +)))
  (testing "rx-expand: question mark"
    (are [e] (= (rx-expand e) `(rx-question-mark ~(first e)))
         '(\a ?)
         '(:a ?)
         '(0 ?)
         '("hello" ?))))

(deftest rx-expand-complex-tests
  (testing "rx-expand: combinations"
    (are [e r] (= (rx-expand e) r)
         '((\a | \b) *) 
         `(rx-star (rx-union \a \b))

         '((\a | \b) * \c * \d) 
         `(rx-concat
                                   (rx-star (rx-union \a \b))
                                   (rx-star \c)
                                   \d)

         '([\a \b] + [not \a \b] *) 
         `(rx-concat
                                       (rx-plus (rx-choice [\a \b]))
                                       (rx-star (rx-not [\a \b])))

         '("either: " ([0 1] + | [\a \b] +))
         `(rx-concat 
            "either: "
            (rx-union
              (rx-plus (rx-choice [0 1]))
              (rx-plus (rx-choice [\a \b])))))))
                           
         

