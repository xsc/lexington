(ns ^{ :doc "Tests for Regular Expression Matching"
       :author "Yannick Scherer" }
  lexington.regex.match-tests
  (:use clojure.test
        lexington.regex.core))

(def hello (rx-literal "Hello"))
(def space (rx-literal " "))
(def world (rx-literal "World"))

(deftest rx-matcher-tests
  (testing "literal matcher"
    (let [m1 (rx-matcher hello)
          m2 (rx-matcher space)
          m3 (rx-matcher world)]
      (is (= (m1 "Hello World") 5))
      (is (= (m2 "           ") 1))
      (is (= (m3 "World Hello") 5))
      (is (= (m1 "World") nil))
      (is (= (m2 "not space") nil))
      (is (= (m3 "Hello") nil))))
  (testing "concat matcher"
    (let [m (rx-matcher (rx-concat hello space world))]
      (is (= (m "Hello") nil))
      (is (= (m " ") nil))
      (is (= (m "World") nil))
      (is (= (m "Hello World") 11))
      (is (= (m "Hello Worlds") 11))))
  (testing "union matcher"
    (let [m (rx-matcher (rx-union hello space world))]
      (is (= (m "not right") nil))
      (is (= (m "Hello") 5))
      (is (= (m " ") 1))
      (is (= (m "World") 5))
      (is (= (m "Hello World") 5))
      (is (= (m "Hello Worlds") 5))))
  (testing "repeated matcher"
    (testing "rx-star"
      (let [m1 (rx-matcher (rx-star hello))
            m2 (rx-matcher (rx-star space))
            m3 (rx-matcher (rx-star world))]
        (is (= (m1 "Hello World") 5))
        (is (= (m1 "HelloHoi World") 5))
        (is (= (m1 "HelloHello WorldWorld") 10))
        (is (= (m2 " ") 1))
        (is (= (m2 " a    ") 1))
        (is (= (m2 "      ") 6))
        (is (= (m3 "World Hello") 5))
        (is (= (m3 "WorldWor Hello") 5))
        (is (= (m3 "WorldWorld HelloHello") 10))
        (is (= (m1 "World") 0))
        (is (= (m2 "not space") 0))
        (is (= (m3 "Hello") 0))))
    (testing "rx-plus"
      (let [m1 (rx-matcher (rx-plus hello))
            m2 (rx-matcher (rx-plus space))
            m3 (rx-matcher (rx-plus world))]
        (is (= (m1 "Hello World") 5))
        (is (= (m1 "HelloHoi World") 5))
        (is (= (m1 "HelloHello WorldWorld") 10))
        (is (= (m2 " ") 1))
        (is (= (m2 " a    ") 1))
        (is (= (m2 "      ") 6))
        (is (= (m3 "World Hello") 5))
        (is (= (m3 "WorldWor Hello") 5))
        (is (= (m3 "WorldWorld HelloHello") 10))
        (is (= (m1 "World") nil))
        (is (= (m2 "not space") nil))
        (is (= (m3 "Hello") nil))))
    (testing "rx-question-mark"
      (let [m1 (rx-matcher (rx-question-mark hello))
            m2 (rx-matcher (rx-question-mark space))
            m3 (rx-matcher (rx-question-mark world))]
        (is (= (m1 "Hello World") 5))
        (is (= (m1 "HelloHoi World") 5))
        (is (= (m1 "HelloHello WorldWorld") 5))
        (is (= (m2 " ") 1))
        (is (= (m2 " a    ") 1))
        (is (= (m2 "      ") 1))
        (is (= (m3 "World Hello") 5))
        (is (= (m3 "WorldWor Hello") 5))
        (is (= (m3 "WorldWorld HelloHello") 5))
        (is (= (m1 "World") 0))
        (is (= (m2 "not space") 0))
        (is (= (m3 "Hello") 0)))))
  (testing "complex matcher"
    (let [m (rx-matcher (rx-concat
                          (rx-union (rx-literal "Hello") (rx-literal "Goodbye"))
                          (rx-literal " you")
                          (rx-star (rx-literal ", you"))
                          (rx-literal " and you.")))]
      (is (m "Hello you and you."))
      (is (m "Goodbye you and you."))
      (is (m "Hello you, you and you."))
      (is (m "Goodbye you, you and you."))
      (is (m "Hello you, you, you and you."))
      (is (m "Goodbye you, you, you and you."))
      (is (not (m "Jolly good!")))
      (is (not (m "Hello you.")))
      (is (not (m "Hello you, you and you!"))))))
