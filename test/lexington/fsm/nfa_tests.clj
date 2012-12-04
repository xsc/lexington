(ns ^{ :doc "Test of NFA Functionality."
       :author "Yannick Scherer" }
  lexington.fsm.nfa-tests
  (:use clojure.test
        lexington.fsm.nfa
        lexington.fsm.utils
        [lexington.fsm.core :only [accept-in accept-empty]]))

(deftest nfa-generation
  (testing "nfa-add"
    (let [nfa (-> {}
               (nfa-add :a 0 :b)
               (nfa-add :b 1 :a)
               (nfa-add :a 0 :a))]
      (are [s e n] (= (get-in nfa [:transitions s e]) n)
           :a 0 #{:a :b}
           :b 1 #{:a})))
  (testing "nfa-add-epsilon"
    (let [nfa (-> {}
                (nfa-add :a 0 :b)
                (nfa-add-epsilon :b :a))]
      (are [s e n] (= (get-in nfa [:transitions s e]) n)
           :a 0    #{:b}
           :b epsi #{:a})))
  (testing "nfa*"
    (let [nfa (nfa*
                [:a 0 :b 0 :a]
                [:b 1 #{:a :b} epsi :c]
                [:a 0 :c])]
      (are [s e n] (= (get-in nfa [:transitions s e]) n)
           :a 0    #{:a :b :c}
           :b 1    #{:a :b}
           :b epsi #{:c})))
)

(def abc-nfa
  (->
    (nfa*
      [:a \a :a epsi :b]
      [:b \b :b epsi :c]
      [:c \c :c])
    (accept-in :b :c)))

(deftest nfa-combination
  (testing "loop-nfa"
    (let [nfa (loop-nfa abc-nfa)]
      (is (some #{epsi} (fsm-transition-inputs nfa :b :a)))
      (is (some #{epsi} (fsm-transition-inputs nfa :c :a)))))
  (testing "loop0-nfa"
    (let [nfa (loop0-nfa abc-nfa)]
      (is (some #{epsi} (fsm-transition-inputs nfa :b :a)))
      (is (some #{epsi} (fsm-transition-inputs nfa :c :a)))
      (is (contains? (set (:accept nfa)) :a))))
)
