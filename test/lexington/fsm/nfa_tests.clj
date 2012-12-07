(ns ^{ :doc "Test of NFA Functionality."
       :author "Yannick Scherer" }
  lexington.fsm.nfa-tests
  (:use clojure.test
        lexington.fsm.nfa
        lexington.fsm.utils
        lexington.fsm.core))

(def abc-nfa
  (->
    (nfa*
      [:a \a :a \b :b]
      [:b \b :b \c :c]
      [:c \c :c])
    (accept-in :b :c)))

(deftest nfa-combination
  (testing "loop-nfa"
    (let [nfa (loop-nfa abc-nfa)]
      (is (= (:type nfa) :e-nfa))
      (is (some #{eps} (fsm-transition-inputs nfa :b :a)))
      (is (some #{eps} (fsm-transition-inputs nfa :c :a)))))
  (testing "loop0-nfa"
    (let [nfa (loop0-nfa abc-nfa)]
      (is (= (:type nfa) :e-nfa))
      (is (some #{eps} (fsm-transition-inputs nfa :b :a)))
      (is (some #{eps} (fsm-transition-inputs nfa :c :a)))
      (is (contains? (set (:accept nfa)) :a))))
)
