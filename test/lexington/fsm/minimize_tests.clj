(ns ^{ :doc "Tests for DFA Minimization"
       :author "Yannick Scherer" }
  lexington.fsm.minimize-tests
  (:use clojure.test
        lexington.fsm.core
        lexington.fsm.minimize
        [lexington.fsm.utils :only [fsm-equal?]]))

;; Accept when string contains exactly on "a" AND at 
;; least one "b"
(def-dfa D
  (:state  :q0 \b -> :q1 \a -> :q2)
  (:state  :q1 \b -> _   \a -> :q3)
  (:state  :q2 \b -> :q3 \a -> :q4)
  (:accept :q3 \b -> _   \a -> :q5)
  (:state  :q4 \b -> :q5 \a -> _)
  (:state  :q5 \b -> _   \a -> _))

(def-dfa Dmin
  (:state  :q0 \b -> :q2 \a -> :q1)
  (:state  :q1 \b -> :q3)
  (:state  :q2 \b -> :q2 \a -> :q3)
  (:accept :q3 \b -> _))

(deftest minimize-given-dfa
  (testing "Minimization of given DFA."
    (are [a] (fsm-equal? Dmin (minimize-dfa D :algorithm a))
      :hopcroft
      :brz)))

