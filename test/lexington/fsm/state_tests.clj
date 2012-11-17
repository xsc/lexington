(ns lexington.fsm.state-tests
  (:use [lexington.fsm.core :as fsm]
        clojure.test))

(deftest simple-states

  (testing "State Transitions with literals"
    (let [s (fsm/state :k 
              \a :a
              \b :b
              \c :c)
          t (fsm/state-transitions s)]
      (is (= (fsm/state-name s) :k)
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        \c        :c
        ::fsm/any ::fsm/reject))))

  (testing "State Transitions with sets of literals"
    (let [s (fsm/state :k 
              #{\a \b} :ab
              #{\c \d} :cd)
          t (fsm/state-transitions s)]
      (is (= (fsm/state-name s) :k)
      (are [e s] (= (t e) s)
        \a        :ab
        \b        :ab
        \c        :cd
        \d        :cd
        ::fsm/any ::fsm/reject))))

  (testing "State Transitions with 'except'"
    (let [s (fsm/state :k
              (fsm/except \a \b \c) :d
              (fsm/except \a \b)    :c
              (fsm/except \a)       :b)
          t (fsm/state-transitions s)]
      (are [e s] (= (t e) s)
        \a        ::fsm/reject
        \b        :b
        \c        :c
        ::fsm/any :d)))

  (testing "State Transitions with 'any'"
    (let [s (fsm/state :k
              \a        :a
              \b        :b
              (fsm/any) :c)
          t (fsm/state-transitions s)]
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        ::fsm/any :c)))
  
  (testing "State Transitions with 'any' and 'except'"
    (let [s (fsm/state :k
              (fsm/except \a \b) :c
              (fsm/except \a)    :b
              (fsm/any)          :a)
          t (fsm/state-transitions s)]
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        ::fsm/any :c)))

  (testing "State Transitions with 'any', 'except' and literals."
    (let [s (fsm/state :k
              \x                    :x
              (fsm/except \x \a \b) :y
              \a                    :a
              (fsm/any)             :b)
          t (fsm/state-transitions s)]
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        \x        :x
        ::fsm/any :y)))

)
