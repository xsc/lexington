(ns lexington.fsm.state-tests
  (:use [lexington.fsm.core :as fsm]
        [lexington.fsm.states :as s]
        [lexington.fsm.transitions :as t]
        clojure.test))

(deftest simple-states

  (testing "State Transitions with literals"
    (let [s (fsm/state :k 
              \a :a
              \b :b
              \c :c)
          t (:transitions s)]
      (is (= (:name s) :k)
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        \c        :c
        (t/any) (s/reject)))))

  (testing "State Transitions with sets of literals"
    (let [s (fsm/state :k 
              #{\a \b} :ab
              #{\c \d} :cd)
          t (:transitions s)]
      (is (= (:name s) :k)
      (are [e s] (= (t e) s)
        \a        :ab
        \b        :ab
        \c        :cd
        \d        :cd
        (t/any) (s/reject)))))

  (testing "State Transitions with 'except'"
    (let [s (fsm/state :k
              (t/except \a \b \c) :d
              (t/except \a \b)    :c
              (t/except \a)       :b)
          t (:transitions s)]
      (are [e s] (= (t e) s)
        \a        (s/reject)
        \b        :b
        \c        :c
        (t/any) :d)))

  (testing "State Transitions with 'any'"
    (let [s (fsm/state :k
              \a        :a
              \b        :b
              (t/any) :c)
          t (:transitions s)]
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        (t/any) :c)))
  
  (testing "State Transitions with 'any' and 'except'"
    (let [s (fsm/state :k
              (t/except \a \b) :c
              (t/except \a)    :b
              (t/any)          :a)
          t (:transitions s)]
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        (t/any) :c)))

  (testing "State Transitions with 'any', 'except' and literals."
    (let [s (fsm/state :k
              \x                  :x
              (t/except \x \a \b) :y
              \a                  :a
              (t/any)             :b)
          t (:transitions s)]
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        \x        :x
        (t/any) :y)))

)
