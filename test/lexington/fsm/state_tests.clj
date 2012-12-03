(ns lexington.fsm.state-tests
  (:use [lexington.fsm.states :as s]
        [lexington.fsm.transitions :as t]
        clojure.test))

(deftest simple-states

  (testing "State Transitions with literals"
    (let [t (t/resolve-transitions
              [\a :a
               \b :b
               \c :c])]
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        \c        :c
        t/any     s/reject!)))

  (testing "State Transitions with sets of literals"
    (let [t (t/resolve-transitions
              [ #{\a \b} :ab
                #{\c \d} :cd ])]
      (are [e s] (= (t e) s)
        \a        :ab
        \b        :ab
        \c        :cd
        \d        :cd
        t/any     s/reject!)))

  (testing "State Transitions with 'except'"
    (let [t (t/resolve-transitions
              [(t/except \a \b \c) :d
               (t/except \a \b)    :c
               (t/except \a)       :b])]
      (are [e s] (= (t e) s)
        \a        s/reject!
        \b        :b
        \c        :c
        t/any     :d)))

  (testing "State Transitions with 'any'"
    (let [t (t/resolve-transitions
              [\a        :a
               \b        :b
               t/any     :c])]
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        t/any     :c)))
  
  (testing "State Transitions with 'any' and 'except'"
    (let [t (t/resolve-transitions
              [(t/except \a \b) :c
               (t/except \a)    :b
               t/any            :a])]
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        t/any     :c)))

  (testing "State Transitions with 'any', 'except' and literals."
    (let [t (t/resolve-transitions
              [\x                  :x
               (t/except \x \a \b) :y
               \a                  :a
               t/any               :b])]
      (are [e s] (= (t e) s)
        \a        :a
        \b        :b
        \x        :x
        t/any     :y)))

)
