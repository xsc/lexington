(ns lexington.fsm.fsm-tests
  (:use lexington.fsm.core
        clojure.test))

(deftest fsm-structure
  (testing "Simple FSM Structure"
    (let [fsm (states->fsm [ (state :l \l :i)
                             (state :i \i :s)
                             (state :s \s :p)
                             (state :p \p (accept)) ])]
      (is (= (:states fsm) #{:l :i :s :p}))
      (is (= (:initial fsm) :l))
      (are [s e n] (= (get-in (:transitions fsm) [s e]) n)
           :l \l :i
           :i \i :s
           :s \s :p
           :p \p (accept)))))
                               
