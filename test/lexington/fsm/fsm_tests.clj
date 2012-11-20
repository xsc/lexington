(ns lexington.fsm.fsm-tests
  (:use lexington.fsm.core
        lexington.fsm.fsm
        lexington.fsm.states
        lexington.fsm.transitions
        clojure.test))

(deftest fsm-structure
  (testing "Simple FSM Structure"
    (let [fsm (states->fsm [ (state :l \l :i)
                             (state :i \i :s)
                             (state :s \s :p)
                             (state :p 
                                \p    :l
                                (eof) (accept)) ])]
      (is (= (:initial fsm) :l))
      (are [s e n] (= (get-in (:transitions fsm) [s e]) n)
           :l \l :i
           :i \i :s
           :s \s :p
           :p \p :l))))
                               
