(ns lexington.fsm.fsm-tests
  (:use lexington.fsm.core
        lexington.fsm.fsm
        lexington.fsm.states
        lexington.fsm.transitions
        clojure.test))

(deftest fsm-structure
  (testing "Simple FSM Structure"
    (let [f (fsm 
              (state :l \l :i (eof) (accept))
              (state :i \i :s)
              (state :s \s :p)
              (state :p \p :l))
          transitions (:transitions f)]
      (is (= (:initial f) :l))
      (is (contains? (:accept f) (get-in transitions [:l (eof)])))
      (are [s] (contains? (:reject f) (get-in transitions [s (any)]))
           :l 
           :i 
           :s 
           :p)
      (are [s e n] (= (get-in transitions [s e]) n)
           :l \l :i
           :i \i :s
           :s \s :p
           :p \p :l))))
                               
