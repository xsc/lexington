(ns lexington.fsm.fsm-tests
  (:use lexington.fsm.core
        lexington.fsm.fsm
        [lexington.fsm.states :as s]
        [lexington.fsm.transitions :as t]
        clojure.test))

(deftest fsm-structure
  (testing "Simple FSM Structure"
    (let [f (states->fsm [(s/new-state :l [ \l :i (t/eof) (s/accept) ])
                          (s/new-state :i [ \i :s ])
                          (s/new-state :s [ \s :p ])
                          (s/new-state :p [ \p :l ]) ])
          transitions (:transitions f)]
      (is (= (:initial f) :l))
      (is (contains? (:accept f) (get-in transitions [:l (t/eof)])))
      (are [s] (contains? (:reject f) (get-in transitions [s (t/any)]))
           :l 
           :i 
           :s 
           :p)
      (are [s e n] (= (get-in transitions [s e]) n)
           :l \l :i
           :i \i :s
           :s \s :p
           :p \p :l))))
                    
(deftest fsm*-macro
  (testing "Special Destination States"
    (let [f (fsm* 
              (state :init 
                \a -> _
                \b -> :reject!
                \c -> :accept!))]
      nil))
  (testing "Special Inputs"
    (let [f (fsm* 
              (state :init
                \a              -> :a
                (:not \b \c \d) -> :e
                (:or \b \c)     -> :bc
                _               -> :d))]
      nil)))

