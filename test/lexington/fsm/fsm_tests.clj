(ns ^{ :doc "Test of FSM Building Functionality."
       :author "Yannick Scherer" }
  lexington.fsm.fsm-tests
  (:use clojure.test
        lexington.fsm.utils
        lexington.fsm.core
        lexington.fsm.consts
        lexington.fsm.validate-types))

(deftest fsm-struct-tests

  (testing "dfa*"
    (let [f (dfa* [:a \a :a \b :b]
                  [:b \b :b \c :c]
                  [:c \c :c])]
      (is (= (:type f) :dfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input target] (= (get-in t [state input]) #{target})
             :a \a :a
             :a \b :b
             :b \b :b
             :b \c :c
             :c \c :c))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing "nfa*"
    (let [f (nfa* [:a \a :a \b :b \a :c]
                  [:b \b :b \c :c]
                  [:c \c :c \c :a])]
      (is (= (:type f) :nfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a #{:a :c}
             :a \b #{:b}
             :b \b #{:b}
             :b \c #{:c}
             :c \c #{:a :c}))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))
  
  (testing "epsilon-nfa*"
    (let [f (epsilon-nfa* [:a \a :a eps :b eps :c]
                          [:b \b :b eps :c]
                          [:c \c :c])]
      (is (= (:type f) :e-nfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a  #{:a}
             :a eps #{:b :c}
             :b \b  #{:b}
             :b eps #{:c}
             :c \c  #{:c}))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing "add-transition"
    (letfn [(test-tr [f from input to]
              (let [f (add-transition f from input to)]
                (is (contains? (:states f) from))
                (is (contains? (:states f) to))
                (is (contains? (get-in f [:transitions from input]) to))))]
      (let [f (dfa* [:a \a :a])]
        (is (thrown? Exception (add-transition :a \a :c)))
        (test-tr f :a \c :c)
        (test-tr f :b any :a))
      (let [f (nfa* [:a \a :a])]
        (is (thrown? Exception (add-transition :a eps :c)))
        (test-tr f :a \a :c)
        (test-tr f :b any :a)
        (test-tr f :a \b :b))
      (let [f (epsilon-nfa* [:a \a :a])]
        (test-tr f :a eps :b)
        (test-tr f :a eps :c)
        (test-tr f :a any :d)
        (test-tr f :a \b :b))))

  )

(deftest fsm-macro-test

  (testing "dfa macro (no special transitions)"
    (let [f (dfa
              (:state :a \a -> :a \b -> :b)
              (:state :b \b -> :b \c -> :c)
              (:state :c \c -> :c))]
      (is (= (:type f) :dfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input target] (= (get-in t [state input]) #{target})
             :a \a :a
             :a \b :b
             :b \b :b
             :b \c :c
             :c \c :c))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing "dfa macro (with 'stay in state' target)"
    (let [f (dfa
              (:state :a \a -> _ \b -> :b)
              (:state :b \b -> _ \c -> :c)
              (:state :c \c -> _))]
      (is (= (:type f) :dfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input target] (= (get-in t [state input]) #{target})
             :a \a :a
             :a \b :b
             :b \b :b
             :b \c :c
             :c \c :c))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing "dfa macro (with any transitions)"
    (let [f (dfa
              (:state :a \a -> _ _ -> :b)
              (:state :b \b -> _ _ -> :c)
              (:state :c _ -> _))]
      (is (= (:type f) :dfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input target] (= (get-in t [state input]) #{target})
             :a \a  :a
             :a any :b
             :b \b :b
             :b any :c
             :c any :c))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing "dfa macro (with accepting and rejecting states)"
    (let [f (dfa
              (:state :a \a -> :a \b -> :b)
              (:accept :b \b -> :b \c -> :c)
              (:reject :c \c -> :c))]
      (is (= (:type f) :dfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input target] (= (get-in t [state input]) #{target})
             :a \a :a
             :a \b :b
             :b \b :b
             :b \c :c
             :c any :c))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{:b}))
      (is (= (:reject f) #{:c}))))

  (testing "nfa macro (no special transitions)"
    (let [f (nfa
              (:state :a \a -> :a \b -> :b \a -> :c)
              (:state :b \b -> :b \c -> :c)
              (:state :c \c -> :c \c -> :a))]
      (is (= (:type f) :nfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a #{:a :c}
             :a \b #{:b}
             :b \b #{:b}
             :b \c #{:c}
             :c \c #{:a :c}))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing "nfa macro (with 'stay in state' target))"
    (let [f (nfa
              (:state :a \a -> _ \b -> :b \a -> :c)
              (:state :b \b -> _ \c -> :c)
              (:state :c \c -> _ \c -> :a))]
      (is (= (:type f) :nfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a #{:a :c}
             :a \b #{:b}
             :b \b #{:b}
             :b \c #{:c}
             :c \c #{:a :c}))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing "nfa macro (with any transitions))"
    (let [f (nfa
              (:state :a \a -> :a \a -> :c _ -> :b)
              (:state :b \b -> :b _ -> :c)
              (:state :c _ -> _ ))]
      (is (= (:type f) :nfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a #{:a :c}
             :a any #{:b}
             :b \b #{:b}
             :b any #{:c}
             :c any #{:c}))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing "nfa macro (with accepting and rejecting states)"
    (let [f (nfa
              (:state :a \a -> :a \b -> :b \a -> :c)
              (:accept :b \b -> :b \c -> :c)
              (:reject :c \c -> :c \c -> :a))]
      (is (= (:type f) :nfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a #{:a :c}
             :a \b #{:b}
             :b \b #{:b}
             :b \c #{:c}
             :c any #{:c}))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{:b}))
      (is (= (:reject f) #{:c}))))
  
  (testing "epsilon-nfa macro (with no special states)"
    (let [f (epsilon-nfa
              (:state :a \a -> :a eps -> :b eps -> :c)
              (:state :b \b -> :b eps -> :c)
              (:state :c \c -> :c))]
      (is (= (:type f) :e-nfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a  #{:a}
             :a eps #{:b :c}
             :b \b  #{:b}
             :b eps #{:c}
             :c \c  #{:c}))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing "epsilon-nfa macro (with 'stay in state' target)"
    (let [f (epsilon-nfa
              (:state :a \a -> _ eps -> :b eps -> :c)
              (:state :b \b -> _ eps -> :c)
              (:state :c \c -> _))]
      (is (= (:type f) :e-nfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a  #{:a}
             :a eps #{:b :c}
             :b \b  #{:b}
             :b eps #{:c}
             :c \c  #{:c}))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing "epsilon-nfa macro (with any-transitions)"
    (let [f (epsilon-nfa
              (:state :a \a -> :a eps -> :b eps -> :c _ -> :d)
              (:state :b \b -> :b eps -> :c _ -> :d)
              (:state :c \c -> :c _ -> :d))]
      (is (= (:type f) :e-nfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a  #{:a}
             :a eps #{:b :c}
             :a any #{:d}
             :b \b  #{:b}
             :b eps #{:c}
             :b any #{:d}
             :c \c  #{:c}
             :c any #{:d}))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing "epsilon-nfa macro (with accepting and rejecting states)"
    (let [f (epsilon-nfa
              (:state :a \a -> :a eps -> :b eps -> :c)
              (:accept :b \b -> :b eps -> :c)
              (:reject :c \c -> :c))]
      (is (= (:type f) :e-nfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a  #{:a}
             :a eps #{:b :c}
             :b \b  #{:b}
             :b eps #{:c}
             :c any #{:c}))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{:b}))
      (is (= (:reject f) #{:c}))))

  (testing "epsilon-nfa macro (with no special states)"
    (let [f (epsilon-nfa
              (:state :a \a -> :a eps -> :b)
              (:state :b \b -> :b eps -> :c)
              (:state :c \c -> :c))]
      (is (= (:type f) :e-nfa))
      (is (validate-fsm-type f))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a  #{:a}
             :a eps #{:b}
             :b \b  #{:b}
             :b eps #{:c}
             :c \c  #{:c}))
      (is (= (:initial f) :a))
      (are [s] (contains? (:states f) s) :a :b :c)
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  )
