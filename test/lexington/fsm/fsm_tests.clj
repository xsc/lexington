(ns ^{ :doc "Test of FSM Building Functionality."
       :author "Yannick Scherer" }
  lexington.fsm.fsm-tests
  (:use clojure.test
        lexington.fsm.utils
        lexington.fsm.core))

(deftest 
  fsm-struct-tests

  (testing 
    "dfa*"
    (let [f (dfa* [:a \a :a \b :b]
                  [:b \b :b \c :c]
                  [:c \c :c])]
      (is (= (:type f) :dfa))
      (let [t (:transitions f)]
        (are [state input target] (= (get-in t [state input]) #{target})
             :a \a :a
             :a \b :b
             :b \b :b
             :b \c :c
             :c \c :c))
      (is (= (:initial f) :a))
      (is (= (:states f) #{:a :b :c}))
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing 
    "nfa*"
    (let [f (nfa* [:a \a :a \b :b \a :c]
                  [:b \b :b \c :c]
                  [:c \c :c \c :a])]
      (is (= (:type f) :nfa))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a #{:a :c}
             :a \b #{:b}
             :b \b #{:b}
             :b \c #{:c}
             :c \c #{:a :c}))
      (is (= (:initial f) :a))
      (is (= (:states f) #{:a :b :c}))
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))
  
  (testing 
    "epsilon-nfa*"
    (let [f (epsilon-nfa* [:a \a :a eps :b]
                          [:b \b :b eps :c]
                          [:c \c :c])]
      (is (= (:type f) :e-nfa))
      (let [t (:transitions f)]
        (are [state input targets] (= (get-in t [state input]) targets)
             :a \a  #{:a}
             :a eps #{:b}
             :b \b  #{:b}
             :b eps #{:c}
             :c \c  #{:c}))
      (is (= (:initial f) :a))
      (is (= (:states f) #{:a :b :c}))
      (is (= (:accept f) #{}))
      (is (= (:reject f) #{}))))

  (testing
    "add-transition"
    (let [f (dfa* [:a \a :a])]
      (is (add-transition f :a \c :c))
      (is (thrown? Exception (add-transition :a \a :c))))
    (let [f (nfa* [:a \a :a])]
      (is (add-transition f :a \a :c))
      (is (thrown? Exception (add-transition :a eps :c))))
    (let [f (epsilon-nfa* [:a \a :a])]
      (is (add-transition f :a eps :c))
      (is (add-transition f :a \a :c))))

  )




