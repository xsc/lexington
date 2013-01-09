(ns ^{ :doc "Tests for Regular Expression Building."
       :author "Yannick Scherer" }
  lexington.regex.build-tests
  (:use clojure.test
        lexington.regex.core
        lexington.fsm.utils
        [lexington.fsm.consts :as c]))

(def hello (rx-literal "Hello"))
(def world (rx-literal "World"))

(deftest rx-literal-tests
  (testing "rx-literal"
    (let [{:keys[pattern nfa]} hello]
      (is (= pattern "Hello"))
      (is (= (count (:states nfa)) 6))
      (is (= (count (:accept nfa)) 1))
      (is (= (:type nfa) :dfa))
      (is (= (:reject nfa) #{}))
      (is (= (fsm-alphabet nfa) #{\H \e \l \o}))
      (is (every? #(= (count %) 1) (vals (:transitions nfa))))
      (is (every? #(= (count %) 1) (mapcat vals (vals (:transitions nfa)))))
      (let [q0 (:initial nfa)
            q1 (first (get-in nfa [:transitions q0 \H]))
            q2 (first (get-in nfa [:transitions q1 \e]))
            q3 (first (get-in nfa [:transitions q2 \l]))
            q4 (first (get-in nfa [:transitions q3 \l]))
            q5 (first (get-in nfa [:transitions q4 \o]))]
        (doseq [q [q0 q1 q2 q3 q4 q5]]
          (is (contains? (:states nfa) q)))
        (is (contains? (:accept nfa) q5))))))

(deftest rx-concat-tests
  (testing "rx-concat"
    (let [{:keys[pattern nfa]} (rx-concat hello world)]
      (is (= pattern "HelloWorld"))
      (is (= (count (:states nfa)) 12))
      (is (= (count (:accept nfa)) 1))
      (is (= (:type nfa) :e-nfa))
      (is (= (:reject nfa) #{}))
      (is (= (fsm-alphabet nfa) #{\H \e \l \o \W \r \d c/epsi}))
      (let [q0 (:initial nfa)
            q1 (first (get-in nfa [:transitions q0 \H]))
            q2 (first (get-in nfa [:transitions q1 \e]))
            q3 (first (get-in nfa [:transitions q2 \l]))
            q4 (first (get-in nfa [:transitions q3 \l]))
            q5 (first (get-in nfa [:transitions q4 \o]))
            q6 (first (get-in nfa [:transitions q5 c/epsi]))
            q7 (first (get-in nfa [:transitions q6 \W]))
            q8 (first (get-in nfa [:transitions q7 \o]))
            q9 (first (get-in nfa [:transitions q8 \r]))
            q10 (first (get-in nfa [:transitions q9 \l]))
            q11 (first (get-in nfa [:transitions q10 \d]))]
        (doseq [q [q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11]]
          (is (contains? (:states nfa) q)))
        (is (contains? (:accept nfa) q11))))))

(deftest rx-union-tests
  (testing "rx-union"
    (let [{:keys[pattern nfa]} (rx-union hello world)]
      (is (= pattern "(Hello|World)"))
      (is (= (count (:states nfa)) 13))
      (is (= (count (:accept nfa)) 2))
      (is (= (:type nfa) :e-nfa))
      (is (= (:reject nfa) #{}))
      (is (= (fsm-alphabet nfa) #{\H \e \l \o \W \r \d c/epsi}))
      (let [qi (get-in nfa [:transitions (:initial nfa) c/epsi])]
        (is (set? qi))
        (is (= (count qi) 2))
        (is (every?
              (fn [s]
                (let [i (set (keys (get-in nfa [:transitions s])))]
                  (or (= i #{\H}) (= i #{\W}))))
              qi))))))

(deftest rx-repeat-tests
  (testing "rx-plus"
    (let [{:keys[pattern nfa]} (rx-plus hello)]
      (is (= pattern "(Hello)+"))
      (is (= (count (:states nfa)) 6))
      (is (= (count (:accept nfa)) 1))
      (is (= (:type nfa) :e-nfa))
      (is (= (:reject nfa) #{}))
      (is (= (fsm-alphabet nfa) #{\H \e \l \o c/epsi}))
      (doseq [q (:accept nfa)]
        (is (= (get-in nfa [:transitions q c/epsi]) #{(:initial nfa)})))))
  (testing "rx-star"
    (let [{:keys[pattern nfa]} (rx-star hello)]
      (is (= pattern "(Hello)*"))
      (is (= (count (:states nfa)) 6))
      (is (= (count (:accept nfa)) 2))
      (is (= (:type nfa) :e-nfa))
      (is (= (:reject nfa) #{}))
      (is (= (fsm-alphabet nfa) #{\H \e \l \o c/epsi}))
      (is (contains? (:accept nfa) (:initial nfa)))
      (doseq [q (:accept nfa)]
        (when-not (= q (:initial nfa))
          (is (= (get-in nfa [:transitions q c/epsi]) #{(:initial nfa)}))))))
  (testing "rx-question-mark"
    (let [{:keys[pattern nfa]} (rx-question-mark hello)]
      (is (= pattern "(Hello)?"))
      (is (= (count (:states nfa)) 6))
      (is (= (count (:accept nfa)) 2))
      (is (= (:type nfa) :dfa))
      (is (= (:reject nfa) #{}))
      (is (= (fsm-alphabet nfa) #{\H \e \l \o}))
      (is (contains? (:accept nfa) (:initial nfa))))))

