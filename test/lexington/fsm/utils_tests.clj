(ns ^{ :doc "Tests for FSM Utils"
       :author "Yannick Scherer" }
  lexington.fsm.utils-tests
  (:use clojure.test
        lexington.fsm.utils
        lexington.fsm.core
        [lexington.fsm.consts :as c]))

(def abc-dfa
  (dfa* [:a \a :a \b :b]
        [:b \b :b \c :c]
        [:c \c :c]))

(deftest fsm-normalize-tests
  (testing "initial test"
    (is (= (:type abc-dfa) :dfa))
    (is (= (:states abc-dfa) #{:a :b :c}))
    (is (= (:initial abc-dfa) :a))
    (is (= (:reject abc-dfa) #{}))
    (is (= (:accept abc-dfa) #{}))
    (is (every? (complement #{c/reject!}) 
                (->>
                  (vals (:transitions abc-dfa))
                  (mapcat vals)
                  (apply concat)))))
  (testing "fsm-normalize"
    (let [f (fsm-normalize abc-dfa)]
      (is (= (:type f) :dfa))
      (is (= (:states f) #{:a :b :c c/reject!}))
      (is (= (:initial f) :a))
      (is (= (:reject f) #{c/reject!}))
      (is (= (:accept f) #{}))
      (is (every? #(some #{[c/any #{c/reject!}]} %)
                  (vals (:transitions f))))))
  (testing "fsm-normalize: remove reject transitions"
    (let [f (-> abc-dfa
              (reject-in :b)
              (fsm-normalize))]
      (is (= (:type f) :dfa))
      (is (= (:states f) #{:a :b :c c/reject!}))
      (is (= (:initial f) :a))
      (is (= (:reject f) #{:b c/reject!}))
      (is (= (:accept f) #{}))
      (is (= (get-in f [:transitions :b])
             { c/any #{:b} })))))

(deftest fsm-states-tests
  (testing "fsm-next-states/fsm-next-state-set"
    (are [s l] (= (fsm-next-states abc-dfa s) l)
         :a [:a :b]
         :b [:b :c]
         :c [:c])
    (are [s l] (= (fsm-next-state-set abc-dfa s) l)
         :a #{:a :b}
         :b #{:b :c}
         :c #{:c}))
  (testing "fsm-destination-states"
    (are [s i d] (= (fsm-destination-states abc-dfa s i) d)
         :a \a #{:a}
         :a \b #{:b}
         :b \b #{:b}
         :b \c #{:c}
         :c \c #{:c}))
  (testing "fsm-transition-inputs"
    (are [s d i] (= (fsm-transition-inputs abc-dfa s d) i)
         :a :a #{\a}
         :a :b #{\b}
         :b :b #{\b}
         :b :c #{\c}
         :c :c #{\c}))
  (testing "fsm-alphabet"
    (= (fsm-alphabet abc-dfa) #{\a \b \c}))
  (testing "fsm-state-seq"
    (let [sqa (fsm-state-seq abc-dfa)
          sqb (fsm-state-seq abc-dfa :b)
          sqc (fsm-state-seq abc-dfa :c)]
      (is (= (take 3 sqa) [#{:a} #{:a :b} #{:a :b :c}]))
      (is (every? #(= % #{:a :b :c}) (take 100 (drop 3 sqa))))
      (is (= (first sqb) #{:b}))
      (is (every? #(= % #{:b :c}) (take 100 (rest sqb))))
      (is (every? #(= % #{:c}) (take 100 sqc)))))
  (testing "fsm-reachable-state-seq"
    (let [sqa (fsm-reachable-state-seq abc-dfa)
          sqb (fsm-reachable-state-seq abc-dfa :b)
          sqc (fsm-reachable-state-seq abc-dfa :c)]
      (is (= sqa [:a :b :c]))
      (is (= sqb [:b :c]))
      (is (= sqc [:c]))))
  (let [f (-> abc-dfa (add-transition :d \c :e) (reject-in :e))]
    (testing "fsm-unreachable-state"
      (is (= (fsm-unreachable-states f) #{:d :e})))
    (testing "fsm-dead-states"
      (is (= (fsm-dead-states (reject-in f :c)) #{:a :b :c :d :e})))))

(deftest fsm-replace-states-tests
  (testing "fsm-replace-states"
    (let [f (fsm-replace-states abc-dfa #{:a} (constantly :q0))]
      (is (= (:initial f) :q0))
      (is (= (:states f) #{:q0 :b :c}))
      (is (= (get-in f [:transitions :q0])
             { \a #{:q0} \b #{:b} }))))
  (testing "fsm-remove-states"
    (let [f (fsm-remove-states abc-dfa #{:b :c})]
      (is (= (:states f) #{:a}))
      (is (= (:transitions f) { :a { \a #{:a} } }))))
  (testing "fsm-remove-unreachable-states/fsm-remove-dead-states"
    (let [s (-> abc-dfa
             (add-transition :d \e :e))
          u (fsm-remove-unreachable-states s)]
      (is (not (= s abc-dfa)))
      (is (= u abc-dfa)))
    (let [x (accept-in abc-dfa :c)
          s (-> x
              (add-transition :a \d :d)
              (add-transition :d \e :e)
              (reject-in :e))
          d (fsm-remove-dead-states s)]
      (is (= (get-in s [:transitions :c \d] #{:d})))
      (is (= (get-in s [:transitions :d \e] #{:e})))
      (is (= (:states s) #{:a :b :c :d :e}))
      (is (= (:reject s) #{:e}))
      (is (= (:accept s) #{:c}))
      (is (= (:states d) #{:a :b :c}))
      (is (= (:reject d) #{}))
      (is (= d x))))
  (testing "fsm-rename-states"
    (let [f (fsm-rename-states abc-dfa
                               (fn [q]
                                 (when-not (= q c/reject!)
                                   (keyword (str "q" (name q))))))]
      (is (= (:states f) #{:qa :qb :qc}))
      (is (= (:initial f) :qa))
      (are [s i d] (= (get-in f [:transitions s i]) #{d})
           :qa \a :qa
           :qa \b :qb
           :qb \b :qb
           :qb \c :qc
           :qc \c :qc))
    (let [f (fsm-rename-single-state abc-dfa :c :end)]
      (is (= (:states f) #{:a :b :end}))
      (is (get-in f [:transitions :end] { \c #{:end} })))
    (let [f (fsm-prefix-states abc-dfa "q")]
      (is (= (:states f) #{:qa :qb :qc}))
      (is (= (:initial f) :qa))
      (are [s i d] (= (get-in f [:transitions s i]) #{d})
           :qa \a :qa
           :qa \b :qb
           :qb \b :qb
           :qb \c :qc
           :qc \c :qc))
    (let [f (fsm-reindex abc-dfa)]
      (is (= (:states f) #{:q0 :q1 :q2}))
      (is (= (:initial f) :q0))
      (are [s i d] (= (get-in f [:transitions s i]) #{d})
           :q0 \a :q0
           :q0 \b :q1
           :q1 \b :q1
           :q1 \c :q2
           :q2 \c :q2))))

(deftest fsm-equality
  (testing "fsm-equal?"
    (is (fsm-equal? (fsm-prefix-states abc-dfa "x")
                    (fsm-prefix-states abc-dfa "y")))))
