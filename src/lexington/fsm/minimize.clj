(ns ^{ :doc "Implementation of Hopcroft's Algorithm."
       :author "Yannick Scherer" }
  lexington.fsm.minimize
  (:use [clojure.set :as sets]
        [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.transform :only [reindex-fsm]]))

;; ## Minimization
;;
;; P := {F, Q \ F};
;; W := {F};
;; while (W is not empty) do
;;   choose and remove a set A from W
;;   for each c in ∑ do
;;     let X be the set of states for which a transition on c leads to a state in A
;;     for each set Y in P for which X ∩ Y is nonempty do
;;       replace Y in P by the two sets X ∩ Y and Y \ X
;;       if Y is in W
;;         replace Y in W by the same two sets
;;       else
;;         if |X ∩ Y| <= |Y \ X|
;;           add X ∩ Y to W
;;         else
;;           add Y \ X to W
;;     end;
;;   end;
;; end;
;;
;; see: [Wikipedia](http://en.wikipedia.org/wiki/DFA_minimization#Hopcroft.27s_algorithm)

(defn- find-source-states
  "Find all states that (given a transition table) end up in a given state when 
   presented with a specific input."
  [transitions input dest-states]
  (mapcat
    (fn [dest]
      (map first
        (filter (fn [[from t]]
                  (= (or (t input) (t t/any)) dest))
                transitions)))
    dest-states))

(defn- generate-next-step
  [partitions indicators source-states]
  (let [src (set source-states)]
    (reduce
      (fn [[p i] px]
        (let [iset (sets/intersection px src)]
          (if (empty? iset)
            (vector (conj p px) i)
            (let [dset (sets/difference px src)]
              (vector
                (sets/union #{iset dset} p)
                (cond (i px) (sets/union #{iset dset} i)
                      (<= (count iset) (count dset)) (conj i iset)
                      :else (conj i dset)))))))
      [#{} (set indicators)]
      partitions)))

(defn- create-partitions
  [{:keys[transitions accept reject states]}]
  (let [alphabet (mapcat (comp keys second) transitions)]
    (loop [partitions (hash-set accept (set (filter (comp not accept) states)))
           indicators (hash-set accept)]
      (if-not (seq indicators)
        (filter (comp not empty?) partitions)
        (let [a (first indicators)
              rst (disj indicators a)
              [p i] (reduce 
                      (fn [[p i] input]
                        (generate-next-step p i (find-source-states transitions input a)))
                      [partitions rst]
                      alphabet)]
          (recur p i))))))

(defn- rename-transitions
  [rename-map transitions]
  (reduce
    (fn [m [from tt]]
      (let [n (rename-map from)]
        (if (m n)
          m
          (assoc m n
                 (reduce #(assoc %1 (first %2) (rename-map (second %2))) {} tt)))))
    {}
    transitions))

(defn minimize-dfa
  "Minimize DFA using Hopcroft's Algorithm."
  [{:keys[states accept reject initial transitions] :as fsm}]
  (let [partitions (create-partitions fsm)
        partition-map (zipmap partitions
                              (for [i (range)] (keyword (str "s" i))))
        rename-map (reduce
                     (fn [m [p n]]
                       (reduce #(assoc %1 %2 n) m p))
                     {}
                     partition-map)]
    (-> {}
      (assoc :states (set (map rename-map states)))
      (assoc :accept (set (map rename-map accept)))
      (assoc :reject (set (map rename-map reject)))
      (assoc :initial (rename-map initial))
      (assoc :transitions (rename-transitions rename-map transitions))
      (reindex-fsm))))
