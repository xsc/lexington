(ns ^{ :doc "Implementation of Minimization Algorithms."
       :author "Yannick Scherer" }
  lexington.fsm.minimize
  (:use [clojure.set :as sets]
        [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.states :as s :only [reject! accept!]]
        [lexington.fsm.nfa :only [nfa->dfa]]
        [lexington.fsm.dfa :only [reverse-dfa]]
        lexington.fsm.core
        lexington.fsm.utils))

;; ## Minimization

(defmulti minimize-dfa 
  "Minimize the given DFA usign the specified `:algorithm`:
   - `:brz`: Brzozowski Minimization (default)
   - `:hopcroft`: Hopcroft Minimization
  "
  (fn [dfa & {:keys[algorithm]}] 
    algorithm)
  :default :brz)

;; ## Brzozowski Minimization
;;
;; 1. Reverse DFA to get an NFA with multiple initial states.
;; 2. Convert NFA to DFA.
;; 3. Reverse DFA again.
;; 4. Convert NFA to DFA to get minimal result.
;;
;; __Note:__ `lexington.fsm.dfa/reverse-dfa` already creates a suitable DFA, so
;; this function is nothing else than said reversal applied twice."

(defmethod minimize-dfa :brz
  [fsm & _]
  (-> fsm
    reverse-dfa
    reverse-dfa))

;; ## Hopcroft Minimization
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
                  (contains? (or (t input) (t t/any) #{s/reject!}) dest))
                transitions)))
    dest-states))

(defn- generate-next-step
  "Execution of Hopcroft's algorithm for one input character's source state set."
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
  "Create partitions of state space based on equivalency classes."
  [{:keys[transitions accept reject states]}]
  (let [alphabet (mapcat (comp keys second) transitions)]
    (loop [partitions (hash-set accept reject (set (filter (comp not (sets/union accept reject)) states)))
           indicators (hash-set accept reject)]
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
  "Based on a rename map generated from equivalency classes, clean up the
   transition table."
  [rename-map transitions]
  (reduce
    (fn [m [from tt]]
      (let [n (rename-map from)]
        (if (m n)
          m
          (->>
            (reduce
              (fn [tt [e to]]
                (assoc tt e (set (map rename-map to))))
              {} tt)
            (assoc m n)))))
    {}
    transitions))

(defmethod minimize-dfa :hopcroft
  [fsm & _]
  (let [{:keys[states accept initial transitions] :as fsm} (fsm-normalize (nfa->dfa fsm))
        partitions (create-partitions fsm)
        partition-map (zipmap partitions
                              (for [i (range)] (keyword (str "q" i))))
        rename-map (->
                     (reduce
                       (fn [m [p n]]
                         (reduce #(assoc %1 %2 n) m p))
                       {}
                       partition-map)
                     (assoc s/reject! s/reject!))
        new-transitions (rename-transitions rename-map transitions)
        minimized-fsm (-> {}
                        (assoc :states (set (map rename-map states)))
                        (assoc :accept (set (map rename-map accept)))
                        (assoc :initial (rename-map initial))
                        (assoc :transitions (rename-transitions rename-map transitions)))]
    (->
      minimized-fsm
      fsm-remove-dead-states)))
