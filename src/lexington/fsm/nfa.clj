(ns ^{ :doc "NFA Combination/Transformation" 
       :author "Yannick Scherer" }
  lexington.fsm.nfa
  (:use [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.states :as s :only [reject! accept!]]
        [lexington.fsm.transform :only [reindex-fsm]]
        [lexington.fsm.helpers :only [into-map remap]]))

;; ## Structure
;;
;; An NFA can have multiple target states for a single input, which are
;; represented as sets in the transition table. Epsilon Transitions are
;; allowed (represented by nil input)

(defn add-transition
  "Add Transition to NFA."
  [{:keys[transitions states initial] :as nfa} from input to]
  (let [current-transitions (or (get-in transitions [from input]) #{})]
    (-> nfa
      (assoc-in [:transitions from input]
                (if (set? current-transitions)
                  (conj current-transitions to)
                  (hash-set to current-transitions)))
      (assoc :states (conj (conj (set states) to) from))
      (assoc :initial (or initial from)))))

(defn add-epsilon-transition
  "Add Epsilon Transition to NFA."
  [nfa from to]
  (add-transition nfa from nil to))

(defn nfa-reject-state?
  [state-set]
  (not (some (comp not #(= % s/reject!)) state-set)))

(defn nfa-accept-state?
  [state-set]
  (not (some (comp not #(= % s/accept!)) state-set)))

(defn nfa*
  "Create NFA from a list of transition vectors. A transition vector
   consists of the source state and a list of input/next-state pairs."
  [& transitions]
  (reduce
    (fn [nfa [src-state & trv]]
      (let [pairs (partition 2 trv)]
        (reduce 
          (fn [nfa [input dst-state]]
            (add-transition nfa src-state input dst-state))
          nfa
          pairs)))
    {}
    transitions))
;; ## Epsilon-NFA to NFA

(defn epsilon-closure-state
  "Get epsilon closure states by following all epsilon transitions and creating a set of
   the so-reached states."
  [transitions root]
  (loop [visited #{root}
         next-states [root]]
    (if-not (seq next-states)
      visited
      (let [n (filter (comp not visited)
                      (mapcat #(get-in transitions [% nil]) next-states))]
        (recur (set (concat n visited)) n)))))

(defn epsilon-closure-transitions
  "Get epsilon closure transitions."
  [transitions alphabet closure-state-map]
  (reduce 
    (fn [tt input]
      (if-not input
        tt
        (reduce
          (fn [tt s]
            (let [src-state (closure-state-map s)
                  dst-states (set (mapcat (comp #(map closure-state-map %) 
                                                (fn [s]
                                                  (let [ts (transitions s)]
                                                    (or (ts input) (ts t/any)))))
                                          src-state))]
              (if-not (empty? dst-states)
                (assoc-in tt [src-state input] dst-states)
                tt)))
          tt
          (keys closure-state-map))))
    {}
    alphabet))

(defn epsilon-nfa->nfa
  "Resolve Epsilon Transitions by merging the epsilon targets with the epsilon source."
  [{:keys[transitions initial states accept] :as nfa}]
  (let [alphabet (mapcat keys (vals transitions))
        closure-state-map (into {} 
                            (for [s states]
                              (vector s (epsilon-closure-state transitions s))))
        closure-states (vals closure-state-map)
        closure-accept (set (filter (partial some (set accept)) closure-states))
        closure-trans (epsilon-closure-transitions transitions alphabet closure-state-map)]
    (-> {}
      (assoc :states closure-states)
      (assoc :accept closure-accept)
      (assoc :initial (closure-state-map initial))
      (assoc :transitions closure-trans)
      (reindex-fsm
        nfa-reject-state?
        nfa-accept-state?))))

;; ## NFA to DFA

(defn nfa->dfa
  "Convert NFA to DFA by creating 'set-states' and transitions between them."
  [{:keys[accept reject initial transitions] :as nfa}]
  (letfn [(next-state [current input]
            (let [current (if (vector? current) current (vector current))]
              (vec (apply sorted-set
                     (mapcat 
                       (fn [s]
                         (let [tt (transitions s)]
                           (or (tt input) (tt t/any))))
                       current)))))
          (get-transitions [current]
            (let [inputs (mapcat #(keys (transitions %)) current)]
              (reduce
                (fn [t input]
                  (assoc t input (next-state current input)))
                {}
                inputs)))]
    (loop [states      #{[initial]}
           transitions (assoc {} [initial] (transitions initial))
           remaining-states [[initial]]]
      (if-not (seq remaining-states)
        (-> {}
          (assoc :initial [initial])
          (assoc :transitions transitions)
          (assoc :states states)
          (assoc :accept (set (filter (fn [x] (some (set accept) x)) states)))
          (assoc :reject (set (filter (fn [x] 
                                        (not (some (comp not (set reject)) x))) states)))
          (reindex-fsm 
            nfa-reject-state?
            nfa-accept-state?))
        (let [child-transitions (map get-transitions remaining-states)
              child-states (filter (comp not states) (mapcat vals child-transitions))]
          (recur (set (concat states child-states))
                 (merge transitions (zipmap remaining-states child-transitions))
                 child-states))))))

(defn epsilon-nfa->dfa
  "Convert Epsilon NFA to DFA."
  [nfa]
  (-> nfa
    (epsilon-nfa->nfa)
    (nfa->dfa)))

;; ## DFA to NFA

(defn dfa->nfa
  "Convert DFA to NFA by replacing target states with sets containing the single
   target state."
  [{:keys[transitions] :as dfa}]
  (let [transitions* (-> transitions
                       (remap
                         (fn [to-table]
                           (-> to-table
                             (remap 
                               (fn [to]
                                 (if (set? to)
                                   to
                                   (hash-set to))))
                             (into-map))))
                       (into-map))]
    (assoc dfa :transitions transitions*)))
