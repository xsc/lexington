(ns ^{ :doc "NFA Combination/Transformation" 
       :author "Yannick Scherer" }
  lexington.fsm.nfa
  (:use [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.states :as s :only [reject! accept!]]
        [lexington.fsm.helpers :only [into-map remap]]
        lexington.fsm.fsm))

;; ## Structure
;;
;; An NFA can have multiple target states for a single input, which are
;; represented as sets in the transition table. Epsilon Transitions are
;; allowed (represented by nil input)

(def ^:const epsi ::epsilon)

(defn nfa-add
  "Add Transition to NFA."
  [{:keys[transitions states initial] :as nfa} from input to]
  (let [current-transitions (or (get-in transitions [from input]) #{})
        to-set (if (set? to) to (hash-set to))]
    (-> nfa
      (assoc-in [:transitions from input]
                (if (set? current-transitions)
                  (set (concat to-set current-transitions))
                  (conj to-set current-transitions)))
      (assoc :states (set (concat to-set (conj (set states) from))))
      (assoc :initial (or initial from)))))

(defn nfa-add-epsilon
  "Add Epsilon Transition to NFA."
  [nfa from to]
  (nfa-add nfa from epsi to))

(defn nfa-reject-state?
  "Is the default reject state?"
  [state-set]
  (= #{s/reject!} state-set))

(defn nfa-accept-state?
  "Is the default accept state?"
  [state-set]
  (= #{s/accept!} state-set))

(defn nfa*
  "Create NFA from a list of transition vectors. A transition vector
   consists of the source state and a list of input/next-state pairs."
  [& transitions]
  (reduce
    (fn [nfa [src-state & trv]]
      (let [pairs (partition 2 trv)]
        (reduce 
          (fn [nfa [input dst-state]]
            (nfa-add nfa src-state input dst-state))
          nfa
          pairs)))
    {}
    transitions))

;; ## NFA Combination

(defn loop-nfa
  "Create looping NFA by adding epsilon transitions from each accepting state
   to the initial one."
  [{:keys[initial accept] :as nfa}]
  (reduce 
    (fn [nfa a]
      (nfa-add-epsilon nfa a initial))
    nfa
    accept))

(defn loop0-nfa
  "Create looping NFA by adding epsilon transitions from each accepting state 
   to the initial one. Also, make the initial state accepting."
  [{:keys[initial] :as nfa}]
  (-> nfa
    (loop-nfa)
    (accept-empty)))

(defn concat-nfa
  "Create concatenation of NFAs by adding epsilon transitions from each accepting state
   to the following NFA's initial one."
  [& nfas]
  (let [nfas (map-indexed (fn [i nfa] 
                            (prefix-fsm-states nfa (str "x" i "-")))
                          nfas)]
    (reduce
      (fn [{s1 :states a1 :accept i1 :initial :as nfa} 
           {s2 :states a2 :accept i2 :initial :as next-nfa}]
        (let [cnfa (reduce
                     (fn [nfa a]
                       (nfa-add-epsilon nfa a i2))
                     nfa a1)]
          (-> cnfa
            (assoc :transitions (merge (:transitions cnfa) (:transitions next-nfa)))
            (assoc :states (set (concat s1 s2)))
            (assoc :accept a2))))
      nfas)))

(defn union-nfa
  "Create union of NFAs by adding a new initial state and epsilon transitions to the
   initial states of all given NFAs."
  [& nfas]
  (let [nfas (map-indexed (fn [i nfa] 
                            (prefix-fsm-states nfa (str "x" (inc i) "-")))
                          nfas)
        ix :x0]
    (reduce
      (fn [nfa {:keys[initial states accept transitions] :as next-nfa}]
        (-> nfa
          (assoc :states (set (concat (:states nfa) states)))
          (assoc :accept (set (concat (:accept nfa) accept)))
          (assoc :transitions (merge (:transitions nfa) transitions))
          (nfa-add-epsilon ix initial)))
      (initial-state {} ix)
      nfas)))

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
                      (mapcat #(get-in transitions [% epsi]) next-states))]
        (recur (set (concat n visited)) n)))))

(defn epsilon-closure-transitions
  "Get epsilon closure transitions."
  [transitions alphabet closure-state-map]
  (reduce 
    (fn [tt input]
      (if (= epsi input)
        tt
        (reduce
          (fn [tt s]
            (let [src-state (closure-state-map s)
                  dst-states (set (mapcat (comp #(map closure-state-map %) 
                                                (fn [s]
                                                  (if-let [ts (transitions s)]
                                                    (or (ts input) (ts t/any))
                                                    #{})))
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
  [nfa]
  (let [{:keys[accept reject initial transitions] :as nfa} (epsilon-nfa->nfa nfa)]
    (letfn [(next-state [current input]
              (let [current (if (vector? current) current (vector current))]
                (vec (apply sorted-set
                            (mapcat 
                              (fn [s]
                                (if-let [tt (transitions s)]
                                  (or (tt input) (tt t/any))
                                  #{}))
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
                   child-states)))))))

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
