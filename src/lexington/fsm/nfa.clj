(ns ^{ :doc "NFA Combination/Transformation" 
       :author "Yannick Scherer" }
  lexington.fsm.nfa
  (:use [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.states :as s :only [reject! accept!]]
        lexington.fsm.core
        lexington.fsm.utils))

;; ## Structure
;;
;; An NFA can have multiple target states for a single input, which are
;; represented as sets in the transition table. Epsilon Transitions are
;; allowed (represented by nil input)

(defn nfa-reject-state?
  "Is the default reject state?"
  [state-set]
  (= #{s/reject!} state-set))

(defn nfa-accept-state?
  "Is the default accept state?"
  [state-set]
  (= #{s/accept!} state-set))

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
  [{:keys[initial accept] :as nfa}]
  (-> nfa
    (loop-nfa)
    (assoc :accept (conj (set accept) initial))))

(defn concat-nfa
  "Create concatenation of NFAs by adding epsilon transitions from each accepting state
   to the following NFA's initial one."
  [& nfas]
  (let [nfas (map-indexed (fn [i nfa] 
                            (fsm-prefix-states nfa (str "x" i "-")))
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
                            (fsm-prefix-states nfa (str "x" (inc i) "-")))
                          nfas)
        ix :x0]
    (reduce
      (fn [nfa {:keys[initial states accept transitions] :as next-nfa}]
        (-> nfa
          (assoc :states (set (concat (:states nfa) states)))
          (assoc :accept (set (concat (:accept nfa) accept)))
          (assoc :transitions (merge (:transitions nfa) transitions))
          (nfa-add-epsilon ix initial)))
      { :initial ix }
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
  (if-not (epsilon-nfa? nfa)
    nfa
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
        (fsm-reindex
          nfa-reject-state?
          nfa-accept-state?)))))

;; ## NFA to DFA

(defn nfa->dfa
  "Convert NFA to DFA by creating 'set-states' and transitions between them."
  [fsm]
  (if-not (nfa? fsm)
    fsm
    (let [{:keys[accept reject initial transitions] :as nfa} (epsilon-nfa->nfa fsm)]
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
              (fsm-reindex 
                #(not (some (comp not #{s/reject!}) %))
                #(not (some (comp not #{s/accept!}) %))))
            (let [child-transitions (map get-transitions remaining-states)
                  child-states (filter (comp not states) (mapcat vals child-transitions))]
              (recur (set (concat states child-states))
                     (merge transitions (zipmap remaining-states child-transitions))
                     child-states))))))))
