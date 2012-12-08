(ns ^{ :doc "NFA Combination/Transformation" 
       :author "Yannick Scherer" }
  lexington.fsm.nfa
  (:use lexington.fsm.core
        lexington.fsm.utils
        [lexington.fsm.consts :as c]))

;; ## Structure
;;
;; An NFA can have multiple target states for a single input. The two
;; types of NFAs (epsilon-NFA and pure NFA) differ only in their input alphabet,
;; the former being allowed to have `lexington.fsm.consts/epsi` transitions.

;; ## NFA Combination

(defn- nfa-add-epsilon 
  [nfa from to]
  (-> nfa
    (assoc :type :e-nfa)
    (add-transition from c/epsi to)))

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
                      (mapcat #(get-in transitions [% c/epsi]) next-states))]
        (recur (set (concat n visited)) n)))))

(defn epsilon-closure-transitions
  "Get epsilon closure transitions."
  [transitions alphabet closure-state-map]
  (reduce 
    (fn [tt input]
      (if (= c/epsi input)
        tt
        (reduce
          (fn [tt s]
            (let [src-state (closure-state-map s)
                  dst-states (set (mapcat (comp #(map closure-state-map %) 
                                                (fn [s]
                                                  (if-let [ts (transitions s)]
                                                    (or (ts input) (ts c/any))
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
  (if (nfa? nfa)
    nfa
    (let [alphabet (mapcat keys (vals transitions))
          closure-state-map (into {} 
                                  (for [s states]
                                    (vector s (epsilon-closure-state transitions s))))
          closure-states (vals closure-state-map)
          closure-accept (set (filter (partial some (set accept)) closure-states))
          closure-trans (epsilon-closure-transitions transitions alphabet closure-state-map)]
      (-> {}
        (assoc :type :nfa)
        (assoc :states closure-states)
        (assoc :accept closure-accept)
        (assoc :initial (closure-state-map initial))
        (assoc :transitions closure-trans)
        (fsm-reindex)))))

;; ## NFA to DFA

(defn multi-nfa->dfa
  "Convert NFA to DFA using multiple initial states by creating 'set-states' and transitions between them."
  [{:keys[accept transitions] :as fsm} initial-states]
  (if (and (= (count initial-states) 1) (dfa? fsm))
    fsm
    (let [accepting-state? #(some (set accept) %)]
      (letfn [(next-state-set [current in]
                (->> current
                  (mapcat 
                    (fn [s]
                      (when-let [tt (transitions s)]
                        (or (tt in) (tt c/any)))))
                  set
                  hash-set))
              (get-transitions [s]
                (let [inputs (mapcat (comp keys transitions) s)]
                  (reduce #(assoc %1 %2 (next-state-set s %2)) {} inputs)))]
        (loop [states #{initial-states}
               transitions (hash-map initial-states (get-transitions initial-states))
               remaining-states [initial-states]]
          (if-not (seq remaining-states)
            (-> {}
              (assoc :type :dfa)
              (assoc :states states)
              (assoc :initial initial-states)
              (assoc :accept (set (filter accepting-state? states)))
              (assoc :transitions transitions)
              (fsm-reindex))
            (let [next-transitions (map get-transitions remaining-states)
                  next-states (->>
                                (mapcat vals next-transitions)
                                (map (comp first vec))
                                (filter (comp not states)))]
              (recur (set (concat states next-states))
                     (merge transitions (zipmap remaining-states next-transitions))
                     next-states))))))))

(defn nfa->dfa
  "Convert NFA (possibly epsilon-NFA) to DFA."
  [fsm]
  (let [{:keys[initial] :as nfa} (epsilon-nfa->nfa fsm)]
    (multi-nfa->dfa nfa (hash-set initial))))
