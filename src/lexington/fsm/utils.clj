(ns ^{ :doc "FSM Manipulation and Analysis Utilities"
       :author "Yannick Scherer" }
  lexington.fsm.utils
  (:use [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.states :as s :only [reject!]]))

;; ## FSM Transition Analysis

(defn fsm-next-states
  "Get set of states directly reachable from a given state."
  [{:keys[states transitions]} src-state]
  (set
    (when-let [state-transitions (transitions src-state)]
      (apply concat (vals state-transitions)))))

(defn fsm-destination-states
  "Get set of states that are reached in the given FSM when receiving the
   given input in the given state."
  [{:keys[states transitions]} src-state input]
  (set
    (when-let [state-transitions (transitions src-state)]
      (or (state-transitions input) (state-transitions t/any)))))

(defn fsm-transition-inputs
  "Get set of inputs that will let the FSM transition from a given source to
   a given destination state."
  [{:keys[states transitions]} src-state dest-state]
  (set
    (when-let [state-transitions (transitions src-state)]
      (reduce
        (fn [inputs [input s]]
          (if (s dest-state)
            (conj inputs input)
            inputs))
        #{}
        state-transitions))))

(defn fsm-alphabet
  "Get the (explicitly given) input alphabet of an FSM. This might differ from the actual
   alphabet, since FSMs can contain `any` transitions."
  [{:keys[transitions]}]
  (set
    (filter (comp not #(= % t/any))
            (mapcat keys (vals transitions)))))

;; ## Reachable/Unreachable/Dead States

(defn fsm-reachable-states
  "Get set of states reachable from a given root state."
  ([fsm] (fsm-reachable-states fsm (:initial fsm)))
  ([fsm root]
   (let [get-next-states (partial fsm-next-states fsm)] 
     (loop [current-states [root]
            visited-states-ordered []
            visited-states #{}]
       (if-not (seq current-states)
         visited-states-ordered
         (let [next-states (filter (comp not visited-states)
                                   (mapcat get-next-states current-states))]
           (recur next-states
                  (concat visited-states-ordered current-states)
                  (set (concat visited-states current-states)))))))))

(defn fsm-unreachable-states
  "Get set of states unreachable from a given root state."
  ([fsm] (fsm-unreachable-states fsm (:initial fsm)))
  ([{:keys[states] :as fsm} root]
   (let [reachable? (set (fsm-reachable-states fsm root))]
     (filter (comp not reachable?) states))))

(defn fsm-dead-states
  "Get set of non-accepting states that only lead to the reject state or themselves."
  [{:keys[states accept] :as fsm}]
  (let [accept? (set accept)]
    (set (filter
           (fn [s]
             (not (or (accept? s)
                      (some (comp not #{s/reject! s}) 
                            (fsm-next-states fsm s)))))
           states))))

;; ## Replace/Remove States

(defn fsm-replace-states
  "Replace all states matching a given predicate." 
  [{:keys[transitions initial] :as fsm}  p replace-fn]
  (letfn [(replace-state [x] (if (p x) (replace-fn x) x))
          (replace-set [x] (set (filter (comp not nil?) (map replace-state x))))
          (replace-sets [nfa & sets]
            (reduce 
              (fn [nfa s]
                (assoc nfa s (replace-set (s nfa))))
              nfa
              sets))]
    (let [new-transitions (reduce 
                            (fn [m [s t]]
                              (let [s (replace-state s)]
                                (if-not s
                                  m
                                  (assoc m s
                                         (reduce
                                           (fn [t [e to]]
                                             (let [to (replace-set to)]
                                               (if (empty? to)
                                                 t
                                                 (assoc t e to))))
                                           {} t)))))
                            {}
                            transitions)]
      (-> fsm
        (replace-sets :accept :reject :states)
        (assoc :initial (replace-state initial))
        (assoc :transitions new-transitions)))))

(defn fsm-remove-states
  "Remove all states matching a given predicate."
  [fsm p]
  (fsm-replace-states fsm p (constantly nil)))

(defn fsm-remove-unreachable-states
  "Remove unreachable states from FSM."
  [fsm]
  (fsm-remove-states fsm 
                     (set (fsm-unreachable-states fsm))))

(defn fsm-remove-dead-states
  "Remove dead states from FSM."
  [fsm]
  (fsm-remove-states fsm 
                     (set (fsm-dead-states fsm))))

;; ## Rename States

(defn fsm-rename-states
  "Rename states of an FSM using a rename function that processes the state name
   to change and returns either a new name or nil."
  [{:keys[transitions initial] :as fsm} rename-fn]
  (letfn [(rename [x] (or (rename-fn x) x))
          (rename-set [x] (set (map rename x)))
          (rename-sets [nfa & sets]
            (reduce 
              (fn [nfa s]
                (assoc nfa s (rename-set (s nfa))))
              nfa
              sets))]
    (let [new-transitions (reduce 
                            (fn [m [s t]]
                              (assoc m (rename s)
                                     (reduce
                                       (fn [t [e to]]
                                         (assoc t e (rename-set to)))
                                       {} t)))
                            {}
                            transitions)]
      (-> fsm
        (rename-sets :accept :reject :states)
        (assoc :initial (rename initial))
        (assoc :transitions new-transitions)))))

(defn fsm-prefix-states
  "Prefix all states of an FSM with a given string."
  [fsm prefix]
  (fsm-rename-states fsm
    (fn [x]
      (when-not (or (= x s/accept!) (= x s/reject!))
        (keyword (str prefix (name x)))))))

(defn fsm-reindex
  "Rename states to `state-x` where `state-0` is the initial state."
  ([fsm] (fsm-reindex fsm #(= % #{s/reject!}) #(= % #{s/accept!})))
  ([{:keys[accept states transitions initial] :as fsm} reject-state? accept-state?]
   (let [state-map (zipmap 
                     (filter (comp not reject-state?) (fsm-reachable-states fsm initial))
                     (for [i (range)] (keyword (str "state-" i))))]
     (fsm-rename-states fsm (fn [s]
                              (cond (reject-state? s) s/reject!
                                    (accept-state? s) s/accept!
                                    :else (state-map s)))))))
