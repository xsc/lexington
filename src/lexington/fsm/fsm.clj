(ns ^{ :doc "Lexington FSM Generation"
       :author "Yannick Scherer" }
  lexington.fsm.fsm
  (:require [lexington.fsm.states :as s]
            [lexington.fsm.transitions :as t :only [any]]
            [lexington.fsm.helpers :as h]))

;; ## FSM Generation
;;
;; A FSM is represented as a map with the following keys:
;;
;; - `:states`      : a set of all the states available
;; - `:initial`     : the starting state
;; - `:accept`      : a set of all states that let the FSM accept the input (if applicable)
;; - `:reject`      : a set of all states that let the FSM reject the input (if applicable)
;; - `:transitions` : a nested hash map `{ :current-state { <input> :next-state } }`
;; - `:meta`        : a nested hash map `{ :key { :state <data> } ... }` for actions, ...
;;
;; Lexington takes a list of separate state maps (containing implicit references like 
;; `(continue)` or `(accept)`) and transforms it into the above representation, producing 
;; an unambiguous and inherently consistent FSM description.

(declare normalize-states
         generate-transitions)

(defn states->fsm
  "Add states to given FSM or create new FSM based on the given states. The first
   element of the list is used as the initial state if none was given so far."
  ([new-states] (states->fsm {} new-states))
  ([{:keys[initial states accept reject transitions] :as fsm} new-states]
   (let [states*       (normalize-states new-states)
         initial*      (s/state-name (first states*))
         state-set     (s/state-names states*)
         accept*       (s/state-names (filter s/accepting? states*))
         reject*       (s/state-names (filter s/rejecting? states*))
         transitions*  (generate-transitions states*)]
     (-> fsm
       (assoc :initial (or initial initial*))
       (assoc :states (set (into states state-set)))
       (assoc :accept (set (into accept accept*)))
       (assoc :reject (set (into reject reject*)))
       (assoc :meta {})
       (assoc :transitions (merge transitions transitions*))))))

(defn ->fsm
  "Add states to given FSM."
  [fsm & states]
  (states->fsm fsm states))

;; ## Transition Table

(defn- generate-transitions
  "Generate transition table from a list of states. The result will be a nested map
   `{ <state> { <input> <next state> ... } ... }`" 
  [states]
  (reduce 
    (fn [table {:keys [name transitions] :as s}]
      (assoc table name transitions))
    {}
    states))

;; ## State Normalization
;; 
;; When generated using the functions in `lexington.fsm.states`, the resulting state maps might 
;; contain references like `(continue)` or `(reject)`. Normalization resolves these references.
;;
;; - `continue` will be replaced with the current state
;; - `reject` will be replaced with the name of a newly generated reject state
;; - `accept` will be replaced with the name of a newly generated accept state
;;

(defn- generate-state-name
  "Generate name for dynamically generated states. Namespacing is used to
   prevent name clashes, but I'm not sure if this is enough."
  [base current-state input]
  (let [input-str (if (keyword? input) 
                    (name input)
                    (str input))
        n (str (name current-state) "-"  (name base) "-on-" input-str)]
      (keyword "lexington.fsm.states" n)))

(defn- generate-acceptor-name
  "Generate name for accepting state."
  [current-state input]
  (generate-state-name :accept current-state input))

(defn- generate-rejector-name
  "Generate name for rejecting state."
  [current-state input]
  (generate-state-name :reject current-state input))

;; ### Reference Resolvers

(defn- resolve-continue
  "Replace `(continue)` transitions with the state's name."
  [{:keys[name] :as s}]
  (-> s
    (h/remap-in :transitions #(if (= % (s/continue)) name %))))

(defn- resolve-acceptors
  "Replace `(accept)` transitions with the name of a newly generated accepting
   state."
  [{:keys[name] :as s}]
  (-> s
    (h/map-in :transitions
      (fn [input next-state]
        (if (s/acceptor? next-state)
          (generate-acceptor-name name input)
          next-state)))))

(defn- resolve-rejectors
  "Replace `(reject)` transitions with the name of a newly generated rejecting state."
  [{:keys[name] :as s}]
  (-> s
    (h/map-in :transitions
      (fn [input next-state]
        (if (s/rejector? next-state)
          (generate-rejector-name name input)
          next-state)))))

;; ### State Generators

(defn- generate-acceptors
  "Generate accepting states when needed."
  [{:keys[name transitions]}]
  (filter (comp not nil?)
          (map (fn [[input next-state]]
                 (when (s/acceptor? next-state)
                   (s/new-accept-state (generate-acceptor-name name input) [])))
               transitions)))

(defn- generate-rejectors
  [{:keys[name transitions]}]
  "Generate rejecting states when needed."
  (filter (comp not nil?)
          (map (fn [[input next-state]]
                 (when (s/rejector? next-state)
                   (s/new-reject-state (generate-rejector-name name input))))
               transitions)))

;; ### Normalization

(defn- normalize-single-state
  "Normalize a given state. This function produces a vector of states which shall be used in place
   of the original state."
  [{ :keys[name transitions] :as s}]
  (cons
    (-> s
      resolve-continue
      resolve-acceptors
      resolve-rejectors
      (h/into-map-in :transitions))
    (concat
      (generate-acceptors s)
      (generate-rejectors s))))

(defn- normalize-states
  "Perform normalization on a list of states."
  [states]
  (-> (mapcat normalize-single-state states)
    vec
    (conj (s/new-reject-state (s/reject)))))

;; ## FSM Minimization

(defn reachable-states
  "Get a set of all reachable states based on an initial state and a transition map."
  [root transitions]
  (loop [reached-states #{root}
         check-states [root]]
    (let [new-states (mapcat
                       (fn [s]
                         (filter 
                           (comp not reached-states)
                           (vals (transitions s))))
                       check-states)]
      (if-not (seq new-states)
        reached-states
        (recur (set (concat reached-states new-states))
               new-states)))))

(defn unreachable-states
  "Get a set of all unreachable states based on a set of all states, an initial one and
   a transition map."
  [states root transitions]
  (let [reachable (reachable-states root transitions)]
    (set (filter (comp not reachable) states))))

(defn remove-unreachable-states
  "Remove all unreachable states from an FSM."
  [{:keys[initial transitions accept reject states] :as fsm}]
  (let [reachable? (reachable-states initial transitions)]
    (-> fsm
      (assoc :accept (set (filter reachable? accept)))
      (assoc :reject (set (filter reachable? reject)))
      (assoc :states (set (filter reachable? states)))
      (assoc :transitions (reduce (fn [m [s t]]
                                    (if (reachable? s)
                                      (assoc m s t)
                                      m))
                                  {}
                                  transitions)))))

(defn minimize
  [fsm]
  (-> fsm
    remove-unreachable-states))

;; ## FSM Transformation

(defn invert
  "Create an FSM that does accept everything not accepted by the given FSM.
   This is done by setting all accepting states to rejecting ones and all other
   states to accepting ones."
  [{:keys[accept reject states] :as fsm}]
  (-> fsm
    (assoc :accept (set (filter (comp not accept) states)))
    (assoc :reject accept)))

(defn- cartesian-product-transitions
  "Create transition map for cartesian product FSM."
  [state-map t1 t2]
  (into {}
    (for [[x y :as s] (keys state-map)]
      (let [tx (t1 x)
            ty (t2 y)
            in (set (concat (keys tx) (keys ty)))]
        (when (and tx ty)
          (vector (state-map s) 
                  (into {}
                    (for [e in] 
                      (let [txe (or (tx e) (tx (t/any)))
                            tye (or (ty e) (ty (t/any)))]
                        (when (and txe tye)
                          (vector e (state-map [txe tye]))))))))))))

(defn cartesian-product
  "Build the cartesian product of two FSM's. This is done by using the following data:
- States: `(:states fsm1)` X `(:states fsm2)`
- Initial State: (`(:initial fsm1)`, `(:initial fsm2)`)
- Transitions: { [x, y] { <input> [(get-in fsm1 [:transitions x <input>]), ...] ... } ... }`
  "
  [fsm1 fsm2 accept? reject?]

  ;; New states and unique names for them
  (let [{ s1 :states i1 :initial t1 :transitions a1 :accept r1 :reject } fsm1
        { s2 :states i2 :initial t2 :transitions a2 :accept r2 :reject } fsm2]
    (let [states (for [x s1 y s2] [x y])
          state-map (zipmap states (for [i (range)] (keyword (str "state-" i))))
          initial (state-map [i1 i2])
          accept (map state-map
                      (filter (fn [[x y]]
                                (accept? x y a1 a2))
                              states))
          reject (map state-map
                      (filter (fn [[x y]]
                                (reject? x y r1 r2))
                              states))
          transitions (cartesian-product-transitions state-map t1 t2)]
      (-> {}
        (assoc :states (set (vals state-map)))
        (assoc :initial initial)
        (assoc :transitions transitions)
        (assoc :accept (set accept))
        (assoc :reject (set reject))
        minimize))))

(defn intersect
  "Create (cartesian product) intersection of two FSMs."
  [fsm1 fsm2]
  (cartesian-product 
    fsm1 fsm2
    (fn [x y a1 a2]
      (and (a1 x) (a2 y)))
    (fn [x y r1 r2]
      (and (r1 x) (r2 y)))))

(defn union
  "Create (cartesian product) union of two FSMs."
  [fsm1 fsm2]
  (cartesian-product
    fsm1 fsm2
    (fn [x y a1 a2]
      (or (a1 x) (a2 y)))
    (fn [x y r1 r2]
      (and (r1 x) (r2 y)))))

(defn difference
  "Create (cartesian product) difference of two FSMs."
  [fsm1 fsm2]
  (cartesian-product
    fsm1 fsm2
    (fn [x y a1 a2]
      (and (a1 x) (not (a2 x))))
    (fn [x y r1 r2]
      (and (r1 x) (not (r2 y))))))
