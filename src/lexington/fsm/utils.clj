(ns ^{ :doc "FSM Manipulation and Analysis Utilities"
       :author "Yannick Scherer" }
  lexington.fsm.utils
  (:use [lexington.fsm.consts :as c]))

;; ## FSM Normalization

(def fsm-normalize
  "Normalize an FSM:
   - remove all transitions from rejecting states (leave only to itself),
   - add default rejecting state,
   - add any-transitions to default rejecting state where no any is given,
   - ...
  "
  (letfn [(remove-reject-state-transitions [transitions reject?]
            (reduce
              (fn [table [state m]]
                (if (reject? state)
                  (assoc-in table [state c/any] #{state})
                  (assoc table state m)))
              {}
              transitions))
          (add-any-to-reject [table state]
            (assoc-in table [state c/any] #{c/reject!}))
          (add-any-to-reject-transitions [transitions states]
            (reduce
              (fn [table state]
                (if-let [tt (transitions state)]
                  (if-not (tt c/any)
                    (->
                      (assoc table state tt)
                      (add-any-to-reject state))
                    (assoc table state tt))
                  (add-any-to-reject table state)))
              {}
              states))]
    (fn [{:keys[states transitions accept reject] :as fsm}]
      (let [normalized-states (conj (set states) c/reject!)
            normalized-transitions (-> transitions
                                     (remove-reject-state-transitions (set reject))
                                     (add-any-to-reject-transitions normalized-states))]
        (-> fsm
          (assoc :states normalized-states)
          (assoc :reject (conj (set reject) c/reject!))
          (assoc :accept (disj (set accept) c/reject!))
          (assoc :transitions normalized-transitions))))))

;; ## FSM Transition Analysis

(defn fsm-next-states
  "Get vector of states directly reachable from a given state (ordered by input entity). "
  [{:keys[states transitions reject]} src-state]
  (distinct
    (when-not (contains? reject src-state)
      (when-let [state-transitions (transitions src-state)]
        (letfn [(c [s1 s2]
                  (cond (coll? s1) (recur (first s1) s2)
                        (coll? s2) (recur s1 (first s2))
                        (or (= s1 c/any) (= s1 c/epsi)) 1
                        (or (= s2 c/any) (= s2 c/epsi)) -1
                        :else (try
                                (compare s1 s2)
                                (catch Exception e)
                                (finally 0))))]
          (mapcat 
            (comp #(sort c %) second) 
            (sort-by first c state-transitions)))))))

(defn fsm-destination-states
  "Get set of states that are reached in the given FSM when receiving the
   given input in the given state."
  [{:keys[states transitions reject]} src-state input]
  (set
    (when-not (contains? reject src-state)
      (when-let [state-transitions (transitions src-state)]
        (or (state-transitions input) (state-transitions c/any) #{c/reject!})))))

(defn fsm-transition-inputs
  "Get set of inputs that will let the FSM transition from a given source to
   a given destination state."
  [{:keys[states transitions reject]} src-state dest-state]
  (set
    (when-not (contains? reject src-state)
      (when-let [state-transitions (transitions src-state)]
        (reduce
          (fn [inputs [input s]]
            (if (s dest-state)
              (conj inputs input)
              inputs))
          #{}
          state-transitions)))))

(defn fsm-alphabet
  "Get the (explicitly given) input alphabet of an FSM. This might differ from the actual
   alphabet, since FSMs can contain `any` transitions."
  [{:keys[transitions]}]
  (set
    (filter (comp not #(= % c/any))
            (mapcat keys (vals transitions)))))

;; ## Reachable/Unreachable/Dead States

(defn fsm-state-seq
  "Get possibly infinite seq of state sets starting from a given root state. Each
   set contains all the states reachable from any of the previous set's states."
  ([fsm] (fsm-state-seq fsm (:initial fsm)))
  ([{:keys[reject] :as fsm} root]
   (let [get-next-states (partial fsm-next-states fsm)]
     (letfn [(lazy-bfs [current-states]
               (lazy-seq
                 (when (seq current-states)
                   (let [next-states (mapcat get-next-states current-states)]
                     (cons current-states (lazy-bfs (set next-states)))))))]
       (lazy-bfs #{root})))))

(defn fsm-reachable-state-seq
  "Get seq of states reachable from a given root state. (breadth-frist-search)"
  ([fsm] (fsm-reachable-state-seq fsm (:initial fsm)))
  ([fsm root]
   (letfn [(lazy-reachable [state-seq visited-states]
             (lazy-seq
               (let [[current-states & next-seq] state-seq
                     current-states (filter (comp not visited-states) current-states)]
                 (when (seq current-states)
                   (concat 
                     current-states
                     (lazy-reachable next-seq (set (concat visited-states current-states))))))))]
     (lazy-reachable
       (fsm-state-seq fsm root)
       #{}))))

(defn fsm-reachable-states
  "Get set of states reachable from a given root state."
  ([fsm] (fsm-reachable-states fsm (:initial fsm)))
  ([fsm root] (set (fsm-reachable-state-seq fsm root))))

(defn fsm-unreachable-states
  "Get set of states unreachable from a given root state."
  ([fsm] (fsm-unreachable-states fsm (:initial fsm)))
  ([{:keys[states] :as fsm} root]
   (let [reachable? (set (fsm-reachable-states fsm root))]
     (filter (comp not reachable?) states))))

(defn fsm-dead-states
  "Get set of non-accepting states that only lead to other non-accepting states."
  [{:keys[states accept] :as fsm}]
  (let [accept? (set accept)
        dead-state? (fn [s]
                      (not 
                        (or (accept? s)
                            (some accept? (fsm-reachable-states fsm s)))))]
    (set (filter dead-state? states))))

;; ## Replace/Remove States

(defn fsm-replace-states
  "Replace all states matching a given predicate." 
  [{:keys[transitions initial] :as fsm} p replace-fn]
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
  "Remove dead states from FSM. Note that this will also remove unreachable
   states from the FSM!"
  [{:keys[reject] :as fsm}]
  (-> fsm
    (assoc :reject (set (concat reject (fsm-dead-states fsm))))
    fsm-normalize
    (fsm-remove-unreachable-states)))

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

(defn fsm-rename-single-state
  "Rename an FSM's state."
  [fsm rename-what rename-to]
  (fsm-rename-states
    fsm
    (fn [s]
      (when (= s rename-what)
        rename-to))))

(defn fsm-prefix-states
  "Prefix all states of an FSM with a given string."
  [fsm prefix]
  (fsm-rename-states fsm
    (fn [x]
      (when-not (or (= x c/accept!) (= x c/reject!))
        (keyword (str prefix (name x)))))))

(def fsm-reindex
  "Reindex states using a given state-name/index-based rename function. It 
   is also possible to specify predicates for which states to replace by the
   default `reject!` or `accept!` states."
  (letfn [(default-rename [state i]
            (keyword (str "q" i)))
          (default-reject? [s] 
            (= s #{c/reject!}))
          (default-accept? [s] 
            (= s #{c/accept!}))]
    (fn 
      ([fsm] (fsm-reindex fsm default-rename))
      ([fsm f] (fsm-reindex fsm f default-reject? default-accept?))
      ([fsm r? a?] (fsm-reindex fsm default-rename r? a?))
      ([{:keys[accept states transitions initial] :as fsm} f r? a?]       
       (let [r? #(or (= % c/reject!) (r? %))
             a? #(or (= % c/accept!) (a? %))
             state-map (zipmap 
                         (->>
                           (fsm-reachable-state-seq fsm initial)
                           (filter #(not (or (r? %) (a? %)))))
                         (range))]
         (fsm-rename-states 
           fsm
           (fn [s]
             (cond (r? s) c/reject!
                   (a? s) c/accept!
                   :else (let [i (state-map s)]
                           (f s i))))))))))

;; ## FSM Equality

(defn- reindex-normalize
  "Normalize, then reindex FSM."
  [fsm]
  (-> fsm
    (fsm-reindex)
    (fsm-normalize)))

(defn fsm-equal?
  "Will check if two given FSMs have the same transitions between states."
  [f1 f2]
  (let [ri #(-> % fsm-reindex fsm-normalize)]
    (= (ri f1) (ri f2))))


