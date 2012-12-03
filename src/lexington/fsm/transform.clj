(ns ^{ :doc "FSM Transformation and Combination"
       :author "Yannick Scherer" }
  lexington.fsm.transform
  (:use [lexington.fsm.states :as s :only [reject! accept!]]))

;; ## Analysis (NFA/DFA)

(defn state-seq-bfs
  "Create a breadth-first seq of all the states reachable from the given node."
  ([fsm] (state-seq-bfs fsm (:initial fsm)))
  ([{:keys[transitions]} root]
   (loop [next-states [root]
          visited-states []]
     (let [visited-states (concat visited-states next-states)]
       (if-not (seq next-states)
         visited-states
         (let [child-states (filter (comp not (set visited-states))
                                    (mapcat #(mapcat (fn [x]
                                                       (if (set? x)
                                                         x
                                                         (vector x)))
                                                      (vals (transitions %))) next-states))]
           (recur child-states visited-states)))))))

(defn state-seq-dfs
  "Create a depth-first seq of all the states reachable from the given node."
  ([fsm] (state-seq-dfs fsm (:initial fsm)))
  ([{:keys[transitions]} root]
   (loop [next-states [root]
          visited-states []]
     (if-not (seq next-states)
       visited-states
       (let [[s & rst] next-states
             child-states (filter (comp not (set (cons s visited-states)))
                                  (mapcat (fn [x]
                                            (if (set? x) x (vector x)))
                                          (vals (transitions s))))
             rest-states (filter (comp not (set child-states)) rst)]
         (recur (concat child-states rest-states)
                (concat visited-states [s])))))))

(defn reachable-states
  "Get a set of all reachable states based on an initial state and a transition map."
  [fsm root]
  (set (state-seq-bfs fsm root)))

(defn unreachable-states
  "Get a set of all unreachable states based on a set of all states, an initial one and
   a transition map."
  [{:keys[states] :as fsm} root]
  (let [reachable (reachable-states fsm root)]
    (set (filter (comp not reachable) states))))

;; ## Transformations (NFA/DFA)

(defn invert-fsm
  "Create an FSM that does accept everything not accepted by the given FSM. This
   is done by switching the accepting states with all non-accepting states. Attention:
   rejecting states will be cleared."
  [{:keys[accept states] :as fsm}]
  (-> fsm
    (assoc :accept (set (filter (comp not accept) states)))
    (assoc :reject #{})))

(defn rename-fsm-states
  "Rename FSM states using the given function. If the function returns nil,
   the current name will be kept."
  [{:keys[accept reject states transitions initial] :as fsm} rename-fn]
  (letfn [(rename [x] (or (rename-fn x) x))]
    (assoc fsm
      :accept (set (map rename accept))
      :reject (set (map rename reject))
      :states (set (map rename states))
      :initial (rename initial)
      :transitions (reduce (fn [m [s t]]
                             (assoc m 
                                    (rename s)
                                    (reduce 
                                      (fn [tr [e to]]
                                        (if (set? to)
                                          (assoc tr e (set (map rename to)))
                                          (assoc tr e (rename to))))
                                      {}
                                      t)))
                           {}
                           transitions))))

(defn prefix-fsm-states
  "Prefix all states of an FSM with a given string."
  [fsm prefix]
  (rename-fsm-states fsm
    (fn [x]
      (when-not (or (= x s/accept!) (= x s/reject!))
        (keyword (str prefix (name x)))))))

(defn remove-unreachable-states
  "Remove all unreachable states from an FSM."
  [{:keys[initial transitions accept reject states] :as fsm}]
  (let [reachable? (reachable-states fsm initial)]
    (assoc fsm
      :accept (set (filter reachable? accept))
      :reject (set (filter reachable? reject))
      :states (set (filter reachable? states))
      :transitions (reduce (fn [m [s t]]
                             (if (reachable? s)
                               (assoc m s t)
                                m))
                           {}
                           transitions))))

(defn reindex-fsm
  "Rename states to `state-x` where `state-0` is the initial state. The higher the
   given number the further away from the root node is a state."
  ([fsm] (reindex-fsm fsm #(= % s/reject!) #(= % s/accept!)))
  ([{:keys[accept reject states transitions initial] :as fsm} reject-state? accept-state?]
   (let [state-map (zipmap 
                     (filter (comp not reject-state?) (state-seq-bfs fsm initial))
                     (for [i (range)] (keyword (str "state-" i))))]
     (rename-fsm-states fsm (fn [s] 
                              (cond (reject-state? s) s/reject!
                                    (accept-state? s) s/accept!
                                    :else (state-map s)))))))

