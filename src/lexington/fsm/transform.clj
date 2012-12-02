(ns ^{ :doc "FSM Transformation and Combination"
       :author "Yannick Scherer" }
  lexington.fsm.transform
  (:use [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.states :as s :only [reject! accept!]]))

;; Analysis

(defn state-seq-bfs
  "Create a BFS seq of all the states reachable from the given node."
  [{:keys[transitions]} root]
  (letfn [(find-next-states [visited-states check-states]
            (lazy-seq
              (let [next-states (filter (comp not visited-states)
                                        (mapcat #(vals (transitions %)) check-states))]
                (concat check-states 
                        (when (seq next-states)
                          (find-next-states (set (concat visited-states next-states)) next-states))))))]
    (find-next-states #{root} [root])))

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

;; ## Transformations

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
      :accept (set (map rename-fn accept))
      :reject (set (map rename-fn reject))
      :states (set (map rename-fn states))
      :initial (rename-fn initial)
      :transitions (reduce (fn [m [s t]]
                             (assoc m 
                                    (rename s)
                                    (reduce 
                                      (fn [tr [e to]]
                                        (assoc tr e (rename to)))
                                      {}
                                      t)))
                           {}
                           transitions))))

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
  ([fsm] (reindex-fsm fsm #(= % s/reject!)))
  ([{:keys[accept reject states transitions initial] :as fsm} reject-state?]
   (let [state-map (zipmap 
                     (filter (comp not reject-state?) (state-seq-bfs fsm initial))
                     (for [i (range)] (keyword (str "state-" i))))]
     (rename-fsm-states fsm (fn [s] 
                              (if (reject-state? s) 
                                s/reject! 
                                (state-map s)))))))

;; ## Cartesian Product

(defn- cartesian-product-transitions
  "Create transition map for cartesian product FSM."
  [t1 t2]
  (into {}
    (for [x (keys t1) y (keys t2)]
      (let [tx (t1 x)
            ty (t2 y)
            in (set (concat (keys tx) (keys ty)))]
        (when (and tx ty)
          (vector [x y]
                  (into {}
                    (for [e in] 
                      (let [txe (or (tx e) (tx t/any))
                            tye (or (ty e) (ty t/any))]
                        (when (and txe tye)
                          (vector e [txe tye])))))))))))

(defn- cartesian-product
  "Build the cartesian product of two FSM's. This is done by using the following data:
- States: `(:states fsm1)` X `(:states fsm2)`
- Initial State: (`(:initial fsm1)`, `(:initial fsm2)`)
- Transitions: { [x, y] { <input> [(get-in fsm1 [:transitions x <input>]), ...] ... } ... }`
  "
  [fsm1 fsm2 accept?]

  ;; New states and unique names for them
  (let [{ s1 :states i1 :initial t1 :transitions a1 :accept r1 :reject } fsm1
        { s2 :states i2 :initial t2 :transitions a2 :accept r2 :reject } fsm2]
    (let [states (for [x s1 y s2] [x y])
          initial [i1 i2]
          accept (filter (fn [[x y]]
                           (accept? x y a1 a2))
                         states)
          transitions (cartesian-product-transitions t1 t2)]
      (-> {}
        (assoc :states (set states))
        (assoc :initial initial)
        (assoc :transitions transitions)
        (assoc :accept (set accept))
        (assoc :reject #{})
        remove-unreachable-states
        (reindex-fsm #(= % [s/reject! s/reject!]))))))

;; ## Combinations

(def intersect-fsm
  "Create (cartesian product) intersection of two FSMs."
  (letfn [(intersect-acceptor? [x y a1 a2]
            (and (a1 x) (a2 y)))]
    (fn [fsm1 & fsms]
      (reduce
        #(cartesian-product %1 %2 intersect-acceptor?)
        fsm1
        fsms))))

(def union-fsm
  "Create (cartesian product) union of two FSMs."
  (letfn [(union-acceptor? [x y a1 a2]
            (or (a1 x) (a2 y)))]
    (fn [fsm1 & fsms]
      (reduce
        #(cartesian-product %1 %2 union-acceptor?)
        fsm1
        fsms))))

(defn difference-fsm
  "Create (cartesian product) difference of two FSMs."
  [fsm1 fsm2]
  (intersect-fsm fsm1 (invert-fsm fsm2)))
