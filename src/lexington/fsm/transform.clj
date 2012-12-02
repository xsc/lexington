(ns ^{ :doc "FSM Transformation and Combination"
       :author "Yannick Scherer" }
  lexington.fsm.transform
  (:use [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.states :as s :only [reject! accept!]]))

;; ## Transformations

(defn invert-fsm
  "Create an FSM that does accept everything not accepted by the given FSM. This
   is done by switching the accepting states with all non-accepting states. Attention:
   rejecting states will be cleared."
  [{:keys[accept states] :as fsm}]
  (-> fsm
    (assoc :accept (set (filter (comp not accept) states)))
    (assoc :reject #{})))

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

;; ## Cartesian Product

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
                      (let [txe (or (tx e) (tx t/any))
                            tye (or (ty e) (ty t/any))]
                        (when (and txe tye)
                          (vector e (state-map [txe tye]))))))))))))

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
          state-map (->
                      (zipmap states (for [i (range)] (keyword (str "state-" i))))
                      (assoc [s/reject! s/reject!] s/reject!))
          initial (state-map [i1 i2])
          accept (map state-map
                      (filter (fn [[x y]]
                                (accept? x y a1 a2))
                              states))
          transitions (cartesian-product-transitions state-map t1 t2)]
      (-> {}
        (assoc :states (set (vals state-map)))
        (assoc :initial initial)
        (assoc :transitions transitions)
        (assoc :accept (set accept))
        (assoc :reject #{})
        remove-unreachable-states))))

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
