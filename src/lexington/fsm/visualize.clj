(ns ^{ :doc "FSM Visualization using Dorothy/GraphViz"
       :author "Yannick Scherer" }
  lexington.fsm.visualize
  (:use dorothy.core
        [clojure.string :as string :only [join]]
        [lexington.fsm.transitions :as t]
        [lexington.fsm.states :as s]
        lexington.fsm.transform))

(defn- collect-transitions
  "Convert a map of `{ <input> :next-state ... }` to `{ :next-state [<input> ...] ... }`"
  [transitions]
  (reduce
    (fn [tr [e to]]
      (let [to (if (set? to) to (hash-set to))]
        (reduce
          (fn [m t]
            (merge-with concat m { t [e] }))
          tr
          to)))
    {}
    transitions))

(defn fsm->dot
  "Convert FSM to GraphViz Dot format."
  [fsm]
  (let [{:keys[accept reject states transitions initial]} (remove-unreachable-states fsm)]
    (-> 
      (graph 
        (concat
          [(graph-attrs { :rankdir "LR" })
           (node-attrs { :shape :ellipse :height "0.8" :width "0.8" :fontsize "8pt" :fixedsize "true" })
           (edge-attrs { :fontsize "8pt" })
           [:__init { :style "invisible" :height "0.1" :width "0.1" }]]

          (map (fn [s]
                 (cond (accept s)
                       [s { :color "darkgreen" :fontcolor "darkgreen" :style "bold" }]
                       (reject s)
                       [s { :color "red" :fontcolor "red" :style "bold" }]
                       :else s))
               (filter #(not (= % s/reject!)) states))
          [[:__init initial { :dir :forward }]]
          (mapcat
            (fn [[from to-map]]
              (let [ct (collect-transitions to-map)]
                (filter (comp not nil?)
                        (map (fn [[to es]]
                               (when-not (= to s/reject!)
                                 (let [label (string/join "," (map #(if (= % t/any) "*" (str %)) es))]
                                   [from to { :dir :forward :label label }])))
                             ct))))
            transitions)))
      dot)))

(defn show-fsm!
  "Open Dorothy Window to display the FSM."
  [fsm & options]
  (apply show! (fsm->dot fsm) options))

(defn save-fsm!
  "Save FSM to File using the given filename and format, e.g.:
    (save-fsm! fsm \"fsm.png\" { :format :png })
  "
  [fsm filename & options]
  (apply save! (fsm->dot fsm) filename options))
