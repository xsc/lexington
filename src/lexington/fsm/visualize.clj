(ns ^{ :doc "FSM Visualization using Dorothy/GraphViz"
       :author "Yannick Scherer" }
  lexington.fsm.visualize
  (:use dorothy.core
        [clojure.string :as string :only [join]]
        [lexington.fsm.transitions :as t]
        [lexington.fsm.states :as s]
        [lexington.fsm.nfa :as nfa :only [epsi]]
        lexington.fsm.fsm))

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

(defn- node-name
  "Generate node name from keyword or set of keywords."
  [s]
  (if (or (vector? s) (set? s))
    (keyword (str "-" (string/join "," (map (comp name node-name) s)) "-" ))
    s))

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
                 (let [sn (node-name s)]
                   (cond (accept s)
                         [sn { :color "darkgreen" :fontcolor "darkgreen" :style "bold" }]
                         (reject s)
                         [sn { :color "red" :fontcolor "red" :style "bold" }]
                         :else sn)))
               (filter #(not (= % s/reject!)) states))
          [[:__init (node-name initial) { :dir :forward }]]
          (mapcat
            (fn [[from to-map]]
              (let [ct (collect-transitions to-map)]
                (filter (comp not nil?)
                        (map (fn [[to es]]
                               (when-not (= to s/reject!)
                                 (let [label (string/join "," 
                                                          (map (fn [e]
                                                                 (cond (= e t/any) "*"
                                                                       (= e nfa/epsi) "\u03f5"
                                                                       :else (str e))) es))]
                                   [(node-name from) (node-name to) { :dir :forward :label label }])))
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
