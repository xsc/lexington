(ns ^{ :doc "FSM Visualization using Dorothy/GraphViz"
       :author "Yannick Scherer" }
  lexington.fsm.visualize
  (:use dorothy.core
        [lexington.fsm.fsm :as fsm]
        [lexington.fsm.transitions :as t]
        [lexington.fsm.states :as s]))

(defn fsm->dot
  "Convert FSM to GraphViz Dot format."
  [fsm]
  (let [{:keys[accept reject states transitions initial]} (remove-unreachable-states fsm)]
    (letfn [(node-id [s]
              (keyword (str "state-" (name s))))
            (edge-id [from to]
              (keyword (str "edge-" (name from) "-" (name to))))]
      (-> 
        (graph 
          (concat
            [(node-attrs { :shape :ellipse :height "0.8" :width "0.8" :fontsize "8pt" :fixedsize "true" })
             (edge-attrs { :fontsize "8pt" })
             [:__init { :style "invisible" :height "0.1" :width "0.1" }]]

            (map (fn [s]
                   (cond (accept s)
                           [s { :color "darkgreen" :fontcolor "darkgreen" :style "bold" }]
                         (reject s)
                           [s { :color "red" :fontcolor "red" :style "bold" }]
                         :else s))
                 (filter #(not (= % (s/reject))) states))
            [[:__init initial { :dir :forward }]]
            (mapcat
              (fn [[from to-map]]
                (filter (comp not nil?)
                        (map (fn [[e to]]
                               (when-not (= to (s/reject))
                                 [from to { :dir :forward 
                                           :label (if (= e (t/any)) "*" (str e)) }])) to-map)))
              transitions)))
        dot))))

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
