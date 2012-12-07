(ns ^{ :doc "FSM Visualization using Dorothy/GraphViz"
       :author "Yannick Scherer" }
  lexington.fsm.visualize
  (:use dorothy.core
        [clojure.string :as string :only [join]]
        [lexington.fsm.consts :as c]
        lexington.fsm.utils))

;; ## Helpers

(defn- collect-transitions
  "Convert a map of `{ <input> #{:next-state ...} ... }` to `{ :next-state [<input> ...] ... }`"
  [transitions]
  (reduce
    (fn [tr [e to]]
      (reduce
        (fn [m t]
          (merge-with concat m { t [e] }))
        tr
        to))
    {}
    transitions))

(defn- node-name
  "Generate node name from keyword or set of keywords."
  [s]
  (if (or (vector? s) (set? s))
    (keyword (str "-" (string/join "," (map (comp name node-name) s)) "-" ))
    s))

;; ## GraphViz Styles

(def ^:const ^:private NODE_STYLE
  { :shape :ellipse 
    :height "0.8"
    :width "0.8"
    :fontsize "8pt"
    :fixedsize "true" })

(def ^:const ^:private EDGE_STYLE
  { :fontsize "8pt"
    :dir :forward })

(def ^:const ^:private INIT_STYLE
  { :style :invisible
    :height "0.05"
    :width "0.05" })

(def ^:const ^:private ACCEPT_STYLE
  { :shape :doublecircle
    :color :darkgreen
    :fontcolor :darkgreen
    :height "0.7"
    :width "0.7" })

(def ^:const ^:private REJECT_STYLE
  { :shape :doublecircle
    :color :red
    :fontcolor :red
    :height "0.7"
    :width "0.7" })

;; ## GraphViz Generation Functions

(defn- default-ignore-state?
  [s]
  (= s c/reject!))

(defn fsm->dot
  "Convert FSM to GraphViz Dot format."
  ([fsm] (fsm->dot fsm default-ignore-state?))
  ([fsm ignore-state?]
   (let [{:keys[accept reject states transitions initial]} (-> fsm
                                                             fsm-normalize
                                                             fsm-remove-unreachable-states)
         init-node (keyword (gensym))]
     (letfn [(generate-node [s]
               (when-not (ignore-state? s)
                 (let [sn (node-name s)]
                   (cond ((set accept) s) [sn ACCEPT_STYLE]
                         ((set reject) s) [sn REJECT_STYLE]
                         :else [sn]))))
             (generate-edges [from to-map]
               (when-not (ignore-state? from)
                 (let [ct (collect-transitions to-map)]
                   (->> ct
                     (map 
                       (fn [[to es]]
                         (when-not (ignore-state? to)
                           (let [label (->>
                                         (map (fn [e]
                                                (cond (= e c/any) "*"
                                                      (= e c/epsi) "\u03f5"
                                                      :else (str e))) es)
                                         (string/join ","))]
                             (vector
                               (node-name from)
                               (node-name to)
                               { :label label })))))
                     (filter (comp not nil?))))))]
       (-> 
         (graph
           (concat
             [(graph-attrs { :rankdir "LR" })
              (node-attrs NODE_STYLE)
              (edge-attrs EDGE_STYLE)
              [init-node INIT_STYLE]]
             (->> states
               (map generate-node)
               (filter (comp not nil?)))
             [[init-node (node-name initial)]]
             (mapcat (fn [[from to]]
                       (generate-edges from to)) transitions)))
         dot)))))

;; ## Presentation Functions

(defn show-fsm!
  "Open Dorothy Window to display the FSM."
  ([fsm] (show-fsm! fsm default-ignore-state?))
  ([fsm ignore?] (show! (fsm->dot fsm ignore?))))

(defn show-fsm-all!
  "Open Dorothy Window to display the FSM, not ignoring any states."
  [fsms]
  (let [fsms (if (vector? fsms) fsms (vector fsms))]
    (doseq [fsm fsms]
      (show-fsm! fsm (constantly nil)))))

(defn save-fsm!
  "Save FSM to File using the given filename and format, e.g.:
    (save-fsm! fsm \"fsm.png\" { :format :png })
  "
  [fsm filename & options]
  (apply save! (fsm->dot fsm) filename options))
