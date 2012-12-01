(ns ^{ :doc "SVG Visualization using lacij"
       :author "Yannick Scherer" }
  lexington.fsm.visualize
  (:use dorothy.core))

(defn show-fsm!
  [{:keys[accept reject states transitions initial]}]
  (letfn [(node-id [s]
            (keyword (str "state-" (name s))))
          (edge-id [from to]
            (keyword (str "edge-" (name from) "-" (name to))))]
    (-> 
      (graph 
        (concat
          [(node-attrs { :shape :circle :fontsize "8pt" :fixedsize "true" })]
          states
          (mapcat
            (fn [[from to-map]]
              (map (fn [[e to]]
                     [from to { :dir :forward }]) to-map))
            transitions)))
      dot
      (show!))))



