(ns ^{ :doc "Lexington FSM Helper Functions"
       :author "Yannick Scherer" }
  lexington.fsm.helpers)

;; ## Sequence -> Map
;;
;; The `map`/`remap` functions are designed to be applied to a map, producing lazy sequences.
;; Once all mappings/remappings have been performed, the result can be translated back into
;; a map by using `into-map` or `into-map-in`.

(defn into-map
  [m]
  (into {} m))

(defn into-map-in
  "Convert map field's value (a map sequence) to map."
  [m k]
  (assoc m k (into-map (m k))))

;; ## Map/Remap

(defn remap
  "Apply function to each value in a map, producing a map seq
   `[ [key (f v1)] ... ]`"
  [m f]
  (map (fn [[k v]] 
         (vector k (f v))) m))

(defn remap-in
  "Apply remap function to a map field."
  [m k f]
  (assoc m k (remap (m k) f)))

(defn map-in
  "Apply Key/Value mapping function to map field."
  [m k f]
  (assoc m k
    (map (fn [[a b]]
           (vector a (f a b))) (m k))))

;; ## Debugging

(defn spy
  "Print a value, then return it. (Only for Debug purposes.)"
  [k]
  (prn k)
  k)
