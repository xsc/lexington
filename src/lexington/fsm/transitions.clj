(ns ^{ :doc "Transition Infrastructure"
       :author "Yannick Scherer" }
  lexington.fsm.transitions)

;; ## Special Keywords
(def nil-state "Reject Target State" ::reject)
(def ^:dynamic *default-state* "Default State" nil)

(defn- get-default-state
  "Get current default state."
  []
  (or *default-state* nil-state))

;; ## Special Inputs

(def eof
  "Will initiate a transition on end-of-data."
  (constantly ::eof))

(def any
  "Will initiate a transition not depending on the input."
  (constantly ::any))

(defn except
  "Transition Rule that will initiate a transition only if the input does not match
  the values given."
  [& args]
  (when (seq args)
    (fn [s]
      (cons [::any s]
            (map (fn [x]
                   [x nil]) args)))))

(defn one-of
  "Transition Rule that will initiate a transition only if the input does match one 
   of the values given."
  [& args]
  (set args))

;; ## Transition Map Generator

(defn- generate-transition-map
  "Generates an unambiguous transition map from an input sequence generated by
`generate-transition-pairs`. It will resolve the destination state of entities
that have `nil` as their next state by looking for the most applicable `::any`
declaration. Example:

    [ [ :a :a ]
      [ ::any :d ]
      [ :b nil ]
      [ :c nil ]
      [ ::any :c ]
      [ :b :b ] ]

The most applicable `::any`declaration is always the closest after a transition. 
However, later transitions might actually overwrite the destination (if they are not
nil). The algorithm used here is thus:

1. Reverse the input sequence and prepend `[::any ::reject]`.
2. Loop through the sequence.
  - if you encounter `::any` set the current default state to its destination;
    then remove all previous `::any` transition from the sequence and add the
    current one.
  - if you encounter a regular transition, remove all following transitions with the same
    input entity and a destination of nil. If the transition's own target state is nil,
    use the current default state instead, otherwise keep the transition as-is but remove all
    previous transitions with the same input entity.
3. Stop when you reach the end of the sequence.

**TODO:** Evaluate Performance."
  [pairs]
  (loop [pairs (cons [::any (get-default-state)]  (reverse pairs))
         result []
         current-default nil]
    (if-not (seq pairs)
      (into {} (map vec result))
      (let [[[e s :as pair] & rst] pairs]
        (if (= ::any e) 
          (recur rst (conj (filter (comp not #{::any} first) result) pair) s)
          (recur (filter (fn [[x y]] 
                           (or (not (= e x)) y)) rst)
                 (if s
                   (conj (filter (comp not #{e} first) result) pair)
                   (conj result [e current-default]))
                 current-default))))))

(defn- generate-transition-pairs
  "Takes a sequence of input/next-state pairs and produces a sequence of the form
`[ [single-input-entity next-state] ... ]` using the following rules:

- an input given as a set will be expanded: `[ #{a b} x ] -> [[a x] [b x]]`
- an input given as a function will result in a function call: `[ f s ] -> (f s)`
- an input given as a literal will not be touched: `[ e x ] -> [ [e x] ]`

The generated sequences will be concatenated in the order they were given. Example:

    (generate-transition-pairs [[:a :a]
                                [#{:b :c} :bc]
                                [(any) :d]])
    ; -> [ [:a :a] [:b :bc] [:c :bc] ... ]
  "
  [pairs]
  (mapcat 
    (fn [[e next-state :as pair]]
      (cond (set? e) (map #(vector % next-state) e)
            (fn? e)  (e next-state)
            :else [pair]))
    pairs))

(defn transitions->map
  "Convert a sequence of input/next-state pairs to a transition map. The input
   sequence is not specially structured, just a vector `[input state input state ...]`"
  [transitions]
  (-> (partition 2 transitions)
    generate-transition-pairs
    generate-transition-map))
