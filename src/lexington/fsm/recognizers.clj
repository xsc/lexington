(ns 
  lexington.fsm.recognizers
  (:use lexington.fsm.core))

;; ## Stateless Recognizer
;; A "stateless" FSM will run until one of the final states (accept/accept-ignore/reject)
;; is reached. It is a function that takes an input and returns the number of input elements
;; it recognized or nil.

(defn recognizer
  [{:keys [initial transitions] :as fsm}]
  (letfn [(next-state [current-state input]
            (if-let [new-state (or (get-in transitions [current-state input])
                                   (get-in transitions [current-state ::any]))]
              new-state
              (reject)))]
    (let [has-accepted? #{(accept) (accept-ignore)}
          has-rejected? #{(reject)}]
        (fn [in-seq]
          (loop [state initial
                 sq    in-seq
                 c     0]
            (cond (has-accepted? state) c
                  (has-rejected? state) nil
                  (not (seq sq)) (when (has-accepted? (next-state state (eof))) c)
                  :else (when-let [s (next-state state (first sq))]
                          (recur s (rest sq) (if (= s ::accept-ignore) c (inc c))))))))))

;; ## FSM with internal State
(defn stateful-recognizer
  [fsm]
  (let [initial-map { :state (:initial-state fsm) 
                      :input nil }
        current-state (atom initial-map)
        transitions (:transition-table fsm)]
    (letfn [(next-state [{:keys[state input] :as m} e]

              (if-let [t (or (get-in transitions [state e])
                             (get-in transitions [state (any)]))]
                (-> m
                  (assoc :state t)
                  (assoc :input (concat input [e])))
                m))]
      (fn [e]
        (cond (= e ::reset) (swap! current-state (fn [x] initial-map))
              :else (swap! current-state next-state e))))))
