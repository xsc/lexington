(ns ^{ :doc "Lexington FSM Error Handling."
       :author "Yannick Scherer" }
  lexington.fsm.errors)

(def ^:dynamic *current-fsm-name* 
  "The currently generated/run FSM's name."
  nil)

(defn error
  "Throw an exception with hints to what FSM it comes from."
  [& msg-parts]
  (throw (Exception. (str "Error in FSM " 
                          (or *current-fsm-name* "(anon)") ": " 
                          (apply str msg-parts)))))

(defn duplicate-state
  "Throw an exception when encountering an already declared state."
  [s]
  (error "duplicate state: " s))

(defn transition-invalid
  "Throw an exception when encountering an invalid transition."
  [from to & msg-parts]
  (error "invalid transition [" from " -> " to "] : " (apply str msg-parts)))

(defn transition-unknown-destination
  "Throw an exception when encountering a transition that has an unknown target state."
  [from to]
  (transition-invalid from to "unknown destination"))
