(ns ^{ :doc "Lexington FSM Error Handling."
       :author "Yannick Scherer" }
  lexington.fsm.errors)

(def ^:dynamic *current-fsm-name* 
  "The currently generated/run FSM's name."
  nil)

(def ^:dynamic *current-meta*
  "Theerror source metadata. (:line)"
  nil)

(defmacro with-error-source
  "Make error messages reference the FSM that is concerned."
  [fsm-name & r]
  `(binding [*current-fsm-name* ~fsm-name]
     ~@r))

(defmacro with-error-meta
  "Make error messages reference the metadata that is connected with them."
  [m & r]
  `(binding [*current-meta* ~m]
     ~@r))

(defn error
  "Throw an exception with hints to what FSM it comes from."
  [& msg-parts]
  (throw (Exception. (str "FSM `" (or *current-fsm-name* "(anon)") "'"
                          (when-let [{:keys[line]} *current-meta*]
                            (str " [" *file* ", line " line "]"))
                          ":\n"
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

(defn transition-missing-arrow
  "Throw an exception when encountering a transition in the `state` macro that does not separate
   the input entity and the next state by an arrow."
  [s input]
  (error "missing '->' in transition '" input "' of state " s))
