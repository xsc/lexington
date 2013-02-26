(ns ^{ :doc "Lexer Utilities, e.g. for Token Processing/Transformation."
       :author "Yannick Scherer" }
  lexington.lexer.utils
  (:use lexington.tokens))

;; ## Transformation Helpers

(defn- transform-tokens
  "Add a token transformation to a Lexer. The optional third parameter
   can be a predicate determining whether a token shall be transformed."
  ([lex f]
   (fn [in-seq]
     (map f (lex in-seq))))
  ([lex f check?]
   (if (fn? check?)
     (fn [in-seq]
       (map (fn [token] 
              (if (check? token)
                (f token)
                token)) (lex in-seq)))
     (transform-tokens lex f))))

(defn- transform-tokens-by-type
  "Add a type-based token transformation to a Lexer. The third parameter is a map
   containg the key :only or :except to specify which types the transformation shall
   be applied to."
  [lex f { :keys [only exclude] }]
  (transform-tokens lex f
    (cond
      only (let [check-only? (if (coll? only) (set only) #(= % only))]
             (comp check-only? token-type))
      exclude (let [check-except? (if (coll? exclude) (set exclude) #(= % exclude))]
               (comp not check-except? token-type))
      :else nil)))

;; ## Lexer Wrappers

;; ### Token Filters

(defn discard
  "Wrap a Lexer into a function removing tokens corresponding to a given number of
   token types. Example:
  
     (-> lexer
       (discard :whitespace :comment))
  "
  [lex & types-to-discard]
  (let [type-set (set types-to-discard)]
    #(->> (lex %) (filter (comp not type-set token-type)))))

(defn retain
  "Wrap a Lexer into a function discarding all tokens except those corresponding to a
   given number of token types. Example:

     (-> lexer
       (retain :comment))
  "
  [lex & types-to-retain]
  (let [type-set (set types-to-retain)]
    #(->> (lex %) (filter (comp type-set token-type)))))

;; ### Predefined Data Generators

(defn with-string
  "Wrap a Lexer into a function adding a field containing a token's string value to
   the token map. Example:

     (-> lexer
       (with-string :str))

   It's possible to specify exceptions for this operation or to narrow it down to a
   specific set of types:

     (-> lexer (with-string :str :exclude [:boolean]))
     (-> lexer (with-string :str :only [:string]))
  "
  [lex k & {:keys[exclude only] :as spec}]
  (letfn [(add-string-field [token]
            (assoc token k (apply str (token-data token))))]
    (transform-tokens-by-type lex add-string-field spec)))

(defn with-int
  "Wrap a Lexer into a function adding a field containing a token's integer value to
   the token map. Example:

     (-> lexer
       (with-int :int :only [:integer]))
  "
  [lex k & {:keys[exclude only] :as spec}]
  (letfn [(add-int-field [token]
            (assoc token k (Integer/parseInt (apply str (token-data token)))))]
    (transform-tokens-by-type lex add-int-field spec)))

;; ### Stateless Data Generators

(defn- build-stateless-handler-map
  "Convert a list of token-type(s)/handler pairs into a handler map usable by `generate`
   or `generate-stateful`."
  [handlers]
  (reduce (fn [m [types f]]
            (if (coll? types)
              (reduce #(assoc %1 %2 f) m types)
              (assoc m types f)))
          {}
          (partition 2 handlers)))

(defn generate
  "Wrap a Lexer into a function handling tokens when they are encountered. The given
   handler functions can return a value that will end up in the token map using the
   given handler-key. Example:
    
     (-> lexer
       (with-string :str)
       (generate :num-value
         :integer       #(Integer/parseInt (:str %))
         [:float :real] #(Double/parseDouble (:str %))))
  "
  [lex handler-key & handlers]
  (let [handler-map (build-stateless-handler-map handlers)]
    (letfn [(process-token [token]
              (let [type (token-type token)]
                (if-let [handler-result (when-let [h (get handler-map type)] 
                                          (h token))]
                  (assoc token handler-key handler-result)
                  token)))]
      #(->> (lex %) (map process-token)))))

(defn generate-for
  "Wrap a Lexer into a function handling a specific token type when it is encountered,
   producing additional entries in the token map based on the given key/handler pairs.
   Example:

     (-> lexer
       (with-string :str)
       (generate-for :integer
         :int #(Integer/parseInt (:str %))
         :sqrt #(Math/sqrt (:int %))
         :half #(/ (:int %) 2)))
  "
  [lex tk-types & handlers]
  (let [handler-pairs (partition 2 handlers)
        check-type? (if (coll? tk-types) (set tk-types) #(= tk-types %))]
    (letfn [(process-token [token]
              (let [type (token-type token)]
                (if (check-type? type)
                  (reduce
                    (fn [t [k handler]]
                      (assoc t k (handler t)))
                    token
                    handler-pairs)
                  token)))]
      #(->> (lex %) (map process-token)))))

;; ### Stateful Data Generators

(defn- build-stateful-handler-map
  "Convert a sequence of stateful handler specifications (e.g. keyword/map or 
   keyword/function pairs) to its corresponding map representation, containing
   the keys :before, :handle and :after."
  [handlers]
  (reduce
    (fn [m [types handler]]
      (let [h (cond (map? handler) (let [{:keys[before handle after]} handler]
                                     (when handle
                                       (if-not (or before after)
                                         (assoc handler :after handle)
                                         handler)))
                    (fn? handler) { :handle handler :after handler }
                    :else nil)]
        (if (coll? types)
          (reduce #(assoc %1 %2 h) m types)
          (assoc m types h))))
    {}
    (partition 2 handlers)))

(defn generate-stateful
  "Wrap a Lexer into a function performing stateful operations on selected
   token types. The value created by said operations will be stored in the token
   map using the given handler-key. Example:
  
     (-> lexer
       (with-int :int :only [:integer :float])
       (generate-stateful :int-sum 0.0
         :integer #(+ (:int %1) %2)
         :float   #(+ (:int %1) %2)))
  "
  [lex handler-key initial-state & handlers]
  (let [handler-map (build-stateful-handler-map handlers)]
    (letfn [(next-state [k token state]
              (if-let [handler (get handler-map (token-type token))]
                (if-let [h (get handler k)]
                  (h token state)
                  state)
                state))
            (handle-token [token state]
              (if-let [handler (get handler-map (token-type token))]
                (if-let [h (:handle handler)]
                  (assoc token handler-key (h token state))
                  token)
                token))
            (lazy-handle [token-seq state]
              (lazy-seq
                (when (seq token-seq)
                  (let [token (first token-seq)
                        state (next-state :before token state)
                        token (handle-token token state)
                        state (next-state :after token state)]
                    (cons token (lazy-handle (rest token-seq) state))))))]
      #(-> (lex %) (lazy-handle initial-state)))))

;; ### Allow for Erroneous Input

(defn allow-partial-input
  "End execution of the lexer once an Exception is thrown."
  [lex]
  (fn [in-seq]
    (let [sq (lex in-seq)]
      (letfn [(lazy-try-catch [sq]
                (lazy-seq
                  (try
                    (when (seq sq)
                      (when-let [x (first sq)]
                        (cons x (lazy-try-catch (rest sq)))))
                    (catch Exception _ nil))))]
        (lazy-try-catch sq)))))

(defn recover-from-error
  "Recover from input that cannot be recognized."
  [lex]
  (fn [in-seq]
    (letfn [(lazy-try-catch [in-seq token-seq]
              (lazy-seq
                (try
                  (when (seq token-seq)
                    (when-let [x (first token-seq)]
                      (cons x (lazy-try-catch
                                (drop (token-length x) in-seq)
                                (rest token-seq)))))
                  (catch Exception _ 
                    (lazy-try-catch
                      (rest in-seq)
                      (lex (rest in-seq)))))))]
      (lazy-try-catch in-seq (lex in-seq)))))
