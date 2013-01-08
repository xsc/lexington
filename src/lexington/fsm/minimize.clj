(ns ^{ :doc "Implementation of Minimization Algorithms."
       :author "Yannick Scherer" }
  lexington.fsm.minimize
  (:use [clojure.set :as sets]
        [lexington.fsm.consts :as c]
        [lexington.fsm.nfa :only [nfa->dfa]]
        [lexington.fsm.dfa :only [reverse-dfa]]
        lexington.fsm.core
        lexington.fsm.utils))

;; ## Minimization

(defmulti minimize-dfa 
  "Minimize the given DFA usign the specified `:algorithm`:
   - `:brz`: Brzozowski Minimization (default)
   - `:hopcroft`: Hopcroft Minimization
  "
  (fn [dfa & {:keys[algorithm]}] 
    algorithm)
  :default :hopcroft)

;; ## Brzozowski Minimization
;;
;; 1. Reverse DFA to get an NFA with multiple initial states.
;; 2. Convert NFA to DFA.
;; 3. Reverse DFA again.
;; 4. Convert NFA to DFA to get minimal result.
;;
;; __Note:__ `lexington.fsm.dfa/reverse-dfa` already creates a suitable DFA, so
;; this function is nothing else than said reversal applied twice."

(defmethod minimize-dfa :brz
  [fsm & _]
  (-> fsm
    reverse-dfa
    reverse-dfa
    fsm-normalize))

(defmethod minimize-dfa :brz-unnormalized
  [fsm & _]
  (-> fsm
    reverse-dfa
    reverse-dfa))

;; ## Hopcroft Minimization
;;
;;     def hopcroft() :
;;       P = {Q−F, F}
;;       L = {F}
;;       while L != {}:
;;         S = extract(L)
;;         for a in ∑:
;;           for B in P:
;;             (B1,B2) = split(B, S, a)
;;             P = P \ {B}; P = P ∪ {B1, B2}
;;             if |B1| < |B2|:
;;               L = L ∪ {B1}
;;             else:
;;               L = L ∪ {B2}
;;       return P;
;;
;; The `split` function will be realised in `hopcroft-split`:
;;
;;     split(B', a, B) = ({q in B'|q leads to a state in B using a},
;;                        {q in B'|q does not lead to a state in B using a})
;;

(defn hopcroft-split
  "Split a given set of states (`p-set`) into those that lead into one of another set 
   of states (`dest-set`) given a specific trigger (`input`). Returns a two-element 
   vector where the first element is a set of states that match the given condition 
   and the second one a set of those that don't."
  [transitions p-set input dest-set]
  (let [dest? (set dest-set)]
    (reduce
      (fn [[m n] s]
        (let [matches? (when-let [t (get transitions s)]
                         (when-let [d (or (first (or (t input) (t c/any))) c/reject!)]
                           (dest? d)))]
          (if matches?
            (vector (conj m s) n)
            (vector m (conj n s)))))
      [#{} #{}]
      p-set)))

(defn hopcroft-refine-partitions
  "Refine the given partitions using Hopcroft's algorithm. Expects a normalized DFA."
  [P L {:keys[transitions] :as dfa}]
  (let [hsplit (partial hopcroft-split transitions)
        alphabet (fsm-alphabet dfa)
        conj-not-empty (fn [s e] 
                         (if (empty? e) 
                           s 
                           (conj s e)))]
    (loop [P (set P)
           L (set L)]
      (if-not (seq L)
        P
        (let [S (first L)
              L (disj L S)
              [P L] (reduce
                      (fn [[P L] a]
                        (reduce
                          (fn [[P L] B]
                            (let [[B1 B2] (hsplit B a S)]
                              (vector
                                (-> P
                                  (disj B)
                                  (conj-not-empty B1)
                                  (conj-not-empty B2))
                                (-> L
                                  (conj-not-empty
                                    (if (< (count B1) (count B2)) B1 B2))))))
                          [P L]
                          P))
                      [P L]
                      alphabet)]
          (recur P L))))))

(defn- rename-transitions
  "Based on a rename map generated from equivalency classes, clean up the
   transition table."
  [rename-map transitions]
  (reduce
    (fn [m [from tt]]
      (let [n (rename-map from)
            any-target (rename-map (or (first (tt c/any)) c/reject!))]
        (if (m n)
          m
          (->>
            (reduce
              (fn [tt [e to]]
                (let [to (rename-map (first to))]
                  (if (and (not (= e c/any)) (= to any-target))
                    tt
                    (assoc tt e #{to}))))
              {} tt)
            (assoc m n)))))
    {}
    transitions))

(defmethod minimize-dfa :hopcroft
  [fsm & _]
  (let [{:keys[accept reject states initial transitions] :as dfa} (-> fsm nfa->dfa fsm-normalize)
        accept? (set accept)
        reject? (set reject)

        ;; Create Partitions
        accept-states (set accept)
        other-states (sets/difference (set states) accept-states)
        partitions (hopcroft-refine-partitions 
                     #{accept-states other-states}
                     #{accept-states}
                     dfa)

        ;; Rename States
        partition-names (zipmap partitions (map #(keyword (str "q" %)) (range)))
        rename-state (->>
                       (mapcat
                         (fn [[p q]]
                           (let [q (if (or (contains? p c/reject!) 
                                           (some reject? p))
                                     c/reject!
                                     q)]
                             (map #(vector % q) p)))
                         partition-names)
                       (into {}))]
    (-> (dfa*)
      (assoc :initial (rename-state initial))
      (assoc :states  (set (vals rename-state)))
      (assoc :accept  (set (map rename-state accept)))
      (assoc :reject  (set (map rename-state reject)))
      (assoc :transitions (rename-transitions rename-state transitions)))))
