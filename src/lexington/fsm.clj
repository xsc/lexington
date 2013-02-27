(ns ^{ :doc "Lexington FSM Facade"
       :author "Yannick Scherer" }
  lexington.fsm
  
  ;; Potemkin for Import
  (:use [potemkin :only [import-vars]])

  ;; Namespaces to import
  (:require lexington.fsm.core
            lexington.fsm.fn
            lexington.fsm.dfa
            lexington.fsm.nfa
            lexington.fsm.utils
            lexington.fsm.minimize
            lexington.fsm.visualize))

(import-vars
  [lexington.fsm.core

   fsm*
   dfa
   dfa*
   def-dfa
   nfa
   nfa*
   def-nfa
   epsilon-nfa
   epsilon-nfa*
   def-epsilon-nfa

   except
   except*
   one-of
   one-of*
   eps

   accept-in
   reject-in
   accept-empty
   initial-state]

  [lexington.fsm.fn

   nfa-fn
   count-fn
   recognize-fn
   prefix-count-fn
   prefix-match-count-fn
   prefix-match-fn]

  [lexington.fsm.dfa

   invert-dfa
   reverse-dfa
   cartesian-product-dfa
   cartesian-union-dfa
   cartesian-intersection-dfa
   cartesian-difference-dfa
   concat-dfa
   union-dfa
   loop-dfa
   loop0-dfa]

  [lexington.fsm.nfa

   concat-nfa
   union-nfa
   loop-nfa
   loop0-nfa
   epsilon-nfa->nfa
   multi-nfa->dfa
   nfa->dfa]

  [lexington.fsm.utils

   fsm-normalize
   fsm-next-states
   fsm-next-state-set
   fsm-destination-states
   fsm-transition-inputs
   fsm-alphabet
   fsm-state-seq
   fsm-reachable-state-seq
   fsm-reachable-states
   fsm-unreachable-states
   fsm-dead-states
   fsm-replace-states
   fsm-remove-states
   fsm-remove-unreachable-states
   fsm-remove-dead-states
   fsm-rename-states
   fsm-rename-single-state
   fsm-reindex
   fsm-equal?]

  [lexington.fsm.minimize

   minimize-dfa]

  [lexington.fsm.visualize

   show-fsm!
   show-fsm-all!
   save-fsm!
   fsm->dot])










