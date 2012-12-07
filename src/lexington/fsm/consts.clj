(ns ^{ :doc "Constants for Lexington FSMs"
       :author "Yannick Scherer" }
  lexington.fsm.consts)

;; ## Constants

(def ^:const epsi    "The epsilon input." ::epsilon)
(def ^:const any     "The any input."     ::any)
(def ^:const reject! "The reject state."  ::reject)
(def ^:const accept! "The accept state."  ::accept)
