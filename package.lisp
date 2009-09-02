(defpackage #:ramarren-state-machine
    (:nicknames #:sm)
  (:use #:cl #:alexandria)
  (:export #:state-machine
           #:*current-state-machine*
           #:define-state-machine
           #:in-state-machine
           #:defsm*
           #:drop-state-machine
           #:find-state-machine
           #:state-machine-state
           #:drive-state-machine
           #:defstate
           #:next-state
           #:sub-machine))
