(in-package :ramarren-state-machine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (drop-state-machine 'test))

(defsm* test)

(defstate :start () ()
  (next-state :mid))

(defstate :mid () ()
  (next-state :end))

(defstate :end () ()
  (next-state :end))
