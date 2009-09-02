(in-package :ramarren-state-machine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (drop-state-machine 'test)
  (drop-state-machine 'test2))

(defsm* test)

(defstate (:start :state a) () ()
  (print :start)
  (next-state :mid))

(defstate (:mid :state-machine test) () ()
  (print :mid)
  (next-state :end))

(defstate (:end :state sms) () ()
  (print :end)
  (if (and (listp (state-machine-of sms))
           (cdr (state-machine-of sms)))
      (pop-machine :end2)
      (next-state :end)))

(defsm* test2)

(defstate :start2 () ()
  (print :start2)
  (sub-machine 'test :start))

(defstate :end2 () ()
  (print :end2)
  (next-state :end2))
