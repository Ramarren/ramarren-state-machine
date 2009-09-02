(in-package :ramarren-state-machine)

(defclass state-machine ()
  ((name :accessor name-of :initarg :name)
   (states :accessor states-of :initarg :states :initform (make-hash-table))))

(defmethod print-object ((state-machine state-machine) stream)
  (print-unreadable-object (state-machine stream :type t :identity t)
    (format stream "~a: ~a states"
            (name-of state-machine)
            (hash-table-count (states-of state-machine)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (defvar *current-state-machine* nil)
    (defvar *state-machines* (make-hash-table))))

(defmacro define-state-machine (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (gethash ',name *state-machines*)
       (setf (gethash ',name *state-machines*)
             (make-instance 'state-machine :name ',name)))))

(defmacro in-state-machine (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *current-state-machine* (gethash ',name *state-machines*))))

(defmacro defsm* (name)
  `(progn
     (define-state-machine ,name)
     (in-state-machine ,name)))

(defun drop-state-machine (name)
  (when (eq (gethash name *state-machines*)
            *current-state-machine*)
    (setf *current-state-machine* nil))
  (remhash name *state-machines*))

(defun find-state-machine (name)
  (gethash name *state-machines*))

(defclass state-machine-state ()
  ((state-machine :accessor state-machine-of :initarg :state-machine :initform *current-state-machine*)
   (next-state :accessor next-state-of :initarg :next-state)
   (state-args  :accessor state-args-of  :initform nil )))

(defun drive-state-machine (state-machine-state &rest driver-args)
  (funcall (gethash (next-state-of state-machine-state)
                    (states-of (state-machine-of state-machine-state)))
           state-machine-state
           driver-args))

(defmacro defstate (name-and-options state-args driver-args &body body)
  (destructuring-bind (name . options) (ensure-list name-and-options)
    (let ((state-machine (if (getf options :state-machine)
                             (gethash *state-machines* (getf options :state-machine))
                             *current-state-machine*)))
      (let ((defun-name (if (gethash name (states-of state-machine))
                            (gethash name (states-of state-machine))
                            (setf (gethash name (states-of state-machine))
                                  (gensym (format nil "~a-" name))))))
        (with-unique-names (state-machine-state driver-args-arg next-state next-state-args)
          `(progn
             (setf (gethash ,name (states-of (find-state-machine ',(name-of state-machine))))
                   ',defun-name)
             (defun ,defun-name (,state-machine-state ,driver-args-arg)
               (destructuring-bind ,state-args (state-args-of ,state-machine-state)
                 (destructuring-bind ,driver-args ,driver-args-arg
                   (labels ((next-state (,next-state &rest ,next-state-args)
                              (setf (next-state-of ,state-machine-state) ,next-state
                                    (state-args-of ,state-machine-state) ,next-state-args)
                              (return-from ,defun-name ,next-state)))
                     ,@body))))))))))
