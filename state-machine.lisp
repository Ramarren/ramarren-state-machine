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

(defun ensure-state-machine (state-machine-state)
  (let ((sm (state-machine-of state-machine-state)))
    (let ((sm (if (listp sm) (car sm) sm)))
      (if (typep sm 'state-machine)
          sm
          (error "~a has no state machine." state-machine-state)))))

(defun maybe-sub-machine (state-machine-state)
  (with-accessors ((sm-stack state-machine-of)) state-machine-state
    (when sm-stack
      (pop sm-stack)
      (when sm-stack
        (setf (next-state-of state-machine-state)
              (pop (state-machine-of state-machine-state)))
        (get-next-state-function state-machine-state)))))

(defun get-next-state-function (state-machine-state)
  (let ((next-state (next-state-of state-machine-state))
        (state-machine (ensure-state-machine state-machine-state)))
    (if next-state
        (if-let (next-state-function (gethash next-state (states-of state-machine)))
          next-state-function
          (error "State machine ~s has no state ~s." state-machine next-state))
        (maybe-sub-machine state-machine-state))))

(defgeneric drive-state-machine (state-machine-state &rest driver-args)
  (:method ((state-machine-state state-machine-state) &rest driver-args)
    (let ((next-state-function (get-next-state-function state-machine-state)))
      (when next-state-function
        (funcall next-state-function
                 state-machine-state
                 driver-args))
      (values (and next-state-function t)))))

(defun next-state (next-state &rest next-state-args)
  (declare (ignore next-state next-state-args))
  (error "Top level next-state is not supposed to be called. It's to give slime autodoc a chance."))

(defun sub-machine (next-state sub-machine sub-state &rest next-state-args)
  (declare (ignore next-state sub-machine sub-state next-state-args))
  (error "Top level sub-machine is not supposed to be called. It's to give slime autodoc a chance."))

(defun make-labels-form (defun-name state-machine-state)
  (with-unique-names (next-state next-state-args sub-machine sub-state sm)
    `((next-state (,next-state &rest ,next-state-args)
                  (assert (state-machine-of ,state-machine-state))
                  (unless (eql ,next-state t)
                    (setf (next-state-of ,state-machine-state) ,next-state))
                  (setf (state-args-of ,state-machine-state) ,next-state-args)
                  (return-from ,defun-name ,next-state))
      (sub-machine (,next-state ,sub-machine ,sub-state &rest ,next-state-args)
                   (with-accessors ((,sm state-machine-of)) ,state-machine-state
                     (let ((,next-state (if (eql ,next-state t)
                                            (next-state-of ,state-machine-state)
                                            ,next-state)))
                       (etypecase ,sm
                         (state-machine
                            (setf ,sm
                                  (list (find-state-machine ,sub-machine)
                                        ,next-state
                                        ,sm)))
                         (list
                            (push ,next-state ,sm)
                            (push (find-state-machine ,sub-machine) ,sm)))))
                   (apply #'next-state ,sub-state ,next-state-args)))))

(defmacro defstate (name-and-options state-args driver-args &body body)
  (destructuring-bind (name . (&key state-machine (state (gensym "STATE-MACHINE-STATE-"))))
      (ensure-list name-and-options)
    (let ((state-machine (if state-machine
                             (gethash state-machine *state-machines*)
                             *current-state-machine*)))
      (let ((defun-name (if (gethash name (states-of state-machine))
                            (gethash name (states-of state-machine))
                            (setf (gethash name (states-of state-machine))
                                  (gensym (format nil "~a-~a-" (name-of state-machine) name))))))
        (with-unique-names (driver-args-arg)
          `(progn
             (setf (gethash ,name (states-of (find-state-machine ',(name-of state-machine))))
                   ',defun-name)
             (defun ,defun-name (,state ,driver-args-arg)
               (destructuring-bind ,state-args (state-args-of ,state)
                 (destructuring-bind ,driver-args ,driver-args-arg
                   (labels ,(make-labels-form defun-name state)
                     ,@body))))))))))
