(asdf:defsystem ramarren-state-machine
  :version "0"
  :description "Infinite state machines"
  :maintainer "Jakub Higersberger <ramarren@gmail.com>"
  :author "Jakub Higersberger <ramarren@gmail.com>"
  :licence "BSD-style"
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "state-machine" :depends-on ("package"))))
