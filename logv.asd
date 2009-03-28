
(defpackage :logv-system
  (:use :cl))

(in-package :logv-system)

(asdf:defsystem :logv
  :depends-on (:rw-ut
	       :trivial-utf-8
               #-:lispworks :bordeaux-threads)
  :description "simple logging for cl-terrace"
  :components
  ((:file "package")
   #-:lispworks (:file "bordeaux-wrappers" :depends-on ("package"))
   #+:lispworks (:file "bordeaux-wrappers-lispworks" :depends-on ("package"))
   (:file "logv" :depends-on (#-lispworks "bordeaux-wrappers"
			      #+lispworks "bordeaux-wrappers-lispworks"))))
