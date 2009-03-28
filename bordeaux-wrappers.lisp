;
; bordeaux-wrappers
;

(in-package :logv)

(defun make-lock (name)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  (bt:make-lock name))

(defun make-recursive-lock (name)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  (bt:make-recursive-lock name))

(defmacro with-recursive-lock-held ((lock) &body body)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  `(bt:with-recursive-lock-held (,lock) ,@body))

(defmacro with-lock-held ((lock) &body body)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  `(bt:with-lock-held (,lock) ,@body))

(defun thread-name (thread)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  (bt:thread-name thread))

(defun current-thread ()
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  (bt:current-thread))

