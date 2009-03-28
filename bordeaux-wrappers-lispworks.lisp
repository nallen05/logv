;
; lispworks-compatibility.lisp
;

(in-package :logv)

(defun make-lock (name)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  (mp:make-lock :name name))
  
(defun make-recursive-lock (name)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  (mp:make-lock :name name))

(defmacro with-recursive-lock-held ((lock) &body body)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  `(mp:with-lock (,lock) ,@body))

(defmacro with-lock-held ((lock) &body body)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  `(mp:with-lock (,lock) ,@body))

(defun thread-name (thread)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  (mp:process-name thread))

(defun current-thread ()
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  (mp:get-current-process))

