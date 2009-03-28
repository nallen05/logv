; `Logv' is a simple logging utility for Common Lisp
;
; The most useful form is LOGV, a macro that behaves like PROGN except that it writes
; each form and its return value to the log.
;
; the following code:
;
;    (logv (+ 1 (logv (+ 1 1)))
;          (+ 10 10)
;          (* 64 64))
;
; will return 4096 and might write something like the following lines to the log:
;
;    ; [2009/03/27] [19:15:08] [repl-thread]: (+ 1 1) -> 2
;    ; [2009/03/27] [19:15:08] [repl-thread]: (+ 1 (LOGV (+ 1 1))) -> 3
;    ; [2009/03/27] [19:15:08] [repl-thread]: (+ 10 10) -> 20
;    ; [2009/03/27] [19:15:08] [repl-thread]: (* 64 64) -> 4096
;
; LOGVS is like LOGV but understands multiple return values. FORMAT-LOG lets you
; write to the log with a format string and arguments [like FORMAT].
;
; the behavior of LOGV and LOGVS is controlled by "log settings". Log settings can
; be changed or accessed with SETF-able function LOG-SETTING. for example the
; following code:
;
;    (setf (log-setting :log-output) "/path/to/log.txt")
;
; makes LOGV, LOGVS, and FORMAT-LOG write to the log file "log.txt"
;
; if the :LOG-OUTPUT setting is:
;
;   -T: then things that write to the log write to *STANDARD-OUTPUT*
;   -NIL: then things that normally write to the log don't do anything
;   -a stream: then things that write to the log write to the stream
;   -a pathname or a string [a pathname designator]: then things that write to the
;    log write to the file pointed to by the string/pathname. the file is created if
;    it doesn't already exist. if there are special characters they are encoded in
;    UTF-8
;
; the :LOG-PREFIX-STRING-FACTORY setting is a thunk [unary function] that returns a
; string to be prepended to the log line.
;
; The macro DEF-LOG-ENV enables you to easily roll your own LOGV/LOGVS/FORMAT-LOG
; like functions and macros. eg:
;
;    (def-log-env :my-log (:logv-macro-name my-logv
;                          :logvs-macro-name my-logvs
;                          :format-log-function-name my-format-log)
;       :log-output "/path/to/log.txt" ; optional, defaults to t
;       :log-prefix-string-factory #<function (lambda ()...>)> ; optional
;    )
;
; you can then set the log settings of your new log enviroment by adding another arg
; to LOG-SETTING:
;
;    (setf (log-setting :log-output :my-log) "/path/to/mylog.txt")
;
; license: BSD. See "license.txt"
; 
; todo: -log rotation script
;       -different log levels

(in-package :logv)

; default logv environment

(defvar *default-log-env* :default-logv-env)

; global database of environments

(defvar *log-envs* nil) ;; (env-name . env-plist)

; log environment settings

(defun .verify-log-env-exists (log-env-name)
  (if (assoc log-env-name *log-envs*)
      t
      (error "unknown log environment ~A. known loggers are: ~A" log-env-name (mapcar 'first *log-envs*))))

(defun .get-log-setting (key log-env-name)
  (getf (rest (assoc log-env-name *log-envs*)) key))

(defun log-setting (key &optional (log-env-name *default-log-env*))
  (.verify-log-env-exists log-env-name)
  (.get-log-setting key log-env-name))

(defsetf log-setting (key (log-env-name *default-log-env*)) (new-value)
  (let ((<log-env-name> (gensym "log-env-name-")))
    `(let ((,<log-env-name> ,log-env-name))
       (.verify-log-env-exists ,<log-env-name>)
       (setf (getf (rest (assoc ,<log-env-name> *log-envs*))
		   ,key)
	     ,new-value))))

; writing to the log file

(defun .write-string-to-log (log-env-name string)
  "
writes `STRING' to the LOG-ENV-NAME's log. exact behavior depends
on value of :LOG-OUTPUT log setting
"
  (let ((log-output (log-setting :log-output log-env-name)))
    (when log-output
      (let ((string (concatenate 'string
				 (funcall (log-setting :log-prefix-string-factory log-env-name))
				 string))
	    (log-mutex (log-setting :log-mutex log-env-name)))
	(with-lock-held (log-mutex)
	  (if (or (stringp log-output)
		  (pathnamep log-output))
	      (with-open-file (out log-output
				   :element-type '(unsigned-byte 8)
				   :direction :output
				   :if-exists :append
				   :if-does-not-exist :create)
		(trivial-utf-8:write-utf-8-bytes string out))
	      (write-string string (if (eql log-output t)
				       *standard-output*
				       log-output)))))))
  (values))

; creating new loggers

(defvar *.default-prefix-string-factory* 
  (lambda ()
    (format nil
	    "~%; ~A [~A]: "
	    (rw-ut:write-time-string (get-universal-time)
				     "[YYYY/MM/DD] [hh:mm:ss]")
	    (thread-name (current-thread)))))

(defmacro def-log-env (env-name (&key logv-macro-name
				      logvs-macro-name
				      format-log-function-name)
		       &key (log-output t)
;; if :LOG-OUTPUT is
;;  -T: then things that write to the log write to *STANDARD-OUTPUT*
;;  -NIL: then things that normally write to the log don't do anything
;;  -a stream: then things that write to the log write to the stream
;;  -a pathname or a string [a pathname designator]: then things that write to the log
;;   write to the file pointed to by the string/pathname. the file is created if it
;;   doesn't already exist. if there are special characters then they are encoded in
;;   UTF-8

		       (log-prefix-string-factory '*.default-prefix-string-factory*))
									      
;; :LOG-PREFIX-STRING-FACTORY is a thunk that creates the string prepended to each log message
  `(progn

     (setf *log-envs*
	   (cons (list ',env-name
		       :log-output ,log-output
		       :log-prefix-string-factory ,log-prefix-string-factory
		       :log-mutex (make-lock (format nil "~A log mutex" ',env-name)))
;; __LOG_MUTEX__ prevents multiple threads from writing to LOG-OUTPUT at the same time
		 (remove ',env-name *log-envs* :key 'first)))

     ,@(when logv-macro-name
	 `((defmacro ,logv-macro-name (form &rest more-forms)
	     (if more-forms
		 `(progn (logv ,form) (logv ,@more-forms))
		 (let ((% (gensym "value")))
		   `(let ((,% ,form))
		      (.write-string-to-log ',',env-name (format nil "~S -> ~S" ',form ,%))
		      ,%))))))

    ,@(when logvs-macro-name
        `((defmacro ,logvs-macro-name (form &rest more-forms)
	    (if more-forms
		`(progn (logvs ,form) (logvs ,@more-forms))
		(let ((%l (gensym "values")))
		  `(let ((,%l (multiple-value-list ,form)))
		     (.write-string-to-log ',',env-name (format nil "~S -> values list: ~S" ',form ,%l))
		     (apply 'values ,%l)))))))

    ,@(when format-log-function-name
	    `((defun ,format-log-function-name (fmt-string &rest fmt-args)
		(.write-string-to-log ',env-name (apply 'format nil fmt-string fmt-args)))))))
		
	    
(def-log-env :logv (:logv-macro-name logv
		    :logvs-macro-name logvs
		    :format-log-function-name format-log))