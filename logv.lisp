;
; http://github.com/nallen05/logv
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

(defsetf log-setting (key &optional (log-env-name *default-log-env*)) (new-value)
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
