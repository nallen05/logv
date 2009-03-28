# Logv

_Logv_ is a cognitively lightweight logging utility for Common Lisp
 
The most useful form is `LOGV`, a macro that behaves like `PROGN` except that it
writes each form and its return value to the log.
 
The following code:
 
    (logv (+ 1 (logv (+ 1 1)))
          (+ 10 10)
          (* 64 64))
 
will return `4096` and might write something like the following lines to the log:
 
    ; [2009/03/27] [19:15:08] [repl-thread]: (+ 1 1) -> 2
    ; [2009/03/27] [19:15:08] [repl-thread]: (+ 1 (LOGV (+ 1 1))) -> 3
    ; [2009/03/27] [19:15:08] [repl-thread]: (+ 10 10) -> 20
    ; [2009/03/27] [19:15:08] [repl-thread]: (* 64 64) -> 4096
 
`LOGVS` is like `LOGV` but understands multiple return values. `FORMAT-LOG` lets you
write to the log with a format string and arguments (like `FORMAT`).
 
The behavior of `LOGV` and `LOGVS` is controlled by "log settings". Log settings can
be changed or accessed with `SETF`-able function `LOG-SETTING`. for example the
following code:
 
    (setf (log-setting :log-output) "/path/to/log.txt")
 
makes `LOGV`, `LOGVS`, and `FORMAT-LOG` write to the log file "log.txt"
 
If the `:LOG-OUTPUT` setting is:
 
* `T`
  then things that write to the log write to `*STANDARD-OUTPUT*`
* `NIL`
  then things that normally write to the log don't do anything
* a _stream_
  then things that write to the log write to the stream
* a _pathname designator_ (a _pathname_ or a _string_): then things that write to the
  log write to the file pointed to by the string/pathname. the file is created if
  it doesn't already exist. if there are special characters they are encoded in
  UTF-8
 
The `:LOG-PREFIX-STRING-FACTORY` setting is a thunk [unary function] that returns a
string to be prepended to the log line.
 
The macro `DEF-LOG-ENV` enables you to easily roll your own `LOGV`/`LOGVS`/`FORMAT-LOG`
like functions and macros. eg:
 
    (def-log-env :my-log (:logv-macro-name my-logv
                          :logvs-macro-name my-logvs
                          :format-log-function-name my-format-log)
       :log-output "/path/to/log.txt"                         ; optional, defaults to t
       :log-prefix-string-factory #<function (lambda ()...>)> ; optional
    )
 
You can then set the log settings of your new log enviroment by adding another arg
to `LOG-SETTING`:
 
    (setf (log-setting :log-output :my-log) "/path/to/mylog.txt")
 
## License

BSD. See "license.txt"
