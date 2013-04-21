(package py [py-from-kl klvm-from-kl defstruct py.dump
             kl-imp-template-func-body
             register-dumper kl-from-shen
             kl-imp-show-code
             backend-utils.map-shen
             backend-utils.translate-to-file
             backend-utils.write-file
             backend-utils.with-file-output

             shenpy- nargs proc this klvm- lbl shenpy_func all python
             func_nargs

             klvm-call
             klvm-closure->
             klvm-closure-func
             klvm-closure-nargs
             klvm-current-error
             klvm-dec-nargs
             klvm-dec-stack-ptr
             klvm-error-unwind-get-handler
             klvm-func-obj
             klvm-goto
             klvm-inc-nargs
             klvm-inc-stack-ptr
             klvm-label
             klvm-mk-closure
             klvm-nargs
             klvm-nargs->
             klvm-nargs>0
             klvm-nargs-cond
             klvm-nregs->
             klvm-pop-closure-args
             klvm-pop-error-handler
             klvm-pop-extra-args
             klvm-push-error-handler
             klvm-push-extra-args
             klvm-reg
             klvm-reg->
             klvm-return
             klvm-stack
             klvm-stack->
             klvm-stack-size
             klvm-thaw
             klvm-closure
             klvm-func
             klvm-toplevel
             ]

(define s*
  [X] Acc -> (cn Acc X) where (string? X)
  [X] Acc -> (cn Acc (str X))
  [X | Y] Acc -> (s* Y (cn Acc X)) where (string? X)
  [X | Y] Acc -> (s* Y (cn Acc (str X))))

(define s
  X -> (s* X ""))

(define str-py-from-shen*
  "" Acc -> Acc
  (@s "-" S) Acc -> (str-py-from-shen* S (cn Acc "_"))
  (@s "_" S) Acc -> (str-py-from-shen* S (cn Acc "___"))
  (@s "$" S) Acc -> (str-py-from-shen* S (cn Acc "__s__"))
  (@s "'" S) Acc -> (str-py-from-shen* S (cn Acc "__quote_"))
  (@s "`" S) Acc -> (str-py-from-shen* S (cn Acc "__bquote_"))
  (@s "/" S) Acc -> (str-py-from-shen* S (cn Acc "__slash_"))
  (@s "*" S) Acc -> (str-py-from-shen* S (cn Acc "__asterisk_"))
  (@s "+" S) Acc -> (str-py-from-shen* S (cn Acc "__plus_"))
  (@s "%" S) Acc -> (str-py-from-shen* S (cn Acc "__percent_"))
  (@s "=" S) Acc -> (str-py-from-shen* S (cn Acc "__eq_"))
  (@s "?" S) Acc -> (str-py-from-shen* S (cn Acc "__question_"))
  (@s "!" S) Acc -> (str-py-from-shen* S (cn Acc "__excl_"))
  (@s ">" S) Acc -> (str-py-from-shen* S (cn Acc "__gt_"))
  (@s "<" S) Acc -> (str-py-from-shen* S (cn Acc "__lt_"))
  (@s "." S) Acc -> (str-py-from-shen* S (cn Acc "__dot_"))
  (@s "|" S) Acc -> (str-py-from-shen* S (cn Acc "__bar_"))
  (@s "#" S) Acc -> (str-py-from-shen* S (cn Acc "__sharp_"))
  (@s "~" S) Acc -> (str-py-from-shen* S (cn Acc "__tilde_"))
  (@s ":" S) Acc -> (str-py-from-shen* S (cn Acc "__colon_"))
  (@s ";" S) Acc -> (str-py-from-shen* S (cn Acc "__sc_"))
  (@s "@" S) Acc -> (str-py-from-shen* S (cn Acc "__at_"))
  (@s "&" S) Acc -> (str-py-from-shen* S (cn Acc "__amp_"))
  (@s "{" S) Acc -> (str-py-from-shen* S (cn Acc "__cbraceopen_"))
  (@s "}" S) Acc -> (str-py-from-shen* S (cn Acc "__cbraceclose_"))
  (@s C S) Acc -> (str-py-from-shen* S (cn Acc C)))

(define str-py-from-shen
  X -> (str-py-from-shen* X ""))

(define sym-py-from-shen
  X -> (intern (str-py-from-shen (str X))))

(define endl -> "c#10;")
(set backslash (n->string 92))
(set dquote (n->string 34))

(define esc-string
  "" Acc -> Acc
  (@s C S) Acc -> (let P (value backslash)
                    (esc-string S (@s Acc P C)))
                  where (or (= C (value backslash))
                            (= C (value dquote)))
  (@s C S) Acc -> (esc-string S (cn Acc "\x0a"))
                  where (= (string->n C) 10)
  (@s C S) Acc -> (esc-string S (cn Acc "\x0d"))
                  where (= (string->n C) 13)
  (@s C S) Acc -> (esc-string S (cn Acc C)))

(set *same-namespace* false)
(set *in-repl* true)

(define id'
  X -> (id' (str X)) where (symbol? X)
  X -> (cn "shen." X) where (not (value *same-namespace*))
  X -> X)

(define id
  "sp" -> "shen_sp"
  "stack" -> "shen_stack"
  "reg" -> "shen_reg"
  "fns" -> "shen_fns"
  "vars" -> "shen_vars"
  X -> (id' X))

(define func-name
  X -> (sym-py-from-shen (concat klvm- X)))

(define esc-obj
  X -> (s [(value dquote) (esc-string X "") (value dquote)]) where (string? X)
  X -> (sym-py-from-shen X) where (symbol? X)
  [proc] -> "proc()" \* for translation template code *\
  X -> (error "Object ~S cannot be escaped" X))

(set *indent* "    ")

(define indent
  0 S -> S
  Level S -> (indent (- Level 1) (cn (value *indent*) S)))

(defstruct context
  (func symbol)
  (nargs A)
  (nregs number))

(define mk-closure
  Func Nargs Ninit ->
  (let R (s [(id "sp") " + 1 : " (id "sp") " + 1 + " Ninit])
       T (id "type_function")
       F (func-name Func)
    (s ["[" T ", " (str F) ", " Nargs ", " (id "stack")
        "[" R "], None]"])))

(define expr-atom
  true -> "True"
  false -> "False"
  X -> (str X) where (number? X)
  X -> (esc-obj X) where (string? X)
  X -> (s ["[" (id "type_symbol") ", " (esc-obj (str X)) "]"])
       where (symbol? X)
  [] -> "[]"
  X -> (esc-obj X))

(define expr2
  [klvm-closure-nargs] _ -> "len(t[3])"
  [klvm-closure-func] _ -> "t[1]"
  [klvm-func-obj] C -> (s ["[" (id "type_function") ", "
                           (func-name (context-func C)) ", "
                           (context-nargs C) ", "
                           (id "reg") "[1 : 1 + " (id "nargs") "], None]"])
  [klvm-reg N] _ -> (s [(id "reg") "[" N "]"])
  [klvm-stack N] _ -> (s [(id "stack") "[" (id "sp") " + " (+ N 1) "]"])
  [klvm-nargs] _ -> (id "nargs")
  [klvm-null-label] _ -> "None"
  [klvm-mk-closure Func Nregs Ninit] _ -> (mk-closure Func Nregs Ninit)
  [klvm-error-unwind-get-handler] _ -> (s [(id "error_unwind_get_handler")
                                           "()"])
  [klvm-current-error] _ -> (id "error_obj")
  [klvm-native X] _ -> X
  [fail] _ -> (id "type_fail")
  X _ -> (expr-atom X))

(define label-sym
  X C -> (concat lbl (concat X (concat - (context-func C)))))

(define expr-label
  N C -> (func-name (label-sym N C)) where (number? N)
  X _ -> (esc-obj X) where (symbol? X)
  X C -> (expr2 X C))

(define nargs-cond
  X Y Z C ->
  (let N (context-nargs C)
       S (indent 1 (s ["if " (id "nargs") " == " N ":" (endl)]))
       S (exprs 2 Y C S)
       S (cn S (indent 1 (s ["elif " (id "nargs") " < " N ":" (endl)])))
       S (exprs 2 X C S)
       S (cn S (indent 1 (s ["else:" (endl)])))
       S (exprs 2 Z C S)
    S))

(define nargs>0
  X Y C -> (let S (indent 1 (s ["if " (id "nargs") " == 0:" (endl)]))
                S (exprs 2 Y C S)
                S (cn S (indent 1 (s ["else:" (endl)])))
                S (exprs 2 X C S)
             S))

(define push-extra-args
  L C -> (let N (context-nargs C)
              S (s ["for x in xrange(1, 1 + " (id "nargs") "):" (endl)])
              X (s [(id "stack") "[" (id "sp") " + x + 1] = " (id "reg")
                    "[x + " (str N) "]" (endl)])
           (indent L (cn S (indent (+ L 1) X)))))

(define pop-extra-args
  L C -> (let N (context-nargs C)
              S (s ["for x in xrange(1, 1 + " (id "nargs") "):" (endl)])
              X (s [(id "reg") "[x + " N "] = " (id "stack") "[" (id "sp")
                    " + x + 1]" (endl)])
           (indent L (cn S (indent (+ L 1) X)))))

(define pop-closure-args'
  L X -> (let R (indent L (make-string "a = ~A[3]~%" X))
              R (cn R (indent L (s ["for i in xrange(0, len(a)):" (endl)])))
              L' (+ L 1)
           (cn R (indent L' (s [(id "reg") "[i + 1] = a[i]" (endl)])))))

(define pop-closure-args
  L [] C -> (pop-closure-args' L "t")
  L X C -> (pop-closure-args' L (expr2 X C)))

(define sum-expr
  [X] C Acc -> (cn Acc (expr2 X C))
  [0 | Y] C Acc -> (sum-expr Y C Acc)
  [X | Y] C Acc -> (let Acc (make-string "~A~A + " Acc (expr2 X C))
                     (sum-expr Y C Acc)))

(define expr-closure
  L X C -> (indent L (s ["t = " (id "fns") "[" (esc-obj (str X)) "]" (endl)]))
           where (symbol? X)
  L X C -> (let L1 (indent L (make-string "t = ~A~%" (expr2 X C)))
                L2 (indent L (s ["if " (id "issymbol") "(t):" (endl)]))
                L3 (indent (+ L 1) (s ["t = " (id "fns") "[t[1]]" (endl)]))
             (cn L1 (cn L2 L3))))

(define expr1
  [klvm-return] _ -> (s ["return " (id "reg") "[0]" (endl)])
  [klvm-dec-nargs nargs] C -> (s [(id "nargs") " -= nargs" (endl)])
  [klvm-inc-nargs X] C -> (s [(id "nargs") " += " (expr2 X C) (endl)])
  [klvm-dec-nargs X] C -> (s [(id "nargs") " -= " (expr2 X C) (endl)])
  [klvm-stack-size X] C -> (s [(id "stack_size") "(" (expr2 X C) ")" (endl)])
  [klvm-nregs-> X] C -> (s [(id "reg_size") "(" (sum-expr X C "") ")" (endl)])
  [klvm-stack-> N X] C -> (s [(id "stack") "[" (id "sp") " + " (+ N 1) "] = "
                              (expr2 X C) (endl)])
  [klvm-reg-> [0] X] C -> (s [(id "reg") "[0] = " (expr-label X C) (endl)])
  [klvm-reg-> X Y] C -> (s [(id "reg") "[" (sum-expr X C "") "] = "
                            (expr2 Y C) (endl)])

  [klvm-inc-stack-ptr X] C -> (s [(id' "sp") " += " (expr2 X C)
                                  "; shen_sp = " (id' "sp") (endl)])
  [klvm-dec-stack-ptr X] C -> (s [(id' "sp") " -= " (expr2 X C)
                                  "; shen_sp = " (id' "sp") (endl)])
  [klvm-nargs-> X] C -> (s [(id "nargs") " = " (expr2 X C) (endl)])
  [klvm-goto N] C -> (s ["return " (func-name (label-sym N C)) (endl)])
  [klvm-call X] _ -> (s ["return " (id "fns") "[" (esc-obj (str X)) "]"
                         (endl)])
                     where (symbol? X)
  [klvm-call X] C -> (s ["return " (expr2 X C) (endl)])
  [klvm-push-error-handler X] C -> (s [(id "push_error_handler")
                                       "(" (expr2 X C) ")" (endl)])
  [klvm-pop-error-handler] _ -> (s [(id "pop_error_handler") "()" (endl)])
  [klvm-native X] _ -> (s [X (endl)])
  X C -> (error "Broken KLVM in ~S (expr: ~S)" (context-func C) X))

(define expr
  _ [klvm-nargs-cond X Y Z] C -> (nargs-cond X Y Z C)
  _ [klvm-nargs>0 X Y] C -> (nargs>0 X Y C)
  L [klvm-push-extra-args [klvm-nargs]] C -> (push-extra-args L C)
  L [klvm-pop-extra-args [klvm-nargs]] C -> (pop-extra-args L C)
  L [klvm-pop-closure-args F] C -> (pop-closure-args L F C)
  L [klvm-pop-closure-args] C -> (pop-closure-args L [] C)
  L [if If Then Else] C -> (let X (make-string "if ~A:~%" (expr2 If C))
                                S (indent L X)
                                S (cn S (indent (+ L 1) (expr1 Then C)))
                                S (cn S (indent L (make-string "else:~%")))
                                S (cn S (indent (+ L 1) (expr1 Else C)))
                             S)
  L [klvm-closure-> X] C -> (expr-closure L X C)
  L X C -> (indent L (expr1 X C)))

(define exprs
  L [] _ Acc -> Acc
  L [X | Y] C Acc -> (exprs L Y C (cn Acc (expr L X C))))

(define func-prelude''
  [] Acc -> Acc
  [X | Y] Acc -> (let S (s ["shen_" X " = " (id' X) (endl)])
                   (func-prelude'' Y (cn Acc (indent 1 S)))))

(define func-prelude'
  -> (func-prelude'' ["stack" "reg" "fns" "vars" "sp"] ""))

(define func-prelude
  -> (func-prelude') where (not (value *same-namespace*))
  -> (indent 1 (s ["global nargs, sp, error_obj" (endl) (func-prelude')])))

(define func-hdr
  Name -> (let FN (func-name Name)
            (s ["global " FN (endl) "def " FN "():" (endl) (func-prelude)])))

(define label
  0 Code C -> (cn (func-hdr (context-func C)) (exprs 1 Code C ""))
  N Code C -> (cn (func-hdr (label-sym N C)) (exprs 1 Code C "")))

(define label
  N Code C -> (cn (func-hdr (label-sym N C)) (exprs 1 Code C "")))

(define template-label
  [[[klvm-label 0] | X]] C -> (exprs 1 X C ""))

(define template
  -> (let C (mk-context this func_nargs 0)
          Name "mkfun"
          T (kl-imp-template-func-body [klvm-native "func_nargs"] proc)
          X (template-label T C)
       (s ["def " Name "(" (func-name (context-func C)) ", func_nargs, proc):"
           (endl) (func-prelude) (endl) X (endl)
           (id "mkfun") " = " Name (endl) (endl)])))

(define labels
  [] _ Acc -> Acc
  [[[klvm-label N] | X] | Y] C Acc -> (let Acc (s [Acc (label N X C) (endl)])
                                        (labels Y C Acc)))

(define mkfunc
  Name Args Nregs Code -> (let Nargs (length Args)
                               C (mk-context Name Nargs Nregs)
                               R (func-hdr Name)
                               R (labels Code C R)
                            R))

(define def-func'
  Name Args -> (s [(id "defun_x") "(" (esc-obj (str Name)) ", " (length Args)
                   ", " (func-name Name) ")" (endl) (endl)]))

(define def-func
  Name Args -> (def-func' Name Args) where (not (value *in-repl*))
  Name Args -> (s [(id "ret") " = " (def-func' Name Args)]))

(define call-toplevel
  Name -> (s [(id "nargs") " = 0" (endl)
              (id "ret") " = " (func-name Name) "()" (endl)])
          where (value *in-repl*)
  Name -> (s [(id "call") "(" (func-name Name) ")" (endl)]))

(define toplevel
  Name _ Nregs Code -> (let X (mkfunc Name [] Nregs Code)
                            F (func-name Name)
                         (s [X (call-toplevel Name) "del " F (endl) (endl)])))

(define py-from-kl-toplevel
  X <- (do (output "KL: ~S~%" X) (fail)) where false
  X <- (do (kl-imp-show-code [X]) (fail)) where false
  [klvm-closure Name Args Nregs Code] -> (mkfunc Name Args Nregs Code)
  [klvm-func Name Args Nregs Code] -> (cn (mkfunc Name Args Nregs Code)
                                          (def-func Name Args))
  [klvm-toplevel Name Args Nregs Code] -> (toplevel Name Args Nregs Code)
  [klvm-nargs-> [0]] -> (s [(id' "nargs") " = 0" (endl)])
  [klvm-call X] -> (s [(id "call") "(" (func-name X) ")" (endl) (endl)])
  _ -> "" where (not (value *in-repl*))
  X -> (expr-atom X))

(define py-from-kl-aux
  [] Acc -> Acc
  [X | Y] Acc -> (py-from-kl-aux Y (cn Acc (py-from-kl-toplevel X))))

(define py-from-kl
  X -> (py-from-kl-aux (klvm-from-kl (function primitives) X) ""))

(set skip-internals false)

(define dump-to-file
  Code To -> (backend-utils.write-file
              (s ["import shenpy" (endl)  Code (endl)]) To))

(define py-from-kl-expr
  X -> (py-from-kl [X]))

(set *silence* false)

(define dump-to-stream
  Src F -> (let X (read-file Src)
             (backend-utils.map-shen (/. X (py-from-kl [X])) X F)))

(define py.dump
  Srcdir F Dstdir -> (let D (make-string "~A~A.py" Dstdir F)
                          S (make-string "~A~A" Srcdir F)
                          . (if (value *silence*)
                                _
                                (output "== ~A -> ~A~%" S D))
                       (backend-utils.with-file-output
                         D
                         (/. F (do (s ["import shenpy" (endl) (endl)])
                                   (dump-to-stream S F))))))

(declare py.dump [string --> [string --> [string --> boolean]]])
(declare dump-to-file [string --> [string --> boolean]])

\* Register function py.dump as a dumper in Modulesys for all implementations
   of python language. Do nothing if Modulesys is not loaded *\

(if (trap-error (do (register-dumper) true) (/. _ false))
    (register-dumper python all py.dump)
    _)
)
