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
             klvm-if
             klvm-inc-nargs
             klvm-inc-stack-ptr
             klvm-label
             klvm-mk-closure
             klvm-nargs
             klvm-nargs->
             klvm-nargs>0
             klvm-nargs-cond
             klvm-nregs->
             klvm-put-closure-args
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

(defstruct context
  (func symbol)
  (nargs A)
  (nregs number)
  (indent 0))

(define s*
  [X] Acc -> (cn Acc X) where (string? X)
  [X] Acc -> (cn Acc (str X))
  [X | Y] Acc -> (s* Y (cn Acc X)) where (string? X)
  [X | Y] Acc -> (s* Y (cn Acc (str X))))

(define s
  X -> (s* X ""))

(define indent'
  0 S -> S
  Level S -> (let X "    "
               (indent' (- Level 1) (cn X S))))

(define indent
  Level -> (indent' Level "") where (number? Level)
  C -> (indent' (context-indent C) ""))

(define ls
  X C -> (cn (indent C) (s X)))

(define endl -> "c#10;")

(define endl+
  X C -> (do (context-indent-> C (+ X (context-indent C)))
             (endl)))

(define indent-decr
  X C -> (context-indent-> C (- (context-indent C) X)))

(define endl-
  X C -> (do (indent-decr X C)
             (endl)))

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

(define mk-closure
  Func Nargs Ninit ->
  (let R (s [(id "sp") " + 1 : " (id "sp") " + 1 + " Ninit])
       T (id "type_function")
       F (func-name Func)
    (s ["[" T ", " (str F) ", " Nargs ", " (id "stack")
        "[" R "], None]"])))

(define expr-atom
  X -> (str X) where (number? X)
  true -> "True"
  false -> "False"
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
  0 C -> (context-func C)
  X C -> (concat lb (concat X (concat - (context-func C)))))

(define expr-label
  N C -> (func-name (label-sym N C)) where (number? N)
  X _ -> (esc-obj X) where (symbol? X)
  X C -> (expr2 X C))

(define nargs-cond
  X Y Z C ->
  (let N (context-nargs C)
       S (s [(indent C) "if " (id "nargs") " == " N ":" (endl+ 1 C)])
       S (exprs Y C S)
       . (indent-decr 1 C)
       S (s [S (indent C) "elif " (id "nargs") " < " N ":" (endl+ 1 C)])
       S (exprs X C S)
       . (indent-decr 1 C)
       S (s [S (indent C) "else:" (endl+ 1 C)])
       S (exprs Z C S)
       . (indent-decr 1 C)
    S))

(define nargs>0
  X Y C -> (let S (s [(indent C) "if " (id "nargs") " == 0:" (endl+ 1 C)])
                S (exprs Y C S)
                . (indent-decr 1 C)
                S (s [S (indent C) "else:" (endl+ 1 C)])
                S (exprs X C S)
                . (indent-decr 1 C)
             S))

(define push-extra-args
  C -> (let N (context-nargs C)
            S (s [(indent C) "for x in xrange(1, 1 + " (id "nargs") "):"
                  (endl+ 1 C)])
            S (s [S (indent C) (id "stack") "[" (id "sp") " + x + 1] = "
                  (id "reg") "[x + " (str N) "]" (endl- 1 C)])
         S))

(define pop-extra-args
  C -> (let N (context-nargs C)
            S (s [(indent C) "for x in xrange(1, 1 + " (id "nargs") "):"
                  (endl+ 1 C)])
            S (s [S (indent C) (id "reg") "[x + " N "] = " (id "stack")
                  "[" (id "sp") " + x + 1]" (endl- 1 C)])
         S))

(define put-closure-args'
  X C -> (let R (s [(indent C) "a = " X "[3]" (endl)])
              R (s [R (indent C) "for i in xrange(0, len(a)):" (endl+ 1 C)])
           (s [R (indent C) (id "reg") "[i + 1] = a[i]" (endl- 1 C)])))

(define put-closure-args
  [] C -> (put-closure-args' "t" C)
  X C -> (put-closure-args' (expr2 X C) C))

(define sum-expr
  [X] C Acc -> (cn Acc (expr2 X C))
  [0 | Y] C Acc -> (sum-expr Y C Acc)
  [X | Y] C Acc -> (let Acc (make-string "~A~A + " Acc (expr2 X C))
                     (sum-expr Y C Acc)))

(define expr-closure
  X C -> (s [(indent C) "t = " (id "fns") "[" (esc-obj (str X)) "]" (endl)])
         where (symbol? X)
  X C -> (let R (s [(indent C) "t = " (expr2 X C) (endl)])
              R (s [R (indent C) "if " (id "issymbol") "(t):" (endl+ 1 C)])
              R (s [R (indent C) "t = " (id "fns") "[t[1]]" (endl- 1 C)])
           R))

(define expr-if
  If Then Else C -> (let S (s [(indent C) "if " (expr2 If C) ":" (endl+ 1 C)])
                         S (cn S (expr1 Then C))
                         . (indent-decr 1 C)
                         S (s [S (indent C) "else:" (endl+ 1 C)])
                         S (cn S (expr1 Else C))
                         . (indent-decr 1 C)
                      S))

(define expr1
  [klvm-return] C -> (s [(indent C) "return " (id "reg") "[0]" (endl)])
  [klvm-dec-nargs nargs] C -> (s [(indent C) (id "nargs") " -= nargs" (endl)])
  [klvm-inc-nargs X] C -> (s [(indent C) (id "nargs") " += " (expr2 X C)
                              (endl)])
  [klvm-dec-nargs X] C -> (s [(indent C) (id "nargs") " -= " (expr2 X C)
                              (endl)])
  [klvm-stack-size X] C -> (s [(indent C) (id "stack_size") "(" (expr2 X C)
                               ")" (endl)])
  [klvm-nregs-> X] C -> (s [(indent C) (id "reg_size") "(" (sum-expr X C "")
                            ")" (endl)])
  [klvm-stack-> N X] C -> (s [(indent C) (id "stack") "[" (id "sp") " + "
                              (+ N 1) "] = " (expr2 X C) (endl)])
  [klvm-reg-> [0] X] C -> (s [(indent C) (id "reg") "[0] = " (expr-label X C)
                              (endl)])
  [klvm-reg-> X Y] C -> (s [(indent C) (id "reg") "[" (sum-expr X C "") "] = "
                            (expr2 Y C) (endl)])

  [klvm-inc-stack-ptr X] C -> (s [(indent C) (id' "sp") " += " (expr2 X C)
                                  (endl) (indent C) "shen_sp = " (id' "sp")
                                  (endl)])
  [klvm-dec-stack-ptr X] C -> (s [(indent C) (id' "sp") " -= " (expr2 X C)
                                  (endl) (indent C) "shen_sp = " (id' "sp")
                                  (endl)])
  [klvm-nargs-> X] C -> (s [(indent C) (id "nargs") " = " (expr2 X C) (endl)])
  [klvm-goto N] C -> (s [(indent C) "return " (func-name (label-sym N C))
                         (endl)])
  [klvm-call X] C -> (s [(indent C) "return " (id "fns")
                         "[" (esc-obj (str X)) "]" (endl)])
                     where (symbol? X)
  [klvm-call X] C -> (s [(indent C) "return " (expr2 X C) (endl)])
  [klvm-push-error-handler X] C -> (s [(indent C) (id "push_error_handler")
                                        "(" (expr2 X C) ")" (endl)])
  [klvm-pop-error-handler] C -> (s [(indent C) (id "pop_error_handler") "()"
                                    (endl)])
  [klvm-native X] C -> (s [(indent C) X (endl)])
  [klvm-nargs-cond X Y Z] C -> (nargs-cond X Y Z C)
  [klvm-nargs>0 X Y] C -> (nargs>0 X Y C)
  [klvm-push-extra-args [klvm-nargs]] C -> (push-extra-args C)
  [klvm-pop-extra-args [klvm-nargs]] C -> (pop-extra-args C)
\\!!!  [klvm-put-closure-args F] C -> (put-closure-args F C)
  [klvm-put-closure-args] C -> (put-closure-args [] C)
  [klvm-if If Then Else] C -> (expr-if If Then Else C)
  [klvm-closure-> X] C -> (expr-closure X C)
  X C -> (error "Broken KLVM in ~S (expr: ~S)" (context-func C) X))

(define exprs
  [] _ Acc -> Acc
  [X | Y] C Acc -> (exprs Y C (cn Acc (expr1 X C))))

(define func-prelude''
  [] C Acc -> Acc
  [X | Y] C Acc -> (let S (s [Acc (indent C) "shen_" X " = " (id' X) (endl)])
                     (func-prelude'' Y C S)))

(define func-prelude'
  C -> (func-prelude'' ["stack" "reg" "fns" "vars" "sp"] C ""))

(define func-prelude
  C -> (func-prelude' C) where (not (value *same-namespace*))
  C -> (s [(indent C) "global nargs, sp, error_obj" (endl)
           (func-prelude' C)]))

(define func-hdr
  Name C -> (let FN (func-name Name)
              (s ["global " FN (endl) (indent C) "def " FN "():" (endl+ 1 C)
                  (func-prelude C)])))

(define label
  N Code C -> (let R (func-hdr (label-sym N C) C)
                   R (cn R (exprs Code C ""))
                   . (indent-decr 1 C)
                R))

(define template-label
  [[[klvm-label 0] | X]] C -> (exprs X C ""))

(define template
  -> (let C (mk-context this func_nargs 0 0)
          Name "mkfun"
          T (kl-imp-template-func-body [klvm-native "func_nargs"] proc)
       (s ["def " Name "(" (func-name (context-func C)) ", func_nargs, proc):"
           (endl+ 1 C) (func-prelude C) (template-label T C) (endl)
           (id "mkfun") " = " Name (endl) (endl)])))

(define labels
  [] _ Acc -> Acc
  [[[klvm-label N] | X] | Y] C Acc -> (let R (s [Acc (label N X C) (endl)])
                                        (labels Y C R)))

(define mkfunc
  Name Args Nregs Code -> (let Nargs (length Args)
                               C (mk-context Name Nargs Nregs 0)
                               R (labels Code C "")
                               . (indent-decr 1 C)
                            R))

(define def-func'
  Name Args -> (s [(indent 0) (id "defun_x") "(" (esc-obj (str Name)) ", "
                   (length Args) ", " (func-name Name) ")" (endl) (endl)]))

(define def-func
  Name Args -> (def-func' Name Args) where (not (value *in-repl*))
  Name Args -> (s [(indent 0) (id "ret") " = " (def-func' Name Args)]))

(define call-toplevel
  Name -> (s [(indent 0) (id "nargs") " = 0" (endl) (indent 0)
               (id "ret") " = " (func-name Name) "()" (endl)])
          where (value *in-repl*)
  Name -> (s [(indent 0) (id "call") "(" (func-name Name) ")" (endl)]))

(define toplevel
  Name _ Nregs Code -> (let X (mkfunc Name [] Nregs Code)
                            F (func-name Name)
                            TL (call-toplevel Name)
                         (s [(indent 0) X TL "del " F (endl) (endl)])))

(define py-from-kl-toplevel
  X <- (do (output "KL: ~S~%" X) (fail)) where false
  X <- (do (kl-imp-show-code [X]) (fail)) where false
  [klvm-closure Name Args Nregs Code] -> (mkfunc Name Args Nregs Code)
  [klvm-func Name Args Nregs Code] -> (cn (mkfunc Name Args Nregs Code)
                                          (def-func Name Args))
  [klvm-toplevel Name Args Nregs Code] -> (toplevel Name Args Nregs Code)
  [klvm-nargs-> [0]] -> (s [(id' "nargs") " = 0" (endl)])
  [klvm-call X] -> (s [(indent 0) (id "call") "(" (func-name X) ")" (endl)
                       (endl)])
  _ -> "" where (not (value *in-repl*))
  X -> (s [(id' "reg") "[1] = " (expr-atom X) (endl)
           (id "ret") " = " (id' "reg") "[0]" (endl)]))

(define py-from-kl-aux
  [] Acc -> Acc
  [X | Y] Acc -> (py-from-kl-aux Y (cn Acc (py-from-kl-toplevel X))))

(define py-from-kl
  X -> (py-from-kl-aux (klvm-from-kl (function primitives) X) ""))

(define dump-to-file
  Code To -> (backend-utils.write-file
              (s [(indent 0) "import shenpy" (endl)  Code (endl)]) To))

(define py-from-kl-expr
  X -> (py-from-kl [X]))

(define dump-to-stream
  Src F -> (let X (read-file Src)
             (backend-utils.map-shen (/. X (py-from-kl [X])) X F)))

(define py.dump
  Srcdir F Dstdir -> (let D (make-string "~A~A.py" Dstdir F)
                          S (make-string "~A~A" Srcdir F)
                          . (if (value *hush*)
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
