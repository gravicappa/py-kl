(package py [py-from-kl klvm.s2-from-kl defstruct py.dump
             register-dumper kl-from-shen

             klvm.dbg.show-code klvm.entry-template klvm.return-template
             klvm.native klvm.lambda
              
             klvm.call klvm.closure klvm.closure-> klvm.closure-nargs
             klvm.entry klvm.func klvm.func-obj klvm.goto klvm.goto-next
             klvm.if klvm.if-nargs>0 klvm.nargs klvm.nargs- klvm.nargs->
             klvm.nargs+ klvm.nargs-cond klvm.next klvm.next-> klvm.nregs->
             klvm.pop-error-handler klvm.push-error-handler
             klvm.put-closure-args klvm.reg klvm.reg-> klvm.ret klvm.ret->
             klvm.return klvm.s2.runtime klvm.sp+ klvm.sp- klvm.tailcall
             klvm.tailif klvm.thaw klvm.toplevel klvm.wipe

             backend-utils.map-shen
             backend-utils.translate-to-file
             backend-utils.write-file
             backend-utils.with-file-output
             
             shenpy- nargs proc this klvm- lbl shenpy_func all python
             func_nargs]

(defstruct context
  (func symbol)
  (nargs A)
  (nregs number)
  (indent 0)
  (toplevel string)
  (inline (list symbol)))

\* Can contain entry, return items *\
(set inline [])

(define s'
  [] Acc -> Acc
  [X] Acc -> (cn Acc X) where (string? X)
  [X] Acc -> (cn Acc (str X))
  [X | Y] Acc -> (s' Y (cn Acc X)) where (string? X)
  [X | Y] Acc -> (s' Y (cn Acc (str X))))

(define s
  X -> (s' X ""))

(define join'
  [] _ Acc -> Acc
  [X] _ Acc -> (s [Acc X])
  [X | Xs] S Acc -> (join' Xs S (s [Acc X S])))

(define join
  [] _ -> ""
  X S -> (join' X S ""))

(define endl -> "c#10;")

(define py-indent
  S 0 -> S
  S I -> (py-indent (cn S "    ") (- I 1)))

(define py-expr
  X I S true -> (cn (py-expr X I S false) (endl))
  X I S false -> (cn (py-indent S I) (s X)))

(define py'
  [] I S -> S
  [[--> | X] | Y] I S -> (py' Y I (py' X (+ I 1) S))
  [[X | Xs] | Y] I S -> (py' Y I (py-expr [X | Xs] I S true))
  [[] | Y] I S -> (py' Y I (py-expr [] I S true))
  [X | Y] I S -> (py' Y I (py-expr [X] 0 S false)))

(define py
  I X -> (py' X I "") where (number? I)
  C X -> (py' X (context-indent C) "") where (context? C))

(define @
  X Acc -> (append (reverse X) Acc))

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

(set same-namespace false)
(set in-repl true)

(define id'
  X -> (id' (str X)) where (symbol? X)
  X -> (cn "shen." X) where (not (value same-namespace))
  X -> X)

(define id
  "sp" -> "shen_sp"
  "reg" -> "shen_reg"
  "fns" -> "shen_fns"
  "globvars" -> "shen_globvars"
  X -> (id' X))

(define func-name-str
  [klvm.native X] -> X where (symbol? X)
  [klvm.native X] -> (intern X) where (string? X)
  X -> (esc-obj (str (sym-py-from-shen (concat klvm- X)))))

(define func-name
  [klvm.native X] -> X
  X -> (sym-py-from-shen (concat klvm- X)))

(define native
  [klvm.native X] -> X
  X -> X)

(define esc-obj
  X -> (s [(value dquote) (esc-string X "") (value dquote)]) where (string? X)
  X -> (sym-py-from-shen X) where (symbol? X)
  X -> (error "Object ~S cannot be escaped" X))

(define expr-obj
  X -> (str X) where (number? X)
  true -> "True"
  false -> "False"
  X -> (esc-obj X) where (string? X)
  X -> (s [(id "intern") "(" (esc-obj (str X)) ")"]) where (symbol? X)
  [] -> "()"
  X -> (esc-obj X))

(define ensure-str
  [klvm.native X] -> X
  X -> (esc-obj X) where (string? X)
  X -> (esc-obj (str X)) where (symbol? X))

(define func-obj-name
  [] -> "None"
  Name -> (ensure-str Name))

(define func-obj
  Func Nargs Name -> (s ["(" (id "type_function") ", "
                         (func-obj-name Name) ", "
                         (func-name Func) ", "
                         (native Nargs) ", "
                         "" (id "reg") "[" (id "sp") " : "
                         (id "sp") " + " (id "nargs") "])"]))

(define closure-obj
  Func Nargs -> (s ["(" (id "type_function") ", None, " (func-name Func) ", "
                    (native Nargs) ", [])"]))

(define closure-name
  Name -> (intern (cn (str Name) "_obj")))

(define expr2
  [klvm.closure-nargs] -> "len(closure[4])"
  [klvm.func-obj Func Nargs Name] -> (func-obj Func Nargs Name)
  [klvm.reg 0] -> (s [(id "reg") "[" (id "sp") "]"])
  [klvm.reg N] -> (s [(id "reg") "[" (id "sp") " + " N "]"])
  [klvm.nargs] -> (id "nargs")
  [klvm.next] -> (id "next")
  [klvm.ret] -> (id "ret")
  [klvm.native X] -> X
  [klvm.lambda X] -> (esc-obj (closure-name X))
  [fail] -> (id "fail_obj")
  [X | Y] -> (error "Unexpected L2 expression ~S" [X | Y])
  X -> (expr-obj X))

(define label-sym
  0 C -> (context-func C)
  X C -> (concat lb (concat X (concat - (context-func C)))))

(define expr-label
  N C -> (func-name (label-sym N C)) where (number? N)
  X _ -> (esc-obj X) where (symbol? X)
  X C -> (expr2 X))

(define sum-expr2'
  [X] Acc -> (cn Acc (expr2 X))
  [0 | Xs] Acc -> (sum-expr2' Xs Acc)
  [X | Xs] Acc -> (let Acc (make-string "~A~A + " Acc (expr2 X))
                    (sum-expr2' Xs Acc))
  X Acc -> (cn Acc (expr2 X)))

(define sum-expr2
  X -> (sum-expr2' X ""))

(define expr-if
  If Then Else C Acc -> (@ [["if " (expr2 If) ":"]
                            [--> | (expr1 Then C [])]
                            ["else:"]
                            [--> | (expr1 Else C [])]]
                           Acc))

(define unwind-protect
  Thunk Restore -> (trap-error (let R (thaw Thunk)
                                    . (thaw Restore)
                                 R)
                               (/. E (do (thaw Restore)
                                         (error (error-to-string E))))))

(define with-global
  Var Value Thunk -> (let Prev (value Var)
                       (unwind-protect (freeze (do (set Var Value)
                                                   (thaw Thunk)))
                         (freeze (set Var Prev)))))

(define entry-tpl'
  -> (let Klvm (klvm.entry-template [klvm.native "func"]
                                    [klvm.native "func_arity"]
                                    [klvm.native "func_name"])
          \\. (output "KLVM: ~S~%" Klvm)
          C (mk-context [klvm.native "name"] func_arity 0 0 "" [entry])
       (py 0 [["def fn_entry(func, func_arity, func_name):"]
              [--> | (func-prelude ["sp"])]
              [--> | (exprs [Klvm] C [])]
              [--> ["return " (id "fail_obj")]]])))

(define return-tpl'
  -> (let Klvm (klvm.return-template [klvm.native "retval"]
                                     [klvm.native "retnext"])
          \\. (output "KLVM: ~S~%" Klvm)
       C (mk-context
          [klvm.native "name"] func_nargs 0 0 "" [return])
       (py 0 [["def fn_return(retval, retnext):"]
              [--> | (func-prelude ["sp"])]
              [--> | (exprs [Klvm] C [])]])))

(define entry-tpl
  Same-ns? -> (with-global same-namespace Same-ns? (freeze (entry-tpl'))))

(define return-tpl
  Same-ns? -> (with-global same-namespace Same-ns? (freeze (return-tpl'))))

(define func-entry
  F Nargs Name C Acc -> (@ [["x = " (id "fn_entry") "(" (func-name F)
                             ", " Nargs ", " (func-obj-name Name) ")"]
                            ["if x != " (id "fail_obj") ": return x"]
                            [(id "sp") " = " (id' "sp")]]
                           Acc))

(define func-return
  X Next C Acc -> [["return " (id "fn_return") "(" X ", "
                    (id "reg") "[" (id "sp") " + " Next "])"] | Acc])

(define nargs-cond
  Arity L E G C Acc -> (let P (expr2 Arity)
                         (@ [["if " (id "nargs") " == " P ":"]
                             [--> | (exprs E C [])]
                             ["elif " (id "nargs") " < " P ":"]
                             [--> | (exprs L C [])]
                             ["else:"]
                             [--> | (exprs G C [])]]
                            Acc)))

(define if-nargs>0
  Then Else C Acc -> (@ [["if " (id "nargs") " > 0:"]
                         [--> | (exprs Then C [])]
                         ["else:"]
                         [--> | (exprs Else C [])]]
                        Acc))

(define push-error-handler
  X C Acc -> [[(id "push_error_handler") "(" (expr2 X) ")"] | Acc])

(define put-closure-args
  C Acc -> (@ [["a = closure[4]"]
               ["n = len(a)"]
               ["if n > 0:"]
               [--> ["i = " (id "sp") " + " (id "nargs")]
                    [(id "reg_size") "(" (id "nargs") " + n)"]
                    [(id "reg") "[i:i + n] = a"]
                    [(id "nargs") " += n"]]]
                  Acc))

(define next-val
  X C -> (func-name (label-sym X C)) where (number? X)
  X C -> (expr2 X))

(define reg->
  0 Y -> [(id "reg") "[" (id "sp") "] = " (expr2 Y)]
  X Y -> [(id "reg") "[" (id "sp") " + " X "] = " (expr2 Y)])

(define nregs->
  [X] Acc -> (@ [[(id "reg_size") "(" X ")"]
                 [(id "sp_top") " = " X]]
                Acc)
             where (number? X)
  X Acc -> (@ [["n = " (sum-expr2 X)]
               [(id "reg_size") "(n)"]
               [(id "sp_top") " = n"]]
              Acc))

(define expr1
  [klvm.entry F Nargs Name] C Acc -> (func-entry F Nargs Name C Acc)
  [klvm.return X Next] C Acc -> (func-return (expr2 X) Next C Acc)
  [klvm.goto L] C Acc -> [["return " (func-name (label-sym L C))] | Acc]
  [klvm.goto-next] C Acc -> [["return " (id "next")] | Acc]
  [klvm.call] C Acc -> [["return closure[2]"] | Acc]
  [klvm.if If Then Else] C Acc -> (expr-if If Then Else C Acc)
  [klvm.nargs-cond N L E G] C Acc -> (nargs-cond N L E G C Acc)
  [klvm.if-nargs>0 Then Else] C Acc -> (if-nargs>0 Then Else C Acc)
  [klvm.push-error-handler X] C Acc -> (push-error-handler X C Acc)
  [klvm.pop-error-handler] _ Acc -> [[(id "pop_error_handler") "()"] | Acc]
  [klvm.put-closure-args] C Acc -> (put-closure-args C Acc)
  [klvm.ret-> X] C Acc -> [[(id "ret") " = " (expr2 X)] | Acc]
  [klvm.nregs-> X] C Acc -> (nregs-> X Acc)
  [klvm.reg-> X Y] C Acc -> [(reg-> X Y) | Acc]
  [klvm.next-> X] C Acc -> [[(id "next") " = " (next-val X C)] | Acc]
  [klvm.sp+ X] C Acc -> (@ [[(id "sp") " += " (expr2 X)]
                            [(id' "sp") " = " (id "sp")]]
                           Acc)
  [klvm.sp- X] C Acc -> (@ [[(id "sp") " -= " (expr2 X)]
                            [(id' "sp") " = " (id "sp")]]
                           Acc)
  [klvm.nargs-> X] C Acc -> [[(id "nargs") " = " (expr2 X)] | Acc]
  [klvm.nargs+ X] C Acc -> [[(id "nargs") " += " (expr2 X)] | Acc]
  [klvm.nargs- X] C Acc -> [[(id "nargs") " -= " (expr2 X)] | Acc]
  [klvm.closure-> X] C Acc -> [["closure = " (expr2 X)] | Acc]
  [klvm.wipe X] C Acc -> [[(id "wipe_stack") "(" (expr2 X) ")"] | Acc]
  X C _ -> (error "Broken KLVM in ~S (expr: ~S)" (context-func C) X))

(define exprs
  [] _ Acc -> (reverse Acc)
  [X | Xs] C Acc -> (exprs Xs C (expr1 X C Acc)))

(define func-prelude'
  X Acc -> (let X' ["reg" "fns" "globvars" | X]
                L (join (map (/. X (s ["shen_" X])) X') ", ")
                R (join (map id' X') ", ")
             (append Acc [[L " = " R]])))

(define func-prelude
  X -> (func-prelude' X []) where (not (value same-namespace))
  X -> (func-prelude' X [["global nargs, sp, next, ret"]]))

(define label-prelude
  0 -> (func-prelude [])
  _ -> (func-prelude ["sp"]))

(define label
  N Code C Acc -> (let FN (func-name (label-sym N C))
                    (@ [["global " FN]
                        ["def " FN "():"]
                        [--> | (label-prelude N)]
                        [--> | (exprs Code C [])]]
                       Acc)))

(define labels
  [] _ Acc -> (reverse Acc)
  [[N | L] | Ls] C Acc -> (labels Ls C (label N L C Acc)))

(define mkfunc
  Name Args Nregs Labels ->
  (let Nargs (length Args)
       C (mk-context Name Nargs Nregs 0 "" (value inline))
       R (labels Labels C [])
    R))

(define def-func'
  Name Args -> (py 0 [[(id "defun_x") "(" (esc-obj (str Name)) ", "
                       (length Args) ", " (func-name Name) ")"]
                      []]))

(define def-func
  Name Args -> (def-func' Name Args) where (not (value in-repl))
  Name Args -> (py 0 [[(id "ret") " = " (def-func' Name Args)]]))

(define call-toplevel
  Name -> (py 0 [[(id "nargs") " = 0"]
                 [(id "next") " = " (func-name Name) "()"]])
          where (value in-repl)
  Name -> (py 0 [[(id "call") "(" (func-name Name) ")" (endl)]]))

(define toplevel
  Name Args Nregs Code -> (let X (py 0 (mkfunc Name Args Nregs Code))
                               F (func-name Name)
                               TL (call-toplevel Name)
                            (py 0 [X TL ["del " F] []])))

(define py-from-kl-toplevel
  X <- (do (output "KL: ~S~%" X) (fail)) where false
  X <- (do (klvm.dbg.show-code [X]) (fail)) where false
  
  [klvm.closure Name Args Nregs Code] ->
  (let Fname (closure-name Name)
    (cn (py 0 (mkfunc Name Args Nregs Code))
        (py 0 [[]
               [(esc-obj Fname) " = " (closure-obj Name (length Args))]
               []])))

  [klvm.func Name Args Nregs Code] -> (cn (py 0 (mkfunc Name Args Nregs Code))
                                          (def-func Name Args))
  [klvm.toplevel Name Args Nregs Code] -> (toplevel Name Args Nregs Code))

(define py-from-klvm
  [] Acc -> Acc
  [X | Y] Acc -> (py-from-klvm Y (cn Acc (py-from-kl-toplevel X))))

(define py-from-kl
  X -> (py-from-klvm (klvm.s2-from-kl (function primitives) X) ""))

(define dump-to-file
  Code To -> (backend-utils.write-file
              (s [(indent 0) "import shenpy" (endl) Code (endl)]) To))

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


\*



*\
