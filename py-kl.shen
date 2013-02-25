(package py- [py-from-kl klvm-from-kl defstruct kl-imp-show-code
              kl-imp-template-func-body

              shenpy- nargs proc this klvm- lbl shenpy_func

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
              ]

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

\* renaming all py-reserved keywords, functions, ... *\
(define str-py-from-shen
  X -> (cn "shen_" X)
       where (element? X ["return" "def" "while" "for" "if" "else" "in"
                          "print" "eval" "object" "lambda" "not" "True"
                          "False" "__init__" "class" "yield" "None" "import"])
  X -> (str-py-from-shen* X ""))

(define sym-py-from-shen
  X -> (intern (str-py-from-shen (str X))))

(set backslash (n->string 92))
(set dquote (n->string 34))

(define esc-string
  "" Acc -> Acc
  (@s C S) Acc -> (let P (value py-backslash)
                    (esc-string S (make-string "~A~A~A" Acc P C)))
                  where (or (= C (value backslash))
                            (= C (value dquote)))
  (@s C S) Acc -> (esc-string S (cn Acc "\x0a"))
                  where (= (string->n C) 10)
  (@s C S) Acc -> (esc-string S (cn Acc "\x0d"))
                  where (= (string->n C) 13)
  (@s C S) Acc -> (esc-string S (cn Acc C)))

(define func-name
  X -> (sym-py-from-shen (concat klvm- X)))

(define esc-obj
  X -> (make-string "c#34;~Ac#34;" (esc-string X "")) where (string? X)
  X -> (func-name X) where (shen-sysfunc? X)
  X -> (sym-py-from-shen X) where (symbol? X)
  [proc] -> "proc()" \* for translation template code *\
  X -> (error "Object ~R cannot be escaped" X))

(set *py-indent* "    ")

(define pyindent
  0 S -> S
  Level S -> (pyindent (- Level 1) (cn (value *py-indent*) S)))

(define cut-shen-prefix
  (@s "shen-" S) -> S
  (@s "shen_" S) -> S
  S -> S)

(defstruct pycontext
  (func symbol)
  (nargs A)
  (nregs number))

(define emit-set
  X V C -> (let S (esc-string (str X) "")
                V (py-from-kl-expr V false C)
             (make-string "Shenpy.globals[c#34;~Ac#34;] = ~A~%" S V)))

(define py-mk-closure
  Func Nargs Ninit ->
  (let R (make-string "shenpy.sp + 1 : shenpy.sp + 1 + ~A" Ninit)
       T "shenpy.type_function"
       F (func-name Func)
    (make-string "[~A, ~A, ~A, shenpy.stack[~A], None]" T F Nargs R)))

(define py-expr2
  [klvm-closure-nargs] _ -> "len(t[3])"
  [klvm-closure-func] _ -> "t[1]"
  [klvm-func-obj] C -> (make-string
                        "[~A, ~A, ~A, shenpy.reg[1 : 1 + ~A], None]"
                        "shenpy.type_function"
                        (func-name (pycontext-func C))
                        (pycontext-nargs C)
                        "shenpy.nargs")
  [klvm-reg N] _ -> (make-string "shenpy.reg[~A]" N)
  [klvm-stack N] _ -> (make-string "shenpy.stack[shenpy.sp + ~A]" (+ N 1))
  [klvm-nargs] _ -> "shenpy.nargs"
  [klvm-null-label] _ -> "None"
  [klvm-mk-closure Func Nregs Ninit] _ -> (py-mk-closure Func Nregs Ninit)

  [klvm-error-unwind-get-handler] _ ->
  (make-string "shenpy.error_unwind_get_handler()")

  [klvm-current-error] _ -> (make-string "shenpy.error_obj")
  [klvm-native X] _ -> X
  X _ -> X where (number? X)
  X _ -> (esc-obj X))

(define py-label-sym
  X C -> (concat lbl (concat X (concat - (pycontext-func C)))))

(define py-expr-label
  N C -> (func-name (py-label-sym N C)) where (number? N)
  X _ -> (esc-obj X) where (symbol? X)
  X C -> (py-expr2 X C))

(define py-nargs-cond
  X Y Z C ->
  (let N (pycontext-nargs C)
       S (pyindent 1 (make-string "if shenpy.nargs < ~A:~%" N))
       S (py-exprs 2 X C S)
       S (cn S (pyindent 1 (make-string "elif shenpy.nargs > ~A:~%" N)))
       S (py-exprs 2 Z C S)
       S (cn S (pyindent 1 (make-string "else:~%")))
       S (py-exprs 2 Y C S)
    S))

(define py-nargs>0
  X Y C -> (let S (pyindent 1 (make-string "if shenpy.nargs > 0:~%"))
                S (py-exprs 2 X C S)
                S (cn S (pyindent 1 (make-string "else:~%")))
                S (py-exprs 2 Y C S)
             S))

(define py-push-extra-args
  L C -> (let N (pycontext-nargs C)
              S (make-string "for x in range(1, 1 + shenpy.nargs):~%")
              Fmt "shenpy.stack[shenpy.sp + x + 1] = shenpy.reg[x + ~A]~%"
              S (cn S (pyindent (+ L 1) (make-string Fmt N)))
           (pyindent L S)))

(define py-pop-extra-args
  L C -> (let N (pycontext-nargs C)
              S (make-string "for x in range(1, 1 + shenpy.nargs):~%")
              Fmt "shenpy.reg[x + ~A] = shenpy.stack[shenpy.sp + x + 1]~%"
              S (cn S (pyindent (+ L 1) (make-string Fmt N)))
           (pyindent L S)))

(define py-pop-closure-args
  L X C -> (let R (pyindent L (make-string "a = ~A[3]~%" (py-expr2 X C)))
                R (cn R (pyindent L (make-string
                                     "for i in range(0, len(a)):~%" R)))
                L' (+ L 1)
                R (cn R (pyindent L' (make-string
                                      "shenpy.reg[i + 1] = a[i]~%")))

             R))

(define py-sum-expr
  [X] C Acc -> (make-string "~A~A" Acc (py-expr2 X C))
  [0 | Y] C Acc -> (py-sum-expr Y C Acc)
  [X | Y] C Acc -> (let Acc (make-string "~A~A + " Acc (py-expr2 X C))
                     (py-sum-expr Y C Acc)))

(define py-expr1
  [klvm-return] _ -> (make-string "return shenpy.reg[0]~%")
  [klvm-dec-nargs nargs] C -> (make-string "shenpy.nargs -= nargs~%")
  [klvm-inc-nargs X] C -> (make-string "shenpy.nargs += ~A~%" (py-expr2 X C))
  [klvm-dec-nargs X] C -> (make-string "shenpy.nargs -= ~A~%" (py-expr2 X C))
  [klvm-stack-size X] C -> (make-string "shenpy.stack_size(~A)~%"
                                        (py-expr2 X C))
  [klvm-nregs-> X] C -> (make-string "shenpy.reg_size(~A)~%"
                                     (py-sum-expr X C ""))
  [klvm-stack-> N X] C -> (let F "shenpy.stack[shenpy.sp + ~A] = ~A~%"
                            (make-string F (+ N 1) (py-expr2 X C)))
  [klvm-closure-> X] C -> (make-string "t = ~A~%" (py-expr2 X C))
  [klvm-reg-> [0] X] C -> (make-string "shenpy.reg[0] = ~A~%"
                                       (py-expr-label X C))
  [klvm-reg-> X Y] C -> (make-string "shenpy.reg[~A] = ~A~%"
                                     (py-sum-expr X C "")
                                     (py-expr2 Y C))
  [klvm-inc-stack-ptr X] C -> (make-string "shenpy.sp += ~A~%" (py-expr2 X C))
  [klvm-dec-stack-ptr X] C -> (make-string "shenpy.sp -= ~A~%" (py-expr2 X C))
  [klvm-nargs-> X] C -> (make-string "shenpy.nargs = ~A~%"
                                     (py-expr2 X C))
  [klvm-goto N] C -> (make-string "return ~A~%"
                                  (func-name (py-label-sym N C)))
  [klvm-call X] _ -> (make-string "return shenpy.fns[~A]~%" (esc-obj (str X)))
                     where (symbol? X)
  [klvm-call X] C -> (make-string "return ~A~%" (py-expr2 X C))
  [klvm-push-error-handler X] C -> (make-string
                                   "shenpy.push_error_handler(~A)~%"
                                   (py-expr2 X C))
  [klvm-pop-error-handler] _ -> (make-string "shenpy.pop_error_handler()~%")
  X C -> (error "Broken KLVM in ~A (expr: ~A)" (pycontext-func C) X))

(define py-expr
  _ [klvm-nargs-cond X Y Z] C -> (py-nargs-cond X Y Z C)
  _ [klvm-nargs>0 X Y] C -> (py-nargs>0 X Y C)
  L [klvm-push-extra-args [klvm-nargs]] C -> (py-push-extra-args L C)
  L [klvm-pop-extra-args [klvm-nargs]] C -> (py-pop-extra-args L C)
  L [klvm-pop-closure-args F] C -> (py-pop-closure-args L F C)
  L [if If Then Else] C -> (let X (make-string "if ~A:~%" (py-expr2 If C))
                                S (pyindent L X)
                                S (cn S (pyindent (+ L 1) (py-expr1 Then C)))
                                S (cn S (pyindent L (make-string "else:~%")))
                                S (cn S (pyindent (+ L 1) (py-expr1 Else C)))
                             S)
  L X C -> (pyindent L (py-expr1 X C)))

(define py-exprs
  L [] _ Acc -> Acc
  L [X | Y] C Acc -> (py-exprs L Y C (cn Acc (py-expr L X C))))

(define py-func-hdr
  Name -> (make-string "def ~A():~%" (func-name Name)))

(define py-label
  0 Code C -> (cn (py-func-hdr (pycontext-func C)) (py-exprs 1 Code C ""))
  N Code C -> (cn (py-func-hdr (py-label-sym N C)) (py-exprs 1 Code C "")))

(define template-label
  [[[klvm-label 0] | X]] C -> (py-exprs 1 X C ""))

(define template
  -> (let C (mk-pycontext this nargs 0)
          Name "mkfun"
          X (template-label (kl-imp-template-func-body nargs proc) C)
          Fmt "def ~A(~A, nargs, proc):~%~A~%shenpy.mkfun = ~A~%~%"
       (make-string Fmt Name (pycontext-func C) X Name)))

(define py-labels
  [] _ Acc -> Acc
  [[[klvm-label N] | X] | Y] C Acc -> (let Acc (make-string "~A~A~%"
                                                            Acc
                                                            (py-label N X C))
                                        (py-labels Y C Acc)))

(define py-mkfunc
  Name Args Nregs Code -> (let C (mk-pycontext Name (length Args) Nregs)
                               R (py-labels Code C "")
                            (cn R (make-string "shenpy.fns[~A] = ~A~%~%"
                                               (esc-obj (str Name))
                                               (func-name Name)))))

(define py-from-kl-toplevel
  [shen-mk-func Name Args Nregs Code] -> (py-mkfunc Name Args Nregs Code)
  [X] -> (make-string "~A()" X)
  X -> "")

(define py-from-kl-aux
  [] Acc -> Acc
  [X | Y] Acc -> (py-from-kl-aux Y (cn Acc (py-from-kl-toplevel X))))

(define py-from-kl
  X -> (py-from-kl-aux (klvm-from-kl (function primitives) X) ""))

(set skip-internals false)

(define dump-file
  Code To -> (let F (open file To out)
                  . (pr (make-string "import shenpy~%~%") F)
                  . (pr Code F)
                  . (close F)
               true))
)
