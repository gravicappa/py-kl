(defstruct context
  (nregs number)
  (toplevel string)
  (argname symbol)
  (varname symbol))

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
  X -> (cn "$shen$" X)
       where (element? X ["return" "new" "def" "while" "for"
                          "if" "do" "in" "print" "eval"
                          "object" "lambda"])
  X -> (str-py-from-shen* X ""))

(define sym-py-from-shen
  X -> (intern (str-py-from-shen (str X))))

(set py-backslash (n->string 92))
(set py-dquote (n->string 34))

(define esc-string
  "" Acc -> Acc
  (@s C S) Acc -> (let P (value py-backslash)
                    (esc-string S (make-string "~A~A~A" Acc P C)))
                  where (or (= C (value py-backslash))
                            (= C (value py-dquote)))
  (@s C S) Acc -> (esc-string S (cn Acc "\x0a"))
                  where (= (string->n C) 10)
  (@s C S) Acc -> (esc-string S (cn Acc "\x0d"))
                  where (= (string->n C) 13)
  (@s C S) Acc -> (esc-string S (cn Acc C)))

(define func-name
  X -> X)

(define esc-obj
  X -> (make-string "c#34;~Ac#34;" (esc-string X "")) where (string? X)
  X -> (func-name X) where (shen-sysfunc? X)
  X -> (sym-py-from-shen X) where (symbol? X)
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
  (nargs number)
  (nregs number))

(define emit-set
  X V C -> (let S (esc-string (str X) "")
                V (py-from-kl-expr V false C)
             (make-string "Shenpy.globals[c#34;~Ac#34;] = ~A~%" S V)))

(define py-expr2
  [shen-func-obj] C -> (make-string "[~A, ~A, ~A, shenpy.reg[~A : ~A + ~A]]"
                                    "shenpy.type_function"
                                    (esc-obj (pycontext-func C))
                                    (pycontext-nargs C)
                                    (+ 1 (pycontext-nargs C))
                                    (+ 1 (pycontext-nargs C))
                                    "shenpy.nargs")
  [shen-reg N] _ -> (make-string "shenpy.reg[~A]" N)
  [shen-stack N] _ -> (make-string "shenpy.stack[shenpy.sp + ~A]" (+ N 1))
  [shen-nargs] _ -> "shenpy.nargs"
  [shen-null-label] _ -> "None"
  X _ -> X where (number? X)
  X _ -> (esc-obj X))

(define py-label-sym
  X C -> (concat shen-lbl (concat X (concat - (pycontext-func C)))))

(define py-label-str
  X C -> (str-py-from-shen (str (py-label-sym X C))))

(define py-expr-label
  N C -> (py-label-str N C) where (number? N)
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
              Fmt "shenpy.reg[x + ~A] = shenpy.stack[sp + x + 1]~%"
              S (cn S (pyindent (+ L 1) (make-string Fmt N)))
           (pyindent L S)))

(define py-expr1
  [shen-return] _ -> (make-string "return shenpy.reg[0]~%")
  [shen-inc-nargs X] C -> (make-string "shenpy.nargs += ~A~%" (py-expr2 X C))
  [shen-dec-nargs X] C -> (make-string "shenpy.nargs -= ~A~%" (py-expr2 X C))
  [shen-ensure-stack-size X] _ ->
  (make-string "shenpy.ensure_stack_size(~A)~%" (py-expr2 X C))
  [shen-nregs-> X Y] C -> (make-string "shenpy.reg_size(~A + ~A)~%"
                                       (py-expr2 X C)
                                       (py-expr2 Y C))
  [shen-nregs-> N] C -> (make-string "shenpy.reg_size(~A)~%" (py-expr2 N C))
  [shen-stack-> N X] C -> (let F "shenpy.stack[shenpy.sp + ~A] = ~A~%"
                            (make-string F (+ N 1) (py-expr2 X C)))
  [shen-reg-> 0 X] C -> (make-string "shenpy.reg[0] = ~A~%"
                                     (py-expr-label X C))
  [shen-reg-> N X] C -> (make-string "shenpy.reg[~A] = ~A~%" N (py-expr2 X C))
  [shen-inc-stack-ptr X] C -> (make-string "shenpy.sp += ~A~%" (py-expr2 X C))
  [shen-dec-stack-ptr X] C -> (make-string "shenpy.sp -= ~A~%" (py-expr2 X C))
  [shen-nargs-> X] C -> (make-string "shenpy.nargs = ~A~%"
                                     (py-expr2 X C))
  [shen-goto N] C -> (make-string "return ~A~%" (py-label-str N C))
  [shen-call X] C -> (make-string "return ~A~%" (str-py-from-shen (str X)))
                     where (symbol? X)
  [shen-call X] C -> (make-string "return ~A~%" (py-expr2 X C))
  X _ -> (error "Broken KLVM (expr: ~A)" X))

(define py-expr
  _ [shen-nargs-cond X Y Z] C -> (py-nargs-cond X Y Z C)
  _ [shen-nargs>0 X Y] C -> (py-nargs>0 X Y C)
  L [shen-push-extra-args [shen-nargs]] C -> (py-push-extra-args L C)
  L [shen-pop-extra-args [shen-nargs]] C -> (py-pop-extra-args L C)
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
  Name -> (make-string "~%def ~A():~%" (str-py-from-shen (str Name))))

(define py-label
  0 Code C -> (cn (py-func-hdr (pycontext-func C)) (py-exprs 1 Code C ""))
  N Code C -> (cn (py-func-hdr (py-label-sym N C)) (py-exprs 1 Code C "")))

(define py-labels
  [] _ Acc -> Acc
  [[[shen-label N] | X] | Y] C Acc -> (let Acc (cn Acc (py-label N X C))
                                        (py-labels Y C Acc)))

(define py-mkfunc
  Name Args Nregs Code -> (let N (length Args)
                               C (mk-pycontext Name N Nregs)
                            (py-labels Code C "")))

(define py-from-kl-toplevel
  [set X V] _ -> (emit-set 0 X V)
  [shen-mk-func Name Args Nregs Code] _ -> (py-mkfunc Name Args Nregs Code)
  X _ -> "")

(define py-from-kl-aux
  [] Acc -> Acc
  [X | Y] Acc -> (py-from-kl-aux Y (cn Acc (py-from-kl-toplevel X _))))

(define py-from-kl
  X -> (py-from-kl-aux X ""))

(set skip-internals false)

(define py-dump-file
  Code To -> (let F (open file To out) 
                  . (pr (make-string "import shenpy~%~%") F)
                  . (pr Code F)
                  . (close F)
               true))
