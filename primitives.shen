(package py [py-from-kl shenpy-eval klvm.native klvm.reg klvm.runtime]

(set int-funcs [[[X] | [hd tl not string? number? symbol? cons?
                        vector? absvector? value intern vector
                        read-byte close absvector str tlstr n->string
                        string->n empty? error simple-error
                        error-to-string]]
                [[X Y] | [+ - * / = > >= < <= cons set <-address
                         cn pos @p write-byte or and]]
                [[X Y Z] | [address->]]])

(define count-int-funcs-aux
  _ [] Acc -> Acc
  0 [[_ | A] | R] Acc -> (count-int-funcs-aux 0 R (+ Acc (length A)))
  1 [[[X] | A] | R] Acc -> (count-int-funcs-aux 1 R (+ Acc (length A)))
  2 [[[X Y] | A] | R] Acc -> (count-int-funcs-aux 2 R (+ Acc (length A)))
  3 [[[X Y Z] | A] | R] Acc -> (count-int-funcs-aux 3 R (+ Acc (length A)))
  N [[_ | A] | Rest] Acc -> (count-int-funcs-aux N Rest Acc))

(define mkargs
  [X] Acc -> (cn Acc (expr2 X))
  [X | Xs] Acc -> (mkargs Xs (s [Acc (expr2 X) ", "])))

(define mkprim
  Name Args -> (s [(id Name) "(" (mkargs Args "") ")"]))

(define prim-intern
  "true" -> "True"
  "false" -> "False"
  X -> (mkprim "intern" [X]))

(define prim-value
  X -> (s [(id "globvars") "[" (esc-obj (str X)) "]"]) where (symbol? X)
  X -> (s [(id "globvars") "[" X "[1]]"]))

(define prim-vector
  X -> (s ["([" X "] + [" (id "fail_obj") "] * " X ")"]))

(define prim-tuple
  X Y -> (s ["[" (id "fns") "['shen.tuple'], " X ", " Y "]"]))

(define prim-number?
  X -> (s ["(isinstance(" X ", (int, long, float))"
           " and not isinstance(" X ", bool))"]))

(define primitives'
  [] -> "()"
  [= X Y] -> (mkprim "isequal" [X Y])
  [string? X] -> (make-string "isinstance(~A, str)" (expr2 X))
  [number? X] -> (prim-number? (expr2 X))
  [symbol? X] -> (mkprim "issymbol" [X])
  [cons? X] -> (mkprim "iscons" [X])
  [vector? X] -> (mkprim "isvector" [X])
  [absvector? X] -> (mkprim "isabsvector" [X])
  [empty? X] -> (make-string "(~A == ())" (expr2 X))
  [str X] -> (mkprim "tostring" [X])
  [tlstr X] -> (make-string "~A[1:]" (expr2 X))
  [n->string X] -> (mkprim "chr" [X])
  [string->n X] -> (mkprim "ord" [X])
  [not X] -> (make-string "(not ~A)" (expr2 X))
  [intern X] -> (prim-intern X)
  [hd X] -> (make-string "~A[1]" (expr2 X))
  [tl X] -> (make-string "~A[2]" (expr2 X))
  [value X] -> (prim-value (expr2 X))
  [set X Y] -> (mkprim "setval" [X Y])
  [vector X] -> (prim-vector (expr2 X))
  [absvector X] -> (s ["([" (id "fail_obj") "] * " (expr2 X) ")"])
  [<-address V X] -> (make-string "~A[~A]" (expr2 V) (expr2 X))
  [address-> V I X] -> (mkprim "absvector_set" [V I X])
  [read-byte X] -> (mkprim "read_byte" [X])
  [write-byte X Y] -> (mkprim "write_byte" [X Y])
  [close X] -> (s [(expr2 X) ".close()"])
  [error X] -> (mkprim "error" [X])
  [simple-error X] -> (mkprim "error" [X])
  [error-to-string X] -> (mkprim "error_to_string" [X])
  [cn X Y] -> (make-string "(~A + ~A)" (expr2 X) (expr2 Y))
  [pos X Y] -> (make-string "~A[~A]" (expr2 X) (expr2 Y))
  [@p X Y] -> (prim-tuple (expr2 X) (expr2 Y))
  [cons X Y] -> (s ["(" (id "type_cons") ", " (expr2 X) ", " (expr2 Y) ")"])
  [/ X Y] -> (make-string "(1.0 * ~A / ~A)" (expr2 X) (expr2 Y))
  [Op X Y] -> (make-string "(~A ~A ~A)" (expr2 X) Op (expr2 Y))
              where (element? Op [+ - * / > < >= <= and or])
  [pr X Y] -> (mkprim "write_string" [X Y])
  [fail] -> (id "fail_obj")
  _ -> (fail))

(define primitives
  X -> (let X' (primitives' X)
         (if (= X' (fail))
             (fail)
             [klvm.native X'])))

(define primitives
  X -> (primitives' X))

(define gen-prim-args
  N N Acc -> (reverse Acc)
  I N Acc -> (gen-prim-args (+ I 1) N [[klvm.reg I] | Acc]))

(define generate-prim
  X Args -> (let Name (sym-py-from-shen (concat shenpy- X))
                 X' (esc-obj (str X))
                 C (mk-context Name 0 0 0 "" [])
                 Nargs (length Args)
                 Args' (gen-prim-args 0 Nargs [])
                 Code (primitives' [X | Args'])
              (py 0 [["def " Name "():"]
                     [--> ["shen_reg = reg"]
                          ["shen_sp = sp"]
                          ["x = fn_entry(" Name ", " Nargs ", " X' ")"]
                          ["if x != fail_obj: return x"]
                          ["return fn_return(" Code ", " (id "next") ")"]]
                     [(id' "defun_x") "(" X' ", " Nargs ", " Name ")"]])))

(define generate-primitives-n
  _ [] Acc -> Acc
  Args [X | R] Acc -> (let S (generate-prim X Args)
                           Acc (cn Acc S)
                        (generate-primitives-n Args R Acc)))

(define generate-primitives'
  [] Acc -> Acc
  [[Args | Prims] | R] Acc -> (let Acc (generate-primitives-n Args Prims Acc)
                                (generate-primitives' R Acc)))

(define generate-primitives
  -> (let Same-ns true
          S (s [(entry-tpl Same-ns) (endl) (return-tpl Same-ns)])
          S (generate-primitives' (value int-funcs) S)
       (cn S (py-from-kl (klvm.runtime)))))
)
