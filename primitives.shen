(package py- [py-from-kl shenpy- klvm-runtime klvm-reg]

(set int-funcs [[[X] | [hd tl not string? number? symbol? cons?
                        vector? absvector? value intern vector
                        read-byte close absvector str tlstr n->string
                        string->n empty? get-time error simple-error
                        eval-kl error-to-string]]
                [[X Y] | [+ - * / = > >= < <= cons set <-address
                         cn pos @p pr]]
                [[X Y Z] | [address-> open]]])

(define count-int-funcs-aux
  _ [] Acc -> Acc
  0 [[_ | A] | R] Acc -> (count-int-funcs-aux 0 R (+ Acc (length A)))
  1 [[[X] | A] | R] Acc -> (count-int-funcs-aux 1 R (+ Acc (length A)))
  2 [[[X Y] | A] | R] Acc -> (count-int-funcs-aux 2 R (+ Acc (length A)))
  3 [[[X Y Z] | A] | R] Acc -> (count-int-funcs-aux 3 R (+ Acc (length A)))
  N [[_ | A] | Rest] Acc -> (count-int-funcs-aux N Rest Acc))

(define parg
  F X -> (py-expr2 (hd (F X)) _))

(define mkargs
  F [X] Acc -> (make-string "~A~A" Acc (parg F X))
  F [X | Y] Acc -> (let Acc (make-string "~A~A, " Acc (parg F X))
                     (mkargs F Y Acc)))

(define mkprim
  F Name Args -> (make-string "shenpy.~A(~A)" Name (mkargs F Args "")))

(define primitives-aux
  F [klvm-internal] -> "proc()"
  F [= X X] -> "True"
  F [= X Y] -> "False" where (or (and (number? X) (number? Y))
                                 (and (string? X) (string? Y)))
  F [= X Y] -> (mkprim F "isequal" [X Y])
  F [string? X] -> (make-string "isinstance(~A, str)" (parg F X))
  F [number? X] -> (let X (parg F X)
                      Fmt "(isinstance(~A, int) or isinstance(~A, float))"
                   (make-string Fmt X X))
  F [symbol? X] -> (mkprim F "issymbol" [X])
  F [cons? X] -> (mkprim F "iscons" [X])
  F [vector? X] -> (mkprim F "isvector" [X])
  F [absvector? X] -> (mkprim F "isabsvector" [X])
  F [empty? X] -> (mkprim F "isempty" [X])

  F [str X] -> (mkprim F "str" [X])
  F [tlstr X] -> (make-string "~A[1:]" (parg F X))
  F [n->string X] -> (make-string "chr(~A)" (parg F X))
  F [string->n X] -> (make-string "ord(~A)" (parg F X))

  F [not X] -> (make-string "(not ~A)" (parg F X))
  F [intern X] -> (make-string "[shenpy.type_symbol, ~S]" (parg F X))
  F [hd X] -> (make-string "~A[1]" (parg F X))
  F [tl X] -> (make-string "~A[2]" (parg F X))
  F [value X] -> (make-string "shenpy.globals[~A]" (parg F X))
  F [set X Y] -> (make-string
                "shenpy.globals[~A] = ~A" (parg F X) (parg F Y))
  F [vector X] -> (let X (parg F X)
                  (make-string "([~A] + [None] * ~A)" X X))
  F [absvector X] -> (make-string "([None] * ~A)" (parg F X))
  F [<-address V X] -> (make-string "~A[~A]" (parg F V) (parg F X))
  F [address-> V X Y] -> (make-string "~A[~A] = ~A"
                                    (parg F V)
                                    (parg F X)
                                    (parg F Y))

  F [open X Y Z] -> (mkprim F "open" [X Y Z])
  F [pr X Y] -> (mkprim F "pr" [X Y])
  F [read-byte X] -> (mkprim F "read_byte" [X])
  F [close X] -> (mkprim F "close" [X])

  F [get-time X] -> (mkprim F "get_time" [X])
  F [error X] -> (mkprim F "error" [X])
  F [simple-error X] -> (mkprim F "error" [X])
  F [eval-kl X] -> (mkprim F "eval_kl" [X])
  F [error-to-string X] -> (mkprim F "error_to_string" [X])

  F [cn X Y] -> (make-string "(~A + ~A)" (parg F X) (parg F Y))
  F [pos X Y] -> (make-string "~A[~A]" (parg F X) (parg F Y))
  F [@p X Y] -> (make-string
                 "[shenpy.type_tuple, ~A, ~A]" (parg F X) (parg F Y))
  F [cons X Y] -> (make-string
                   "[shenpy.type_cons, ~A, ~A]" (parg F X) (parg F Y))
  F [Op X Y] -> (make-string "(~A ~A ~A)" (parg F X) Op (parg F Y))
                where (element? Op [+ - * / > < >= <=])
  _ _ -> (fail))

(define primitives
  F X -> (let X' (primitives-aux F X)
           (if (= X' (fail))
               (fail)
               [klvm-native X']))
  _ _ -> (fail))

(define generate-prim
  X Args -> (py-from-kl [[defun X Args [X | Args]]]))

(define gen-prim-func
  X -> [X])

(define gen-prim-args
  N N Acc -> (reverse Acc)
  I N Acc -> (gen-prim-args (+ I 1) N [[klvm-reg (+ I 1)] | Acc]))

(define generate-prim
  X Args -> (let Name (sym-py-from-shen (concat shenpy- X))
                 S (make-string "def ~A():~%" Name)
                 Nargs (length Args)
                 Args' (gen-prim-args 0 Nargs [])
                 Code (primitives-aux (function gen-prim-func) [X | Args'])
                 Fmt "return shenpy.mkfun(~A, ~A, lambda: ~A)~%~%"
                 S (cn S (pyindent 1 (make-string Fmt Name Nargs Code)))
                 X' (esc-obj (str X))
              (cn S (make-string "shenpy.fns[~A] = ~A~%~%" X' Name))))

(define generate-primitives-n
  _ [] Acc -> Acc
  Args [X | R] Acc -> (let S (generate-prim X Args)
                           Acc (make-string "~A~A" Acc S)
                        (generate-primitives-n Args R Acc)))

(define generate-primitives-aux
  [] Acc -> Acc
  [[Args | Prims] | R] Acc -> (let Acc (generate-primitives-n Args Prims Acc)
                                (generate-primitives-aux R Acc)))

(define generate-primitives
  -> (let S (template)
          S (generate-primitives-aux (value int-funcs) S)
       (cn S (py-from-kl (klvm-runtime)))))

)
