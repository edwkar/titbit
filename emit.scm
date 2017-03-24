(load "common.scm")
(load "lambda-lift.scm")

(define (emit top-level-form)
  (class-tree-rec rec (top-level-form 3222) 

    (Set!Command (sc num)
      'set-command)

    (InvocationExpression (ie num)
      num)

    (Procedure (proc num)
      (map (partial rec _ 999) (proc 'body)))

    (ProcedureReference (proc-reference num)
      'lala)

    (number? (n)
      lala)))


(emit (Procedure 'name '(a b c d) '(k a c) `(,(InvocationExpression 3 4) ,(InvocationExpression 2 3))))
