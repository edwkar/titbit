;;; This is the titbit Scheme->Java compiler.


(load "ast.scm")
(load "lambda-lifting.scm")
(load "code-generation.scm")

(define input (quote

  (lambda ()
    ((lambda (n rec)
      (if (iszero n)
          1
          (multiply n
                    (rec (minus n 1)
                         rec))))
     64
     (lambda (n rec)
       (if (iszero n)
           1
           (multiply n
                     (rec (minus n 1)
                          rec))))))

))


(define ast (raw->ast input))

(define lambda-lifted-ast (lambda-lift ast))

(emit-program lambda-lifted-ast)

(print "")
