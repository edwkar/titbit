(load "common.scm")

(define (convert-to-objects top-level-form)
  (define (dig sexp)
    (if (simple? sexp)
        sexp
        (case (car sexp)
          ((if) 
            (apply IfExpression (map* dig (cdr sexp))))

          ((lambda) 
            (let ((params (cadr sexp))
                  (body   (caddr sexp)))
              (Procedure params (dig body))))

          ((set!) 
            (let ((name (cadr sexp))
                  (val-sexp (caddr sexp)))
              (Set!Command name (dig val-sexp))))

          (else
            (InvocationExpression (dig (car sexp)) (map dig (cdr sexp)))))))

  (dig top-level-form))
