(load "util.scm")


(define IfExpression 
  (simple-data-class 
    (condition true-sexp false-sexp)))

(define Set!Command 
  (simple-data-class 
    (var value)))

(define InvocationExpression 
  (simple-data-class 
    (fn-sexp argument-sexps)))

(define Procedure 
  (simple-data-class 
    (params body)))

(define AnalyzedProcedure 
  (simple-data-class 
    (name params free-vars body)))

(define AnalyzedProcedureReference 
  (simple-data-class 
    (procedure)))


(define (ast->list-repr ast)
  (class-tree-rec rec (ast)
     ((list?) (list_) 
       (map rec list_))

     (((lambda (obj) 
         (or (symbol? obj) 
             (string? obj) 
             (number? obj))))
       (obj)
         obj)

     (IfExpression (if-expr)
       `(IfExpression (cond:       ,(rec (if-expr 'condition)))
                      (true-sexp:  ,(rec (if-expr 'true-sexp)))
                      (false-sexp: ,(rec (if-expr 'false-sexp)))))

     (Set!Command (set!-cmd)
       `(Set!Command (:var ,(rec (set!-cmd 'var)))
                     (:value ,(rec (set!-cmd 'value)))))

     (InvocationExpression (inv-expr)
       `(InvocationExpression 
          (fn-sexp:        ,(rec (inv-expr 'fn-sexp)))
          (argument-sexps: ,(rec (inv-expr 'argument-sexps)))))


     (Procedure (proc)
       `(Procedure (params: ,(rec (proc 'params)))
                   (body:   ,(rec (proc 'body)))))

     (LambdaLiftResult (llr)
       `(LambdaLiftResult (procedures:     ,(rec (llr 'procedures)))
                          (top-level-form: ,(rec (llr 'top-level-form)))))


     (AnalyzedProcedure (aproc)
       `(AnalyzedProcedure (name:      ,(rec (aproc 'name))) 
                           (params:    ,(rec (aproc 'params)))
                           (free-vars: ,(rec (aproc 'free-vars)))
                           (body:      ,(rec (aproc 'body)))))

     (AnalyzedProcedureReference (ref)
       `(AnalyzedProcedureReference 
          (proc-name:     ,((ref 'procedure) 'name))
          (proc-free-vars ,((ref 'procedure) 'free-vars))))))


(define (raw->ast top-level-sexp)
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

  (dig top-level-sexp))
