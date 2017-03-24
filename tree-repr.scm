(load "common.scm")
(load "object-converter.scm")

(define (tree-repr top-level-form)
  (class-tree-rec rec (top-level-form)
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
       `(InvocationExpression (fn-sexp:        ,(rec (inv-expr 'fn-sexp)))
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
       `(AnalyzedProcedureReference (proc-name:     ,((ref 'procedure) 'name))
                                    (proc-free-vars ,((ref 'procedure) 'free-vars))))))
