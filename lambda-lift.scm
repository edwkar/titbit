(load "common.scm")

(define LambdaLiftResult
  (simple-data-class 
    (procedures top-level-form)))

(define __lambda-lift__*next-proc-id* -1)

(define (lambda-lift top-level-form)
  (let* ((*procedures* '())

         (next-proc-name (lambda ()
           (set! __lambda-lift__*next-proc-id* 
                 (+ __lambda-lift__*next-proc-id* 1))
           __lambda-lift__*next-proc-id*))

         (transformed-tlf 
           (class-tree-rec rec (top-level-form (VarTracker 'no-parent))
             ((number?) (num var-tracker)
               num)

             ((symbol?) (symbol var-tracker)
               (var-tracker 'register-read symbol)
               symbol)

             (IfExpression (if-expr var-tracker)
               (IfExpression (rec (if-expr 'condition)  var-tracker)
                             (rec (if-expr 'true-sexp)  var-tracker)
                             (rec (if-expr 'false-sexp) var-tracker)))

             (Set!Command (set!-command var-tracker)
               (var-tracker 'register-write (set!-command 'var))
               (Set!Command (set!-command 'var) 
                            (rec (set!-command 'value) var-tracker)))

             (InvocationExpression (inv-expr var-tracker)
               (InvocationExpression (rec (inv-expr 'fn-sexp) var-tracker)
                                     (map* (lambda (s) (rec s var-tracker))
                                           (inv-expr 'argument-sexps))))

             (Procedure (proc enclosing-var-tracker)
               (let ((proc-var-tracker (VarTracker enclosing-var-tracker)))
                 (for-each (lambda (p) (proc-var-tracker 'register-binding p))
                           (proc 'params))

                 (let* ((rewritten-body (rec (proc 'body) proc-var-tracker))
                        (analyzed-proc (AnalyzedProcedure (next-proc-name)
                                                          (proc 'params)
                                                          (proc-var-tracker 'free-vars) 
                                                          rewritten-body)))
                   (set! *procedures* (cons analyzed-proc 
                                            *procedures*))
                   (AnalyzedProcedureReference analyzed-proc)))))))

    (LambdaLiftResult *procedures* transformed-tlf)))



(load "tree-repr.scm")

(define res
  (lambda-lift (convert-to-objects (quote

  (lambda ()
    ((lambda (n rec)
      (if (equals n 0)
          1
          (multiply n (rec (minus n 1) rec))))

      10
      (lambda (n rec)
        (if (equals n 0)
          1
          (multiply n (rec (minus n 1) rec)))) 
       )
    
    )))))


(define (emit-expression expr free-vars)
  (class-tree-rec rec (expr)
    ((number?) (num)
      (display "new SchemeInt(")
      (display (number->string num))
      (display ")"))

    ((symbol?) (symbol)
      (if (memq symbol free-vars)
          (begin
            (display "cls.")
            (display symbol))
          (display symbol)))

    (InvocationExpression (inv-expr)
      (display "(")
      (emit-expression (inv-expr 'fn-sexp) free-vars)
      (display ").apply(")
      (for-each (lambda (p) 
                  (emit-expression p free-vars)
                  (display ", "))
                (inv-expr 'argument-sexps))
      (display ")")
      
      )

    (IfExpression (if-expr)
      (display "(")
      (emit-expression (if-expr 'condition) free-vars)
      (display ".isTrue() ? ")
      (emit-expression (if-expr 'true-sexp) free-vars)
      (display " : ")
      (emit-expression (if-expr 'false-sexp) free-vars)
      (display ")")
      
      )
    
    (AnalyzedProcedureReference (aprocr)
      (let ((aproc (aprocr 'procedure)))                                
        (display "new proc_")
        (display (aproc 'name))
        (display "_closure(")
        (for-each (lambda (p) 
                    (if (memq p free-vars)
                      (begin
                        (display "cls.")
                        (display p))
                      (display p))
                    (display ", "))
                  (aproc 'free-vars))
        (display ")")))
    
    ))


(define (emit-procedure proc)
  (display "// ") 
  (print (tree-repr proc))

  (display "static final SchemeObject proc_")
  (display (number->string (proc 'name)))
  (display "(")
  (display "final proc_")
  (display (proc 'name))
  (display "_closure cls, ")
  (for-each (lambda (p) 
              (display "final SchemeObject ")
              (display p)
              (display ", "))
            (proc 'params))
  (print ") {")
  (display "  return ")
  (emit-expression (proc 'body) (proc 'free-vars))
  (print ";")
  (print "}")

  (display "static final class proc_")
  (display (proc 'name))
  (print "_closure extends SchemeClosure {")
  (for-each (lambda (p) 
              (display "  final SchemeObject ")
              (display p)
              (print ";"))
            (proc 'free-vars))
  (display "  proc_")
  (display (proc 'name))
  (display "_closure(")
  (for-each (lambda (p) 
              (display "final SchemeObject ")
              (display p)
              (display ", "))
            (proc 'free-vars))
  (print ") {")
  (display "    ")
  (for-each (lambda (p) 
              (display "this.")
              (display p)
              (display "=")
              (display p)
              (display "; "))
            (proc 'free-vars))
  (print "")
  (print "  }")

  (display "  @Override SchemeObject apply(")
  (for-each (lambda (p) 
              (display "final SchemeObject ")
              (display p)
              (display ", "))
            (proc 'params))
  (print ") {")
  (display "    return proc_")  
  (display (number->string (proc 'name)))
  (display "(this, ")
  (for-each (lambda (p) 
              (display p)
              (display ", "))
            (proc 'params))
  (print ");")
  (print "  }")

  (print "}")

  (print "")
  
  )

(define (emit ll-res)
  (map* emit-procedure (ll-res 'procedures)))

(emit res)
