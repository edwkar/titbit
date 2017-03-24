(load "util.scm")


(define VarTracker 
  (class (parent-var-tracker)
    ((*free-vars*  '())
     (*bound-vars* '()))

    (bound-vars () *bound-vars*)
    (free-vars  () *free-vars*)

    (register-binding (name) 
      (set! *bound-vars* (cons name *bound-vars*)))

    (register-write (name)
      (register-read name)) ;; XXX TODO!!

    (register-read (name)
      (let ((bound? (memq name *bound-vars*)))
        (when (not bound?)
          (let ((registered? (memq name *free-vars*)))
                (when (not registered?)
                  (set! *free-vars* (cons name *free-vars*))))
                (if (not (eqv? parent-var-tracker 'no-parent))
                    (parent-var-tracker 'register-read name)))))))

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
                        (analyzed-proc 
                          (AnalyzedProcedure (next-proc-name)
                                             (proc 'params)
                                             (proc-var-tracker 'free-vars) 
                                             rewritten-body)))
                   (set! *procedures* (cons analyzed-proc 
                                            *procedures*))
                   (AnalyzedProcedureReference analyzed-proc)))))))

    (LambdaLiftResult *procedures* transformed-tlf)))
