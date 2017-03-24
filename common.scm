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
      (register-read name)) ;TODO!!

    (register-read (name)
      (let ((bound? (memq name *bound-vars*)))
        (when (not bound?)
          (let ((registered? (memq name *free-vars*)))
                (when (not registered?)
                  (set! *free-vars* (cons name *free-vars*))))
                (if (not (eqv? parent-var-tracker 'no-parent))
                    (parent-var-tracker 'register-read name)))))))


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
