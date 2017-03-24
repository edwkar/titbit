(load "util.scm")
(load "ast.scm")


;; fugly, re-factor! 
(define (for-each-with-sep f xs sep)
  (if (= 0 (length xs))
      #f
      (begin
        (f (car xs))
        (for-each (lambda (x) 
                    (display sep)
                    (f x))
                  (cdr xs)))))


(define (for-each-with-comma f xs)
  (for-each-with-sep f xs ", "))


(define (emit-expression expr free-vars)
  (class-tree-rec rec (expr)
    ((number?) (num)
      (display "new SchemeInt(")
      (display (number->string num))
      (display "L)"))

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
      (for-each-with-comma 
        (lambda (p) 
          (emit-expression p free-vars))
        (inv-expr 'argument-sexps))
      (display ")"))

    (IfExpression (if-expr)
      (display "((")
      (emit-expression (if-expr 'condition) free-vars)
      (display ").isTrue() ? (")
      (emit-expression (if-expr 'true-sexp) free-vars)
      (display " ) : (")
      (emit-expression (if-expr 'false-sexp) free-vars)
      (display "))"))
    
    (AnalyzedProcedureReference (aprocr)
      (let ((aproc (aprocr 'procedure)))                                
        (display "new proc_")
        (display (aproc 'name))
        (display "_closure(")
        (for-each-with-comma 
          (lambda (p) 
            (if (memq p free-vars)
              (begin
                (display "cls.")
                (display p))
              (display p)))
          (aproc 'free-vars))
        (display ")")))))


(define (emit-procedure proc)
  (display "// ") 
  (print (ast->list-repr proc))

  ;; Emit procedure.
  (display "static final SchemeObject proc_")
  (display (number->string (proc 'name)))
  (display "(")
  (for-each-with-comma display 
                       (cons (string-append "final proc_"
                                            (number->string (proc 'name))
                                            "_closure cls")
                             (map (lambda (p) 
                                    (string-append "final SchemeObject "
                                                   (symbol->string p)))
                                  (proc 'params))))
  (print ") {")
  (display "  return ")
  (emit-expression (proc 'body) (proc 'free-vars))
  (print ";")
  (print "}")

  ;; Emit closure.
  (display "static final class proc_")
  (display (proc 'name))
  (print "_closure extends SchemeObject {")
  (for-each-with-sep (lambda (p) 
                       (display "  final SchemeObject ")
                       (display p))
                     (proc 'free-vars)
                     ";\n")
  (display ";\n")
  (display "  proc_")
  (display (proc 'name))
  (display "_closure(")
  (for-each-with-comma (lambda (p) 
                         (display "final SchemeObject ") 
                         (display p))
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
  (for-each-with-comma (lambda (p) 
                         (display "final SchemeObject ")
                         (display p))
                       (proc 'params))
  (print ") {")
  (display "    return proc_")  
  (display (number->string (proc 'name)))
  (display "(")
  (for-each-with-comma display 
                       (cons "this" (proc 'params)))
  (print ");")
  (print "  }")

  (print "}")

  (print ""))


(define (emit-program lambda-lifted-ast)
  (map* emit-procedure (lambda-lifted-ast 'procedures)))
