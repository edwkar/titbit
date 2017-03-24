;; General syntax rules

(define-syntax when
  (syntax-rules ()
    ((_ cond_ e0 ...)
      (if cond_ 
          (begin 
            e0 
            ...)))))


;; General routines 

(define map* map)

(define (simple? sexp)
  (not (pair? sexp)))

(define (print . objs)
  (for-each display objs)
  (newline))


;; Minimal class-based object-orientation system.,

(define-syntax class
  (syntax-rules (else)
    ((_ ctor-params 
      ((priv-name0 priv-val0) ...) 
      (method0-name method0-params method0-body0 ...)
      ...
      (else else-method))

      (letrec ((ctor (lambda ctor-params
                       (let* ((priv-name0 priv-val0) 
                              ...)
                         (lambda (msg . args)
                           (letrec ((method0-name (lambda method0-params 
                                                    method0-body0 
                                                    ...))
                                     ...)
                             (case msg
                               ((__ctor) ctor)
                               ((method0-name)  (apply method0-name args))
                               ...
                               (else (apply else-method msg args)))))))))
         ctor))

    ((_ ctor-params 
      ((priv-name0 priv-val0) ...) 
      (method0-name method0-params method0-body0 ...)
      ...)

      (class ctor-params
        ((priv-name0 priv-val0) ...) 
        (method0-name method0-params method0-body0 ...)
        ...
        (else (lambda (msg . rem-args)
                (display (string-append "ERROR: Unrecognized msg: " 
                                        (symbol->string msg)))
                (newline)
                (error)))))

    ((_ ctor-params 
      method0
      ...)

      (class ctor-params
        ()
        method0
        ...))))

(define-syntax simple-data-class
  (syntax-rules ()
    ((_ (member-name0 ...))
      (class (member-name0 ...)
        (else 
          (lambda (msg)
            (case msg
              ((member-name0) member-name0)
              ...
              (else 
                (display (string-append "ERROR: Unrecognized msg: " 
                                        (symbol->string msg)))
                (newline)
                (error)))))))
          
    ((_ (member-name0 ... . vararg-member-name))
      (class (member-name0 ... . vararg-member-name)
        (else 
          (lambda (msg)
            (case msg
              ((member-name0) member-name0)
              ...
              ((vararg-member-name) vararg-member-name)
              (else 
                (display (string-append "ERROR: Unrecognized msg: " 
                                        (symbol->string msg)))
                (newline)
                (error))))))) ))

(define-syntax class-tree-rec-expand-cases
  (syntax-rules ()
    ((_ args) 
      (begin
        (display "ERROR: Unrecognized object! ")
        (display (car args))
        (newline)
        (error)))

    ((_ args ((predicate) (handler-param0 ...)
               handler-body0 
               ...)
              rem0 
              ...)
      (let ((obj (car args)))
        (if (predicate obj)
            (apply (lambda (handler-param0 ...)
                     handler-body0
                     ...)
                   args)
            (class-tree-rec-expand-cases args rem0 ...))))

    ((_ args (type (handler-param0 ...)
               handler-body0 
               ...)
              rem0 
              ...)
      (let ((obj (car args)))
        (if (eqv? (obj '__ctor) type)
            (apply (lambda (handler-param0 ...)
                     handler-body0
                     ...)
                   args)
            (class-tree-rec-expand-cases args rem0 ...))))))

(define-syntax class-tree-rec
  (syntax-rules ()
    ((_ rec-name (first-arg0 ...) case0 ...)
      (letrec ((rec-name (lambda args
                           (class-tree-rec-expand-cases args case0 ...))))
        (rec-name first-arg0 ...)))))
