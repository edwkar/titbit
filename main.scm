;;; This is the underbit Scheme->Java compiler.


(load "macro-expansion.ss")
(load "cps-conversion.ss")
(load "util.ss")


(define compilation-stages
  `(
    ('macro-expansion . expand-macros)
    ('cps-conversion . convert-to-cps)
    ('object-based-ast-conversion . convert-to-object-based-ast)
    ('lambda-lifting . lift-lambdas)
    ('code-generation . generate-code)))


(define (main)
  (let* ((input (read)))
    input))
	
