;; A-Normal-Formへ変形する
;; (define name (lambda params body))
;; (define name form)
;; sexp
(define (c.scm:c8a-normalize x)
  (if (pair? x)
      (case (car x)
        ((define) ;; (define var form)
         (let ((form (caddr x))) ;; form
           (if (and (pair? form) ;; form -> (...)
                    (eq? (car form) 'lambda)) ;; form -> (lambda ...)
               (c8a-function (car x) ;; define
                             (cadr x) ;; var
                             (caddr x)) ;; (lambda ...)
               `(,(car x) ,(cadr x) ,(c8normalize-term (caddr x)))))) ;; (define var form)
        (else
         (c8normalize-term x)))
      (c8normalize-term x)))

(define c8a-function
  (lambda (first name lambda-expr)
    (let ((x (c8normalize-term lambda-expr)))
      `(,first ,name ,x))))

(define c8normalize-term
  (lambda (m)
    (c8normalize m (lambda (x) x))))

(define c8normalize
  (lambda (m k)
    (cond ((pair? m)
           (let ((fun (car m))
                 (args (cdr m)))
             (case fun
               ((lambda) (c8lambda args k))
               ((let) (c8let args k))
               ((let*) (c8let* args k))
               ((if) (c8if args k))
               ((set!) (c8set! args k))
               ((begin) (c8begin args k))
               ((letrec) (c8letrec args k))
               ((quote) (c8quote args k))
               (else
                (if (symbol? fun)
                    (c8symbol-fun fun args k))))))
          (else
           (k m)))))

(define c8lambda
  (lambda (args k)
    (let ((params (car args))
          (body (cadr args)))
      (k `(lambda ,params ,(c8normalize-term body))))))

#;(define c8let
(lambda (args k)
  (let ((defs (car args)))
    (let ((def (car defs)))
      (let ((x (car def))
            (m1 (cadr def))
            (m2 (cadr args)))
        (c8normalize m1 (lambda (n1) `(let ((,x ,n1)) ,(c8normalize m2 k)))))))))

(define c8let
  (lambda (args k)
    (let ((defs (car args))
          (body (cadr args)))
      (cond ((null? defs)
             (c8normalize body k))
            ((null? (cdr defs))
             (let ((def (car defs)))
               (let ((var (car def))
                     (exp (cadr def)))
                 (c8normalize exp
                              (lambda (nexp)
                                `(let ((,var ,nexp))
                                   ,(c8normalize body k)))))))
            (else
             (let ((vars (map car defs))
                   (exps (map cadr defs)))
               (let ((tmps (map c8tmp exps)))
                 (let ((tdefs (map list tmps exps))
                       (ndefs (map list vars tmps)))
                   (c8normalize `(let* ,tdefs
                                   (let* ,ndefs
                                     ,body)) k)))))))))

(define c8tmp
  (lambda (x)
    (newvar "anf")))

(define c8let*
  (lambda (args k)
    (let ((defs (car args))
          (body (cadr args)))
      (if (null? defs)
          (c8normalize body k)
          (let ((def (car defs)))
            (let ((var (car def))
                  (exp (cadr def)))
              (c8normalize exp
                           (lambda (aexp)
                             `(let ((,var ,aexp))
                                ,(c8normalize `(let* ,(cdr defs) ,body) k))))))))))

(define c8if
  (lambda (args k)
    (let ((m1 (car args))
          (m2 (cadr args))
          (m3 (caddr args)))
      (c8normalize-name m1
                        (lambda (t)
                          (k `(if ,t
                                  ,(c8normalize-term m2)
                                  ,(c8normalize-term m3))))))))

(define c8set!
  (lambda (args k)
    (let ((var (car args))
          (exp (cadr args)))
      (c8normalize-name exp
                        (lambda (aexp)
                          `(let ((,(newvar "set!_") (set! ,var ,aexp)))
                             ,(k aexp)))))))

(define c8begin
  (lambda (args k)
    (cond ((null? args)
           (k '()))
          ((null? (cdr args))
           (c8normalize (car args) k))
          (else
           (c8normalize-name (car args)
                             (lambda (aexp)
                               (c8normalize `(begin ,@(cdr args)) k)))))))

(define c8letrec
  (lambda (args k)
    (let ((defs (car args))
          (body (cadr args)))
      (if (null? defs)
          (c8normalize body k)
          (let ((vars (map car defs))
                (exps (map cadr defs)))
            (let ((inits (make-list (length vars) #f))
                  (set!s (make-list (length vars) 'set!)))
              (c8normalize `(let* ,(map list vars inits)
                              (begin ,@(map list set!s vars exps)
                                     ,body))
                           k)))))))

(define c8quote
  (lambda (args k)
    (let ((exp (car args)))
      (if (c.scm:self-eval? exp)
          (k exp)
          (k `(quote ,exp))))))

(define c8symbol-fun
  (lambda (fn m* k)
    (if (c8primitive? fn)
        (c8normalize-name* m* (lambda (t*) (k `(,fn . ,t*))))
        (c8normalize-name fn (lambda (t) (c8normalize-name* m* (lambda (t*) (k `(,t . ,t*)))))))))

(define c8normalize-name
  (lambda (m k)
    (c8normalize m (lambda (n)
                     (if (c8value? n)
                         (k n)
                         (let ((t (newvar "anf")))
                           `(let ((,t ,n))
                              ,(k t))))))))

(define c8normalize-name*
  (lambda (m* k)
    (if (null? m*)
        (k '())
        (c8normalize-name (car m*)
                          (lambda (t)
                            (c8normalize-name* (cdr m*)
                                               (lambda (t*) (k `(,t . ,t*)))))))))

(define c8value?
  (lambda (x)
    (or (symbol? x)
        (null? x)
        (c.scm:self-eval? x))))

(define c8*primitive* (list 'eqv? 'eq? 

                            'number? 'complex? 'real? 'rational? 'integer?
                            'exact? 'inexact?
                            '= '< '> '<= '>=
                            '+ '* '- '/ 
                            'quotient 'remainder 'modulo
                            'numerator 'denominator
                            'floor 'ceiling 'truncate 'round
                            'exp 'log 'sin 'cos 'tan 'asin 'acos 'atan
                            'sqrt 'expt
                            'make-rectangular 'make-polar 'real-part 'imag-part 'magnitude 'angle
                            'exact->inexact 'inexact->exact
                            'number->string 'string->number
                            
                            'pair? 'cons 'car 'cdr 'set-car! 'set-cdr!
                            'caar 'cadr 'cddr

                            'symbol?
                            'symbol->string 'string->symbol

                            'char? 'char=? 'char<? 'char>? 'char<=? 'char>=?
                            'char->integer 'integer->char
                            
                            'string?
                            'make-string
                            'string-length 'string-ref 'string-set!


                            'vector?
                            'make-vector 'vector-length 'vector-ref 'vector-set!

                            'procedure?
                            'apply))
(define c8*library* (list 'equal?

                          'zero? 'positive? 'negative? 'odd? 'even?
                          'max 'min
                          'abs
                          'gcd 'lcm
                          'rationalize

                          'not
                          'boolean?

                          'caaar 'caadr 'cadar 'caddr 'cdaar 'cdadr 'cddar 'cdddr
                          'caaaar 'caaadr 'caadar 'caaddr 'cadaar 'cadadr 'caddar 'cadddr
                          'cdaaar 'cdaadr 'cdadar 'cdaddr 'cddaar 'cddadr 'cdddar 'cddddr
                          'null? list?
                          'list 'length 'append 'reverse 'list-tail 'list-ref
                          'memq 'memv 'member
                          'assq 'assv 'assoc

                          'char-ci=? 'char-ci<? 'char-ci>? 'char-ci<=? 'char-ci>=?
                          'char-alphabetic? 'char-numeric? 'char-whitespace? 'char-upper-case? 'char-lower-case?
                          'char-upcase 'char-downcase

                          'string
                          'string=? 'string-ci=?
                          'string<? 'string>? 'string<=? 'string>=?
                          'string-ci<? 'string-ci>? 'string-ci<=? 'string-ci>=?
                          'substring
                          'string-append
                          'string->list 'list->string
                          'string-copy
                          'string-fill!
                          
                          'vector
                          'vector->list 'list->vector
                          'vector-fill!

                          'map 'for-each 'force))

(define c8primitive?
  (lambda (x)
    (or (memq x c8*primitive*)
        (memq x c8*library*))))
