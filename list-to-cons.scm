;; c0後のnamedletをletrecに展開する
;; (define var (lambda params body))
;; (define var expr)
;; sexp
(define (c.scm:s1list-to-cons x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c.scm:s1function (car x) ;; define
                                     (cadr x) ;; name
                                     (caddr x)) ;; (lambda params body)
               `(,(car x) ,(cadr x) ,(s1expr form)))))
        (else
         (s1expr x)))
      (s1expr x)))

(define (c.scm:s1function first name lambda-expr)
  (let ((x (s1expr lambda-expr)))
    `(,first ,name ,x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (s1expr form)
  ;; (print "c.scm:debug, s1expr, form -> " form) ;; debug
  (cond ((pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((symbol? fun)
                  (case fun
                    ((if) (s1if args))
                    ((and) (s1and args))
                    ((or) (s1or args))
                    ((begin) (s1begin args))
                    ((lambda) (s1lambda args))
                    ((delay) (s1delay args))
                    ((let) (s1let args))
                    ((let*) (s1let* args))
                    ((letrec) (s1letrec args))
                    ((set!) (s1set! args))
                    ((quote) (s1quote args))
                    ((list) (s1list args))
                    (else
                     (s1symbol-fun fun args))))
                 (else
                  `(,(s1expr fun) ,@(s1args args))))))
        (else
         (case form
           ((#f) #f)
           ((#t) #t)
           ((()) '())
           (else
            form)))))

(define (s1if args)
  `(if ,(s1expr (car args))
       ,(s1expr (cadr args))
       ,(s1expr (caddr args))))

(define (s1and args)
  `(and ,@(map s1expr args)))

(define (s1or args)
  `(or ,@(map s1expr args)))

(define (s1begin args)
  `(begin ,@(map s1expr args)))

(define (s1lambda args)
  (let ((params (car args))
        (body (cadr args)))
    (list 'lambda params (s1expr body))))

(define (s1let args)
  (if (symbol? (car args))
      (s1named-let args)
      (let loop ((defs (car args))
                 (cdefs '()))
        (cond ((null? defs)
               (if (null? cdefs)
                   (s1expr (cadr args))
                   `(let ,(reverse cdefs) ,(s1expr (cadr args)))))
              (else
               (let ((def (car defs)))
                 (loop (cdr defs)
                       (cons (list (car def)
                                   (s1expr (cadr def)))
                             cdefs))))))))

(define (s1named-let args)
  (let loop ((defs (cadr args))
             (cdefs '()))
    (cond ((null? defs)
           (let ((var (car args))
                 (body (caddr args)))
             `(let ,var ,(reverse cdefs) ,(c10expr body))))
          (else
           (let ((def (car defs)))
             (loop (cdr defs)
                   (cons (list (car def)
                               (c10expr (cadr def)))
                         cdefs)))))))

(define (s1let* args)
  (let ((defs (car args))
        (body (s1expr (cadr args))))
    (let ((vars (map car defs))
          (exps (map cadr defs)))
      (let ((cexps (map s1expr exps)))
        (let ((cdefs (map list vars cexps)))
          (if (null? cdefs)
              body
              `(let* ,cdefs ,body)))))))

(define (s1letrec args)
  (let ((defs (car args))
        (body (s1expr (cadr args))))
    (let ((vars (map car defs))
          (exps (map cadr defs)))
      (let ((cexps (map s1expr exps)))
        (let ((cdefs (map list vars cexps)))
          (if (null? cdefs)
              body
              `(letrec ,cdefs ,body)))))))
              
(define (s1set! args)
  `(set! ,(car args) ,(s1expr (cadr args))))

(define (s1quote args)
  `(quote ,@args))

(define (s1symbol-fun fun args)
  `(,fun ,@(s1args args)))

(define (s1args args)
  (if (null? args)
      '()
      (cons (s1expr (car args))
            (s1args (cdr args)))))

(define (s1list args)
  (if (null? args)
      '()
      `(cons ,(s1expr (car args))
             ,(s1list (cdr args)))))
