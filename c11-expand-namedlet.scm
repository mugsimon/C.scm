;; c0後のnamedletをletrecに展開する
;; (define var (lambda params body))
;; (define var expr)
;; sexp
(define (c.scm:c11expand-namedlet x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c.scm:c11function (car x) ;; define
                                     (cadr x) ;; name
                                     (caddr x)) ;; (lambda params body)
               `(,(car x) ,(cadr x) ,(c11expr form)))))
        (else
         (c11expr x)))
      (c11expr x)))

(define (c.scm:c11function first name lambda-expr)
  (let ((x (c11expr lambda-expr)))
    `(,first ,name ,x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c11expr form)
  (cond ((pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((symbol? fun)
                  (case fun
                    ((if) (c11if args))
                    ((and) (c11and args))
                    ((or) (c11or args))
                    ((begin) (c11begin args))
                    ((lambda) (c11lambda args))
                    ((delay) (c11delay args))
                    ((let) (c11let args))
                    ((let*) (c11let* args))
                    ((letrec) (c11letrec args))
                    ((set!) (c11set! args))
                    ((quote) (c11quote args))
                    (else
                     (c11symbol-fun fun args))))
                 (else
                  `(,(c11expr fun) ,@(c11args args))))))
        (else
         (case form
           ((#f) #f)
           ((#t) #t)
           ((()) '())
           (else
            form)))))

(define (c11if args)
  `(if ,(c11expr (car args))
       ,(c11expr (cadr args))
       ,(c11expr (caddr args))))

(define (c11and args)
  `(and ,@(map c11expr args)))

(define (c11or args)
  `(or ,@(map c11expr args)))

(define (c11begin args)
  `(begin ,@(map c11expr args)))

(define (c11lambda args)
  (let ((params (car args))
        (body (cadr args)))
    (list 'lambda params (c11expr body))))

(define (c11let args)
  (if (symbol? (car args))
      (c11named-let args)
      (let loop ((defs (car args))
                 (cdefs '()))
        (cond ((null? defs)
               (if (null? cdefs)
                   (c11expr (cadr args))
                   `(let ,(reverse cdefs) ,(c11expr (cadr args)))))
              (else
               (let ((def (car defs)))
                 (loop (cdr defs)
                       (cons (list (car def)
                                   (c11expr (cadr def)))
                             cdefs))))))))
                    
(define (c11named-let args)
  (let ((name (car args))
        (defs (cadr args))
        (body (c11expr (caddr args))))
    (let ((vars (map car defs))
          (exps (map c11expr (map cadr defs))))
      `(letrec ((,name (lambda ,vars
                         ,body)))
         (,name ,@exps)))))

(define (c11let* args)
  (let ((defs (car args))
        (body (c11expr (cadr args))))
    (let ((vars (map car defs))
          (exps (map cadr defs)))
      (let ((cexps (map c11expr exps)))
        (let ((cdefs (map list vars cexps)))
          (if (null? cdefs)
              body
              `(let* ,cdefs ,body)))))))

(define (c11letrec args)
  (let ((defs (car args))
        (body (c11expr (cadr args))))
    (let ((vars (map car args))
          (exps (map cadr args)))
      (let ((cexps (map c11expr exps)))
        (let ((cdefs (map list vars cexps)))
          (if (null? cdefs)
              body
              `(letrec ,cdefs ,body)))))))
              
(define (c11set! args)
  `(set! ,(car args) ,(c11expr (cadr args))))

(define (c11quote args)
  `(quote ,@args))

(define (c11symbol-fun fun args)
  `(,fun ,@(c11args args)))

(define (c11args args)
  (if (null? args)
      '()
      (cons (c11expr (car args))
            (c11args (cdr args)))))
