;;; c.scm:c13gcはホイストが終了した関数を受け取り、cons, listがあれば#t、なければ#fを返す
;; (define var (lambda params body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
(define (c.scm:c13gc x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c13function (car x) ;; define
                                     (cadr x) ;; name
                                     (caddr x)) ;; (lambda params body)
                  `(,(car x) ,(cadr x) ,(c13expr form)))))
           (else
            (c13expr x)))
         (c13expr x)))

(define (c.scm:c13function first name lambda-expr)
  (let ((body (caddr lambda-expr)))
    (c13expr body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c13expr form)
  (cond ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c13if args))
                    ((and) (c13and args))
                    ((or) (c13or args))
                    ((begin) (c13begin args))
                    ((lambda) (c13lambda args))
                    ((delay) (c13delay args))
                    ((let) (c13let args))
                    ((let*) (c13let args))
                    ((letrec) (c13letrec args))
                    ((set!) (c13set! args))
                    ((quote) (c13quote args))
                    ((list #;cons) (c13gc args)) ;; c13gc only
                    (else
                     (c13symbol-fun fun args))))
                 (else
                  (or (c13expr fun) (c13args args))))))
        (else
         #f)))

(define (c13if args)
  (or (c13expr (car args))
      (c13expr (cadr args))
      (c13expr (caddr args))))

(define (c13and args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c13expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c13or args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c13expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c13begin args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c13expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c13lambda args)
  (let ((params (car args))
        (body (cadr args)))
    (c13expr body)))

(define (c13let args)
  (if (c.scm:var? (car args))
      (c13named-let args)
      (let loop ((defs (car args)))
        (cond ((null? defs)
               (c13expr (cadr args)))
              (else
               (let ((def (car defs)))
                 (if (c13expr (cadr def))
                     #t
                     (loop (cdr defs)))))))))

(define (c13named-let args)
  (let loop ((defs (cadr args)))
    (cond ((null? defs)
           (c13expr (caddr args)))
          (else
           (let ((def (car defs)))
             (if (c13expr (cadr def))
                 #t
                 (loop (cdr defs))))))))
                   
(define (c13letrec args)
  (let loop ((defs (car args)))
    (cond ((null? defs)
           (c13expr (cadr args)))
          (else
           (let ((def (car defs)))
             (if (c13expr (cadr def))
                 #t
                 (loop (cdr defs))))))))

(define (c13set! args)
  (let ((form (cadr args)))
    (c13expr form)))

(define (c13quote args)
  #f)

(define (c13symbol-fun fun args)
  (c13args args))

(define (c13args args)
  (if (null? args)
      #f
      (or (c13expr (car args))
          (c13args (cdr args)))))

(define (c13gc args)
  #t)
