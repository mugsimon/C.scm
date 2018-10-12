;;; c.scm:c12assignはホイストが終了した関数を受け取り、set!があれば#t、なければ#fを返す
;; (define var (lambda params body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
(define (c.scm:c12assign x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c12function (car x) ;; define
                                     (cadr x) ;; name
                                     (caddr x)) ;; (lambda params body)
                  `(,(car x) ,(cadr x) ,(c12expr form)))))
           (else
            (c12expr x)))
         (c12expr x)))

(define (c.scm:c12function first name lambda-expr)
  (let ((body (caddr lambda-expr)))
    (c12expr body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c12expr form)
  (cond ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c12if args))
                    ((and) (c12and args))
                    ((or) (c12or args))
                    ((begin) (c12begin args))
                    ((lambda) (c12lambda args))
                    ((delay) (c12delay args))
                    ((let) (c12let args))
                    ((let*) (c12let args))
                    ((letrec) (c12letrec args))
                    ((set!) (c12set! args))
                    ((quote) (c12quote args))
                    (else
                     (c12symbol-fun fun args))))
                 (else
                  (or (c12expr fun) (c12args args))))))
        (else
         #f)))

(define (c12if args)
  (or (c12expr (car args))
      (c12expr (cadr args))
      (c12expr (caddr args))))

(define (c12and args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c12expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c12or args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c12expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c12begin args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c12expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c12lambda args)
  (let ((params (car args))
        (body (cadr args)))
    (c12expr body)))

(define (c12let args)
  (if (c.scm:var? (car args))
      (c12named-let args)
      (let loop ((defs (car args)))
        (cond ((null? defs)
               (c12expr (cadr args)))
              (else
               (let ((def (car defs)))
                 (if (c12expr (cadr def))
                     #t
                     (loop (cdr defs)))))))))

(define (c12named-let args)
  (let loop ((defs (cadr args)))
    (cond ((null? defs)
           (c12expr (caddr args)))
          (else
           (let ((def (car defs)))
             (if (c12expr (cadr def))
                 #t
                 (loop (cdr defs))))))))
                   
(define (c12letrec args)
  (let loop ((defs (car args)))
    (cond ((null? defs)
           (c12expr (cadr args)))
          (else
           (let ((def (car defs)))
             (if (c12expr (cadr def))
                 #t
                 (loop (cdr defs))))))))

(define (c12set! args)
  #t)

(define (c12quote args)
  #f)

(define (c12symbol-fun fun args)
  (c12args args))

(define (c12args args)
  (if (null? args)
      #f
      (or (c12expr (car args))
          (c12args (cdr args)))))
