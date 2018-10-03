;;; c.scm:c6raw-lambdaはホイストが終了した関数を受け取り、生のlambda式があれば#t、なければ#fを返す
;; (define var (lambda params body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
(define (c.scm:c6raw-lambda x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c6raw-lambda-function (car x) ;; define
                                               (cadr x) ;; name
                                               (caddr x)) ;; (lambda params body)
                  `(,(car x) ,(cadr x) ,(c.scm:c6raw-lambda-expr form)))))
           ((begin)
            `(begin ,@(map c.scm:c6raw-lambda (cdr x))))
           (else
            (c.scm:c6raw-lambda-expr x)))
         (c.scm:c6raw-lambda-expr x)))

(define (c.scm:c6raw-lambda-function first name lambda-expr)
  (let ((body (caddr lambda-expr)))
    (c6expr body)))

;; form->expr
(define (c.scm:c6raw-lambda-expr form)
  (c6expr form))

(define (c.scm:symbol? x)
  (or (c.scm:var? x)
      (symbol? x)))

(define (c.scm:pair? x)
  (and (not (c.scm:var? x))
       (pair? x)))

(define (c.scm:var? x)
  (and (list? x)
       (= (length x) 7)
       (symbol? (var-name x))
       (boolean? (var-funarg x))
       (boolean? (var-assigned x))
       (boolean? (var-closed x))
       (or (boolean? (var-local-fun x))
           (list? (var-local-fun x)))
       (list? (var-local-fun-args x))
       (list? (var-loc x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c6expr form)
  (cond ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c6if args))
                    ((and) (c6and args))
                    ((or) (c6or args))
                    ((begin) (c6begin args))
                    ((lambda) (c6lambda args))
                    ((delay) (c6delay args))
                    ((let) (c6let args))
                    ((let*) (c6let args))
                    ((letrec) (c6letrec args))
                    ((set!) (c6set! args))
                    ((quote) (c6quote args))
                    (else
                     (c6symbol-fun fun args))))
                 (else
                  (or (c6expr fun) (c6args args))))))
        (else
         #f)))

(define (c6if args)
  (or (c6expr (car args))
      (c6expr (cadr args))
      (c6expr (caddr args))))

(define (c6and args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c6expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c6or args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c6expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c6begin args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c6expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c6lambda args)
  #t)

(define (c6let args)
  (if (c.scm:var? (car args))
      (c6named-let args)
      (let loop ((defs (car args)))
        (cond ((null? defs)
               (c6expr (cadr args)))
              (else
               (let ((def (car defs)))
                 (if (c6expr (cadr def))
                     #t
                     (loop (cdr defs)))))))))

(define (c6named-let args)
  (let loop ((defs (cadr args)))
    (cond ((null? defs)
           (c6expr (caddr args)))
          (else
           (let ((def (car defs)))
             (if (c6expr (cadr def))
                 #t
                 (loop (cdr defs))))))))
                   
(define (c6letrec args)
  (let loop ((defs (car args)))
    (cond ((null? defs)
           (c6expr (cadr args)))
          (else
           (let ((def (car defs)))
             (if (c6expr (cadr def))
                 #t
                 (loop (cdr defs))))))))

(define (c6set! args)
  (c6expr (cadr args)))

(define (c6quote args)
  #f)

(define (c6symbol-fun fun args)
  (c6args args))

(define (c6args args)
  (if (null? args)
      #f
      (or (c6expr (car args))
          (c6args (cdr args)))))
