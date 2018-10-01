;;; c.scm:c5raw-lambdaはc4ホイストが終了した関数を受け取り、生のlambda式があれば#t、なければ#fを返す
;;; c5lamはホイストされたローカル関数を受け取り、生のlambda式があれば#t、なければ#fを返す
;; (define var (lambda requireds rest body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
(define (c.scm:c5raw-lambda x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c5raw-lambda-function (car x) ;; define
                                               (cadr x) ;; name
                                               (caddr x)) ;; (lambda required rest body)
                  `(,(car x) ,(cadr x) ,(c.scm:c5raw-lambda-expr form)))))
           ((begin)
            `(begin ,@(map c.scm:c5raw-lambda (cdr x))))
           (else
            (c.scm:c5raw-lambda-expr x)))
         (c.scm:c5raw-lambda-expr x)))

(define (c.scm:c5raw-lambda-function first name lambda-expr)
  (c5lam (cdr lambda-expr)))

;; form->expr
(define (c.scm:c5raw-lambda-expr form)
  (c5expr form))

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
(define (c5expr form)
  (cond ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c5if args))
                    ((and) (c5and args))
                    ((or) (c5or args))
                    ((begin) (c5begin args))
                    ((lambda) (c5lambda args))
                    ((delay) (c5delay args))
                    ((let) (c5let args))
                    ((let*) (c5let args))
                    ((letrec) (c5letrec args))
                    ((set!) (c5set! args))
                    ((quote) (c5quote args))
                    (else
                     (c5symbol-fun fun args))))
                 (else
                  (list (c5expr fun) (c5args (car args)))))))
        (else
         #f)))
(define (c5if args)
  (or (c5expr (car args))
      (c5expr (cadr args))
      (c5expr (caddr args))))

(define (c5and args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c5expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c5or args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c5expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c5begin args)
  (let loop ((args args))
    (cond ((null? args)
           #f)
          ((c5expr (car args))
           #t)
          (else
           (loop (cdr args))))))

(define (c5lambda args)
  #t)

(define (c5lam args)
  (let ((requireds (car args))
        (rest (cadr args))
        (body (caddr args)))
    (c5expr body)))

(define (c5let args)
  (let loop ((defs (car args)))
    (cond ((null? defs)
           (c5expr (cadr args)))
          (else
           (let ((def (car defs)))
             (if (c5expr (cdr def))
                 #t
                 (loop (cdr defs))))))))
                   
(define (c5letrec args)
  (let loop ((defs (car args)))
    (cond ((null? defs)
           (c5expr (cadr args)))
          (else
           (let ((var (caar defs))
                 (form (cadar defs))
                 (form0 (caddar defs)))
             (if (c5expr form)
                 #t
                 (loop (cdr defs))))))))

(define (c5set! args)
  (c5expr (cadr args)))

(define (c5quote args)
  #f)

(define (c5symbol-fun fun args)
  (c5args (car args)))

(define (c5args args)
  (if (null? args)
      #f
      (or (c5expr (car args))
          (c5args (cdr args)))))
