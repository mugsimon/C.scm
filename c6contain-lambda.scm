;;; ホイストが後の定義を受け取る
;;; lambda式があれば#tを返す

;; (define var (lambda params body))
;; (define var expr)

(define (c6contain-lambda? x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c6def-func first
                           name
                           form)
               (c6def-expr first
                           name
                           form))))
        (else
         (error "CSCM:ERROR, c6contain-lambda?, not a definition" x)))
      (error "CSCM:ERROR, c6contain-lambda?, not a definition" x)))

;;(define (c6def-func first name lambda-expr)
;;  (let ((body (caddr lambda-expr)))
;;    (c6expr body)))

(define (c6def-func first name lambda-expr)
  (let ((params (cadr lambda-expr)) ;; lambda式の引数
        (body (caddr lambda-expr))) ;; lambda式の本体
    (let ((n (if (or (null? params)
                     (cscm:pair? params))
                 (length params)
                 1)))
      (if (> n 10)
          (begin (print "cscm:debug, c6def-func, 引数が暫定10以上の関数定義です: " n) ;; debug
                 #f);;#t)
          (c6expr body)))))

(define (c6def-expr first name expr)
  (c6expr expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c6expr form)
  (cond ((cscm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((cscm:symbol? fun)
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
                    ;;((quote) (c6quote args))
                    ;;((list) #t) ;; 試験導入
                    ;;((map) #t) ;;試験導入
                    ;;((apply) #t) ;;試験導入
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
  (cond ((null? args)
         #f)
        ((null? (cdr args))
         (c6expr (car args)))
        (else
         (c6args args))))

(define (c6or args)
    (cond ((null? args)
         #f)
        ((null? (cdr args))
         (c6expr (car args)))
        (else
         (c6args args))))

(define (c6begin args)
  (cond ((null? args)
         #f)
        ((null? (cdr args))
         (c6expr (car args)))
        (else
         (c6args args))))

(define (c6lambda args)
  ;;(print "cscm:debug, c6lambda, 内部lambda式を発見しました" args) ;; debug
  #t)

(define (c6let args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((exps (map cadr defs)))
      (or (c6args exps)
          (c6expr body)))))
                   
(define (c6letrec args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((exps (map cadr defs)))
      (or (c6args exps)
          (c6expr body)))))

(define (c6set! args)
  (let ((var (car args))
        (exp (cadr args)))
  (c6expr exp)))

;;(define (c6quote args)
;;  #f)


(define (c6quote args)
  (let ((x (car args)))
    ;;(print "cscm:debug, c6quote, args -> " args) ;; debug
    (if (or (cscm:self-eval? x)
            (symbol? x)
            (null? x))
        #f
        #f)));;#t)))

(define (c6symbol-fun fun args)
  (c6args args))

(define (c6args args)
  (if (null? args)
      #f
      (or (c6expr (car args))
          (c6args (cdr args)))))
