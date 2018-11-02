;;; 自由変数をクローズする
;;; letrec式の束縛のうち、local-funが#tのlambda式の引数に自由変数を追加する
;;; 自由変数を追加したローカル関数呼び出しの引数に自由変数を追加する

;; (define var (lambda params body))
;; (define var expr)
(define (c4close x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c4def-func first
                           name
                           form)
               (c4def-expr first
                           name
                           form))))
        (else
         (error "CSCM:ERROR, c4close, not a definition" x)))
      (error "CSCM:ERROR, c4close, not a definition" x)))

(define (c4def-func first name lambda-expr)
  (let ((x (c4expr lambda-expr)))
    `(,first ,name ,x)))

(define (c4def-expr first name expr)
  (let ((x (c4expr expr)))
    `(,first ,name ,x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c4expr form)
  (cond ((cscm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((cscm:symbol? fun)
                  (case fun
                    ((if) (c4if args))
                    ((and) (c4and args))
                    ((or) (c4or args))
                    ((begin) (c4begin args))
                    ((lambda) (c4lambda args))
                    ((delay) (c4delay args))
                    ((let) (c4let args))
                    ((let*) (c4let* args))
                    ((letrec) (c4letrec args))
                    ((set!) (c4set! args))
                    ((quote) (c4quote args))
                    (else
                     (c4symbol-fun fun args))))
                 (else
                  `(,(c4expr fun) ,@(c4args args))))))
        (else
         (case form
           ((#f) #f)
           ((#t) #t)
           ((()) '())
           (else
            form)))))

(define (c4if args)
  `(if ,(c4expr (car args))
       ,(c4expr (cadr args))
       ,(c4expr (caddr args))))

(define (c4and args)
  `(and ,@(map c4expr args)))

(define (c4or args)
  `(or ,@(map c4expr args)))

(define (c4begin args)
  `(begin ,@(map c4expr args)))  

(define (c4lambda args . free-vars)
  (let ((params (car args))
        (body (cadr args)))
    (list 'lambda (cscm:union (if (null? free-vars)
                                  '()
                                  (car free-vars))
                              params)
          (c4expr body))))

(define (c4let args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(let ,(reverse cdefs) ,(c4expr (cadr args))))
          (else
           (loop (cdr defs)
                 (cons (c4def (car defs))
                       cdefs))))))

(define (c4let* args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(let* ,(reverse cdefs) ,(c4expr (cadr args))))
          (else
           (loop (cdr defs)
                 (cons (c4def (car defs))
                       cdefs))))))

(define (c4def def)
  (let ((var (car def))
        (form (cadr def)))
    (list var (if (var-local-fun var)
                  (c4lambda (cdr form) (var-local-fun var))
                  (c4expr form)))))

(define (c4letrec args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(letrec ,(reverse cdefs) ,(c4expr (cadr args))))
          (else
           (loop (cdr defs)
                 (cons (c4def (car defs))
                       cdefs))))))

(define (c4set! args)
  (let ((var (car args))
        (exp (cadr args)))
  `(set! ,var ,(c4expr exp))))

(define (c4quote args)
  `(quote ,(car args)))

(define (c4symbol-fun fun args)
  (if (and (cscm:var? fun)
           (var-local-fun fun))
      `(,fun ,@(var-local-fun fun) ,@(c4args args)) ;; ローカル関数呼び出し
      `(,fun ,@(c4args args))))

(define (c4args args)
  (if (null? args)
      '()
      (cons (c4expr (car args))
            (c4args (cdr args)))))

