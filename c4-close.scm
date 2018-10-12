;;; 自由変数のクロージングを行う
;;; letrec式の束縛内になる自由変数を持つlambda式の引数に自由変数を追加する
;;; 自由変数を追加したローカル関数呼び出しの引数に自由変数を追加する

;; (define var (lambda requireds rest body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
(define (c.scm:c4close x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c4close-function (car x) ;; define
                                            (cadr x) ;; name
                                            (caddr x)) ;; (lambda params body)
                  `(,(car x) ,(cadr x) ,(c.scm:c4close-expr form)))))
           ((begin)
            `(begin ,@(map c.scm:c4close (cdr x))))
           (else
            (c.scm:c4close-expr x)))
         (c.scm:c4close-expr x)))

(define (c.scm:c4close-function first name lambda-expr)
  (let ((x (c4expr lambda-expr)))
    `(,first ,name ,x)))

;; form->expr
(define (c.scm:c4close-expr form)
  (let ((x (c4expr form)))
    x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c4expr form)
  (cond ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
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
    (list 'lambda (c.scm:union (if (null? free-vars)
                                   '()
                                   (car free-vars))
                               params)
          (c4expr body))))

(define (c4let args)
  (if (c.scm:var? (car args))
      (c4named-let args)     
      (let loop ((defs (car args))
                 (cdefs '()))
        (cond ((null? defs)
               `(let ,(reverse cdefs) ,(c4expr (cadr args))))
          (else
           (loop (cdr defs)
                 (cons (c4def (car defs))
                       cdefs)))))))

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

#;(define (c4named-let args)
  (let loop ((defs (cadr args))
             (cdefs '()))
    (cond ((null? defs)
           `(let ,(car var) ,(reverse cdefs) ,(c4expr (caddr args))))
          (else
           (loop (cdr defs)
                 (cons (c4def (car defs))
                       cdefs))))))

(define (c4set! args)
  `(set! ,(car args) ,(c4expr (cadr args))))

(define (c4quote args)
  `(quote ,(car args)))

(define (c4symbol-fun fun args)
  (if (and (c.scm:var? fun)
           (var-local-fun fun))
      `(,fun ,@(var-local-fun fun) ,@(c4args args)) ;; ローカル関数呼び出し
      `(,fun ,@(c4args args))))

(define (c4args args)
  (if (null? args)
      '()
      (cons (c4expr (car args))
            (c4args (cdr args)))))

