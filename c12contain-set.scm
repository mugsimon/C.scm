;;; c.scm:c12assignはホイストが終了した関数を受け取る
;;; クローズされるローカル変数、グローバル変数への代入があれば#tを返す
;; (define var (lambda params body))
;; (define var expr)

(define (c12contain-set!? x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c12def-func first
                            name
                            form)
               (c12def-expr first
                            name
                            form))))
        (else
         (error "CSCM:ERROR, c12contain-set!?, not a definition" x)))
      (error "CSCM:ERROR, c12contain-set!?, not a definition" x)))

(define (c12def-func first name lambda-expr)
  (if (and (cscm:var? name)
           (var-assigned name)) ;; トップレベル関数への代入があるならCにはできない（なくても良い？？0122）
      #f
      (let ((body (caddr lambda-expr)))
        (c12expr body))))

(define (c12def-expr first name expr)
  (if (and (cscm:var? name)
           (var-assigned name))
      #f
      (c12expr expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c12expr form)
  (cond ((cscm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((cscm:symbol? fun)
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
  (cond ((null? args)
         #f)
        ((null? (cdr args))
         (c12expr (car args)))
        (else
         (c12args args))))

(define (c12or args)
  (cond ((null? args)
         #f)
        ((null? (cdr args))
         (c12expr (car args)))
        (else
         (c12args args))))

(define (c12begin args)
  (cond ((null? args)
         #f)
        ((null? (cdr args))
         (c12expr (car args)))
        (else
         (c12args args))))

(define (c12lambda args)
  (let ((params (car args))
        (body (cadr args)))
    (c12expr body)))

(define (c12let args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((exps (map cadr defs)))
      (or (c12args exps)
          (c12expr body)))))

(define (c12letrec args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((exps (map cadr defs)))
      (or (c12args exps)
          (c12expr body)))))

(define (c12set! args)
  (let ((var (car args)) ;; 変数
        (exp (cadr args))) ;; 値
    (if (and (cscm:var? var) ;; make-var
             (var-assigned var) ;; 代入される
             (not (var-closed var))) ;; lambda式に閉じ込められない
        (c12expr exp) ;; クローズされない変数への代入は問題ない
        (if (not (cscm:var? var))
            (c12expr exp) ;; グローバル変数も問題ない
            (begin (print "cscm:debug, c12set!, 代入を発見しました" args) ;; debug
                   #t)))))

(define (c12quote args)
  #f)

(define (c12symbol-fun fun args)
  (c12args args))

(define (c12args args)
  (if (null? args)
      #f
      (or (c12expr (car args))
          (c12args (cdr args)))))
