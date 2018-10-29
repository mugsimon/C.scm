;; c16の解析結果をもとに変形を行う
;; ここで行う変形は、cscmとなった関数内部でcscmではない関数呼び出しがあるとき、これを書き換える

;;(define (var-name var) (car var)) ;; 変数名
;;(define (var-toplevel var) (cadr var)) ;; もともとトップレベルの式->#t
;;(define (var-assigned var) (caddr var)) ;; 代入される->#t
;;(define (var-cscm var) (cadddr var)) ;; Cになる->#t
;;(define (var-local-fun var) (car (cddddr var))) ;; ホイストされた関数->#t
;;(define (var-cons var) (cadr (cddddr var))) ;; コンストラクタを持つ->#t
;;(define (var-loc var) (caddr (cddddr var)))

;;
(define c16*define* #f)

(define (c.scm:c16 x)
  ;;(print "c.scm:debug, c.scm:c16, x -> " x) ;; debug
  (if (c.scm:pair? x)
      (case (car x)
        ((define) ;; (define name (lambda params body)) || (define name expr)
         (let ((form (caddr x))) ;; (lambda params body) || expr
           (if (and (c.scm:pair? form)
                    (eq? (car form) 'lambda))
               (c.scm:c16function (car x) ;; define
                                  (cadr x) ;; name
                                  (caddr x)) ;; (lambda params body)
               (c.scm:c16expr (car x)
                              (cadr x)
                              (caddr x)))))
        (else
         (c16expr x)))
      (c16expr x)))

(define (c.scm:c16function first name lambda-expr)
  (dlet ((c16*define* name))
        (let ((x (c16expr lambda-expr)))
          `(,first ,c16*define* ,x))))

(define (c.scm:c16expr first name expr)
  (dlet ((c16*define* name))
        (let ((x (c16expr expr)))
          `(,first ,c16*define* ,x))))

;;;;;;;;;;;;;;;;;;;;;;
(define (c16constant x)
  (if (c.scm:self-eval? x)
      x
      `(quote ,x)))
(define c16null
  `'())
(define c16false
  #f)
(define c16true
  #t)

;; if
(define (c16if args)
  (let ((t (c16fmla (car args)))
        (m1 (c16expr (cadr args)))
        (m2 (c16expr (caddr args))))
    `(if ,t ,m1 ,m2)))

;; if fmla
(define (c16fmla fmla)
  (if (pair? fmla)
      (case (car fmla)
        ((and)
         (cons 'and (map c16fmla (cdr fmla))))
        ((or)
         (cons 'or (map c16fmla (cdr fmla))))
        ((not)
         (list 'not (c16fmla (cadr fmla))))
        (else
         (c16expr fmla)))
      (c16expr fmla)))

;; and
(define (c16and args)
  `(and ,@(c16args args)))
              
;; or
(define (c16or args)
  `(or ,@(c16args args)))

(define (c16expr form)
  ;;(print "c.scm:debug, c16expr, form -> " form) ;; debug
  (cond ((c.scm:symbol? form)
         (c16vref form))
        ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c16if args))
                    ((and) (c16and args))
                    ((or) (c16or args))
                    ((begin) (c16begin args))
                    ((lambda) (c16lambda args))
                    ((delay) (c16delay args))
                    ((let) (c16let args))
                    ((let*) (c16let* args))
                    ((letrec) (c16letrec args))
                    ((set!) (c16set! args))
                    ((quote) (c16quote args))
                    (else
                     (c16symbol-fun fun args))))
                 (else
                  `(,(c16expr fun) ,@(c16args args))))))
        (else
         (case form
           ((#f) c16false)
           ((#t) c16true)
           ((()) c16null)
           (else (c16constant form))))))

;; (f args)
(define (c16symbol-fun name args)
  (if (and (c.scm:var? name)
           (not (var-cscm name))
           (var-cscm c16*define*)
           (or (var-local-fun name) ;; この２つは念のため程度なきがする
               (var-toplevel name)))
      (let ((n (length args)))
        (case n
          ((0) `(cscm_apply0 ,name ,@(c16args args)))
          ((1) `(cscm_apply1 ,name ,@(c16args args)))
          ((2) `(cscm_apply2 ,name ,@(c16args args)))
          ((3) `(cscm_apply3 ,name ,@(c16args args)))))
      `(,name ,@(c16args args))))

;;
(define (c16args forms)
  (if (null? forms)
      '()
      (cons (c16expr (car forms))
            (c16args (cdr forms)))))

;;
(define (c16begin forms)
  `(begin ,@(map c16expr forms)))

(define (c16body body)
  (c16expr body))

;;
(define (c16lambda args)
  (let ((params (car args))
        (body (cadr args)))
    (list `lambda params (c16expr body))))
          
(define (c16let args)
  (let ((body (cadr args))
        (defs (car args)))
    (let ((vars (map car defs))
          (exps (map c16expr (map cadr defs))))
      `(let ,(map list vars expr) ,(c16expr body)))))

(define (c16let* args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((vars (map car defs))
          (exps (map c16expr (map cadr defs))))
      `(let* ,(map list vars expr) ,(c16expr body)))))

(define (c16letrec args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((vars (map car defs))
          (exps (map c16expr (map cadr defs))))
      `(letrec ,(map list vars expr) ,(c16expr body)))))

(define (c16quote args)
  (case (car args)
    ((()) c16null)
    (else (c16constant (car args)))))

(define (c16vref name)
  ;;(print "c.scm:debug, c16vref, name -> " name) ;; debug
  (if (and (c.scm:var? name)
           (not (var-cscm name))
           (var-toplevel name)
           (var-cscm c16*define*))
      `(cscm_vref ,name)
      name))
               
(define (c16set! args)
  (let ((var (car args))
        (exp (cadr args)))
    `(set! ,var ,(c16expr exp)))) ;; cscmではないはず
