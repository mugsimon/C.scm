;;(define (var-name var) (car var)) ;; 変数名
(define (var-toplevel var) (cadr var)) ;; トップレベル->#t
;;(define (var-assigned var) (caddr var)) ;; 代入される->#t
(define (var-cscm var) (cadddr var)) ;; Cになる->#t
(define (var-cons var) (car (cddddr var))) ;; コンストラクタを持つ->#t
(define (var-local-fun-args var) (cadr (cddddr var)))
(define (var-loc var) (caddr (cddddr var)))

;;(define (set-var-name var x) (set-car! var x))
(define (set-var-toplevel var x) (set-car! (cdr var) x))
;;(define (set-var-assigned var x) (set-car! (cddr var) x))
(define (set-var-cscm var x) (set-car! (cdddr var) x))
(define (set-var-cons var x) (set-car! (cddddr var) x))
(define (set-var-local-fun-args var x) (set-car! (cdr (cddddr var)) x))
(define (set-var-loc var x) (set-car! (cddr (cddddr var)) x))

(define c15*define* #f)

(define (c.scm:c15 x)
  (if (c.scm:pair? x)
      (case (car x)
        ((define) ;; (define name (lambda params body)) || (define name expr)
         (let ((form (caddr x))) ;; (lambda params body) || expr
           (if (and (c.scm:pair? form)
                    (eq? (car form) 'lambda))
               (c.scm:c15function (car x) ;; define
                                  (cadr x) ;; name
                                  (caddr x)) ;; (lambda params body)
               (dlet ((c15*define* (make-var (cadr x))))
                     (let ((expr (c.scm:c15expr (caddr x))))
                       `(,(car x) ,c15*define* ,expr))))))
        (else
         (c15expr x)))
      (c15expr x)))

(define (c.scm:c15function first name lambda-expr)
  (dlet ((c15*define* (make-var name)))
        (let ((x (delt ((*env* '()))
                       (c1lam (cdr lambda-expr)))))
          `(,first ,c15*define* (lambda ,@x)))))

(define (c.scm:c15expr expr)
  (let ((x (dlet ((*env* '()))
                 (c15expr expr))))
    x))        
;;;;;;;;;;;;;;;;;;;;;;
(define (c15constant x)
  (if (c.scm:self-eval? x)
      x
      `(quote ,x)))
(define c15null
  `'())
(define c15false
  #f)
(define c15true
  #t)

;; if
(define (c15if args)
  (let ((t (c15fmla (car args)))
        (m1 (c15epxr (cadr args)))
        (m2 (c15expr (caddr args))))
    `(if ,t ,m1 ,m2)))

;; if fmla
(define (c15fmla fmla)
  (if (pair? fmla)
      (case (car fmla)
        ((and)
         (cons 'and (map c15fmla (cdr fmla))))
        ((or)
         (cons 'or (map c15fmla (cdr fmla))))
        ((not)
         (list 'not (c15fmla (cadr fmla))))
        (else
         (c15expr fmla)))
      (c15expr fmla)))

;; and
(define (c15and args)
  `(and ,@(c15args args)))
              
;; or
(define (c15or args)
  `(or ,@(c15args args)))

(define (c15expr form)
  (cond ((c.scm:symbol? form)
         (c15vref form))
        ((c.scm:pair? form)
         (let ((fun (car form) (args (cdr form))))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c15if args))
                    ((and) (c15and args))
                    ((or) (c15or args))
                    ((begin) (c15begin args))
                    ((lambda) (c15lambda args))
                    ((delay) (c15delay args))
                    ((let) (c15let args))
                    ((let*) (c15let* args))
                    ((letrec) (c15letrec args))
                    ((set!) (c15set! args))
                    ((quote) (c15quote args))
                    (else
                     (c15symbol-fun fun args))))
                 (else
                  `(,(c15expr fun) ,@(c15args))))))
        (else
         (case form
           ((#f) c15false)
           ((#t) c15true)
           ((()) c15null)
           (else (c15constant form))))))

(define (c15symbol-fun name args))



