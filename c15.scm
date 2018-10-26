;;(define (var-name var) (car var)) ;; 変数名
(define (var-toplevel var) (cadr var)) ;; トップレベル->#t
;;(define (var-assigned var) (caddr var)) ;; 代入される->#t
(define (var-cscm var) (cadddr var)) ;; Cになる->#t
(define (var-local-fun var) (car (cddddr var))) ;; hoistされた関数->#t
(define (var-cons var) (cadr (cddddr var))) ;; コンストラクタを持つ->#t
(define (var-loc var) (caddr (cddddr var)))

;;(define (set-var-name var x) (set-car! var x))
(define (set-var-toplevel var x) (set-car! (cdr var) x))
;;(define (set-var-assigned var x) (set-car! (cddr var) x))
(define (set-var-cscm var x) (set-car! (cdddr var) x))
(define (set-var-local-fun var x) (set-car! (cddddr var) x))
(define (set-var-cons var x) (set-car! (cdr (cddddr var)) x)) 
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
        (let ((vars (c.scm:member c15*define* *toplevel*)))
          (if vars
              (set! c15*define* (car vars))
              (begin (set-var-toplevel c15*define* #t)
                     (set-var-cscm c15*define* #t)
                     (set! *toplevel* (cons c15*define* *toplevel*)))))
        (let ((x (dlet ((*env* '()))
                       (c15lam (cdr lambda-expr)))))
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
         (let ((fun (car form))
               (args (cdr form)))
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

;; (f args)
(define (c15symbol-fun name args)
  (letrec ((lparse (lambda (env)
                     (cond ((null? env)
                            (gparse *toplevel*))
                           ((eq? (var-name (car env)) (var-name name))
                            (set-var-toplevel (car env) #f)
                            (if (var-assigned (car env))
                                (set-var-cscm c15*define* #f))
                            `(,(car env) ,@(c15args args)))
                           (else
                            (lparse (cdr env))))))
           (gparse (lambda (env)
                     (cond ((null? env)
                            (if (or (c.scm:primitive? name)
                                    (c.scm:library? name))
                                `(,name ,@(c15args args)) 
                                (let ((var (make-var name)))
                                  (set-var-toplevel var #t)
                                  (set-var-cscm var #t)
                                  (set! *toplevel* (cons var *toplevel*))
                                  `(,var ,@(c15args args)))))
                           ((eq? name (var-name (car env)))
                            `(,(car env) ,@(c15args args)))
                           (else
                            (gparse (cdr env)))))))
    (if (c.scm:var? name)
        (lparse *env*)
        (gparse *toplevel*))))

;;
(define (c15args forms)
  (if (null? forms)
      '()
      (cons (c15expr (car forms))
            (c15args (cdr forms)))))

;;
(define (c15begin forms)
  `(begin ,@(map c15expr forms)))

(define (c15body body)
  (c15expr body))

;;
(define (c15lambda args)
  (cons 'lambda (c1lam args)))

;;
(define (c15lam lambda-expr)
  (dlet ((*env* *env*))
        (do ((vl (car lambda-expr) (cdr vl)))
            ((not (c.scm:pair? vl))
             (if (not (null? vl))
                 (let ((var vl))
                   (set-var-toplevel var #f)
                   (set-var-cscm var #f)
                   (set-var-cons var #f)
                   (set! *env* (cons var *env*)))
                 (list (car lambda-expr) (c15body (cadr lambda-expr)))))
          (let ((var (car vl)))
            (set-var-toplevel var #f)
            (set-var-cscm var #f)
            (set-var-cons var #f)
            (set! *env* (cons var *env*))))))

(define (c15let args)
  (let ((body (cadr args))
        (defs (car args)))
    (dlet ((*env* *env*))
          (dolist (def defs)
                  (let ((var (car def)))
                    (set-var-toplevel var #f)
                    (set-var-cscm var #f)
                    (set-var-cons var #f)
                    (set! *env* (cons var *env*))))
          (set! body (c15body (cadr args)))
          (dolist (def defs)
                  (let ((var (car def))
                        (form (c15expr (cadr def))))
                    (set-car! (cdr def) form))))
    `(let ,defs ,body)))

(define (c15let* args)
  (let ((defs (car args))
        (body (cadr args)))
    (dlet ((*env* *env*))
          (dolist (def defs)
                  (let ((var (car def)))
                    (set-var-toplevel var #f)
                    (set-var-cscm var #f)
                    (set-var-cons var #f)
                    (set! *env* (cons var *env*))))
          (set! body (c15body body))
          (dolist (def defs)
                  (set! *env* (cdr *env*))
                  (let ((var (car def))
                        (form (cadr def)))
                    (set-car! (cdr def) (c15expr form)))))
    `(let* ,defs ,body)))

(define (c15letrec args)
  (let ((defs (car args))
        (body (cadr args)))
    (dlet ((*env* *env*))
          (dolist (def (car args))
                  (let ((var (car def)))
                    (set-var-toplevel var #f)
                    (set-var-cscm var #f)
                    (set-var-cons var #f)
                    (set! *env* (cons var *env*))))
          (set! body (c15body body))
          (dolist (def defs)
                  (let ((form (cadr def)))
                    (set-car! (cdr def) (c15expr form)))))
    `(letrec ,defs ,body)))

(define (c15quote args)
  (case (car args)
    ((()) c15null)
    (else (c15constant (car args)))))

(define (c15vref name)
  (letrec ((lparse (lambda (env)
                     (if (null? env)
                         (begin (set-var-toplevel name #f) ;; ここに達することはない気がするが
                                (set-var-cscm name #f)
                                (set-var-cons name #f)
                                (print "c.scm:debug, c15vrefでlparseのenvがnullになりました") ;; debug
                                name)
                         (let ((var (car env)))
                           (if (eq? (var-name var) (var-name name))
                               var
                               (lparse (cdr env)))))))
           (gparse (lambda (env)
                     (if (null? env)
                         (let ((var (make-var name)))
                           (set-var-toplevel name #t)
                           (set-var-cscm name #t)
                           (set-var-cons name #f)
                           (set! *toplevel* (cons var *toplevel*))
                           var)
                         (let ((var (car env)))
                           (if (eq? name (var-name var))
                               var
                               (gparse (cdr env))))))))
    (if (c.scm:var? name)
        (lparse *env*)
        (gparse *toplevel*))))
               
(define (c15set! args)
  (let ((name (car args))
        (form (cadr args)))
    (set-var-cscm c15*define* #f)
    (letrec ((lparse (lambda (env)
                       (if (null? env)
                           (begin (set-var-toplevel name #f)
                                  (set-var-cscm name #f)
                                  (set-var-cons name #f)
                                  (print "c.scm:debug, c15set!でlparseのenvがnullになりました") ;; debug
                                  `(set! ,name ,(c15expr form)))
                           (let ((var (car env)))
                             (if (eq? (var-name var) (var-name name))
                                 `(set! ,var ,(c15expr form))
                                 (lparse (cdr env)))))))
             (gparse (lambda (env)
                       (if (null? env)
                           (let ((var (make-var name)))
                             (set-var-toplevel name #t)
                             (set-var-cscm name #f)
                             (set-var-cons name #f)
                             (set-var-assigned #t)
                             (set! *toplevel* (cons var *toplevel*))
                             var)
                           (let ((var (car env)))
                             (if (eq? name (var-name var))
                                 var
                                 (gparse (cdr env))))))))
      (if (c.scm:var? name)
          (lparse *env*)
          (gparse *toplevel*)))))
