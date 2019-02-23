;;; tutオリジナルのc1を実行する
;;; c0transform済みと想定

;; (define var (lambda params body))
;; (define var expr)
(define (c1analysis x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c1def-func first
                            name
                            form)
               (c1def-expr first
                            name
                            form))))
        (else
         (error "CSCM:ERROR, c1analysis, not a definition" x)))
      (error "CSCM:ERROR, c1analysis, not a definition" x)))

(define (c1def-func first name lambda-expr)
  (let ((tmp *env*)
        (ret #f))
    (set! *env* '())
    ;;
    (let ((x (c1lam (cdr lambda-expr))))
      (set! ret `(,first ,name (lambda ,@x))))
    ;;
    (set! *env* tmp)
    ret))

(define (c1def-expr first name expr)
  (let ((tmp *env*)
        (ret #f))
    (set! *env* '())
    ;;
    (let ((x (c1expr expr)))
      (set! ret `(,first ,name ,x)))
    ;;
    (set! *env* tmp)
    ret))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Intrinsic constants
(define (c1constant x)
  (if (cscm:self-eval? x)
      x
      `(quote ,x)))

(define c1null 
  ''())
(define c1false
  #f)
(define c1true 
  #t)
(define c1if-else-default 
  ''())
(define c1cond-else-default 
  ''())
(define c1begin-empty-default 
  ''())

;;; SCCOND  Conditionals.

(define (c1if args)
  (list 'if
        (c1fmla (car args))
        (c1expr (cadr args))
        (c1expr (caddr args))))

(define (c1fmla fmla)
  (if (pair? fmla)
      (case (car fmla)
        ((and) (cons 'and (c1map c1fmla (cdr fmla))))
        ((or) (cons 'or (c1map c1fmla (cdr fmla))))
        ((not) (cond ((c1lookup 'not) (c1expr fmla))
                     (else (list 'not (c1fmla (cadr fmla))))))
        (else (c1expr fmla)))
      (c1expr fmla)))

(define (c1and args)
  `(and ,@(c1args args)))

(define (c1or args)
  `(or ,@(c1args args)))


;;; SCEVAL  The Expression Dispatcher.

(define (c1expr form)
  ;;(print "cscm:debug, c1expr, form -> " form) ;; debug
  (cond ((symbol? form)
         (c1vref form))
        ((pair? form)
         (let ((fun (car form)) (args (cdr form)))
           (cond ((symbol? fun)
                  (case fun
                    ((if) (c1if args)) 
                    ((and) (c1and args))
                    ((or) (c1or args))
                    ((begin) (c1begin args))
                    ((lambda) (c1lambda args))
                    ((delay) (c1delay args))
                    ((let) (c1let args))
                    ((let*) (c1let* args))
                    ((letrec) (c1letrec args))
                    ((set!) (c1set! args))
                    ((quote) (c1quote args))
                    (else
                     (c1symbol-fun fun args))))
                 (else
                  `(,(c1expr fun) ,@(c1args args))))))
        (else
          (case form
            ((#f) c1false)
            ((#t) c1true)
            ((()) c1null)
            (else (c1constant form))))))

(define (c1symbol-fun name args)
  (let parse ((env *env*) (ccb #f))
    (cond ((null? env)
           `(,name ,@(c1args args)))
          ((eq? (car env) 'CB)
           (parse (cdr env) #t))
          ((eq? (var-name (car env)) name)
           (if ccb
               (set-var-closed (car env) #t))
           `(,(car env) ,@(c1args args)))
          (else
           (parse (cdr env) ccb)))))

(define (c1args forms)
  (if (end? forms)
      '()
      (cons (c1expr (car forms)) (c1args (cdr forms)))))

(define (c1begin forms)
  `(begin ,@(c1map c1expr forms)))

(define (c1body body)
  (c1expr (car body)))

;;; SCFUNC  Lambda Expression.

(define (c1lambda args)
  (let ((tmp *env*)
        (ret #f))
    (set! *env* (cons 'CB *env*))
    ;;
    (set! ret (cons 'lambda (c1lam args)))
    ;;
    (set! *env* tmp)
    ret))

(define (c1delay args)
  (let ((tmp *env*)
        (ret #f))
    (set! *env* (cons 'CB *env*))
    ;;
    (set! ret (cons 'delay (c1lam (cons '() args))))
    ;;
    (set! *env* tmp)
    ret))

(define (c1lam lambda-expr) ;; (params body)
  (let ((requireds '())
        (rest '()))
    (let ((tmp *env*)
          (ret #f))
      (set! *env* *env*)
      ;;
      (do ((vl (car lambda-expr) (cdr vl)))
          ((not (pair? vl))
           (if (not (null? vl)) ;; paramsがsymbol単体のとき
               (let ((var (make-var vl)))
                 (set! *env* (cons var *env*))
                 (set! rest var)))
           (let ((params (cond ((and (null? requireds)
                                     (null? rest))
                                '())
                               ((null? requireds)
                                rest)
                               ((null? rest)
                                (reverse requireds))
                               (else
                                (let ((req (reverse requireds)))
                                  (let loop ((res req))
                                    (cond ((null? (cdr res))
                                           (set-cdr! res rest)
                                           req)
                                          (else
                                           (loop (cdr res))))))))))
             (set! ret (list params (c1body (cdr lambda-expr))))))
        (let ((var (make-var (car vl))))
          (set! requireds (cons var requireds))
          (set! *env* (cons var *env*))))
      ;;
      (set! *env* tmp)
      ret)))


;;; SCLETS  Let, Let*, and Letrec.

(define (make-var name)
  (if (symbol? name)
      (if (member name '(if cond case and or begin lambda #;delay let let* 
                            letrec do set! quote quasiquote define macro))
          (error "CSCM:ERROR, cannot bind the keyword" name)
          (list name #f #f #f #f '() '() #f)) 
      (error "CSCM:ERROR, not a symbol" name)))

(define (var-name var) (car var))
(define (var-funarg var) (cadr var))
(define (var-assigned var) (caddr var))
(define (var-closed var) (cadddr var))
(define (var-local-fun var) (car (cddddr var)))
(define (var-local-fun-args var) (cadr (cddddr var)))
(define (var-loc var) (caddr (cddddr var)))
(define (var-liftable var) (cadddr (cddddr var))) ;; ラムダリフティング可能か

(define (set-var-name var x) (set-car! var x))
(define (set-var-funarg var x) (set-car! (cdr var) x))
(define (set-var-assigned var x) (set-car! (cddr var) x))
(define (set-var-closed var x) (set-car! (cdddr var) x))
(define (set-var-local-fun var x) (set-car! (cddddr var) x))
(define (set-var-local-fun-args var x) (set-car! (cdr (cddddr var)) x))
(define (set-var-loc var x) (set-car! (cddr (cddddr var)) x))
(define (set-var-liftable var x) (set-car! (cdddr (cddddr var)) x)) ;; ラムダリフティング可能か

(define *env* '())

;;
(define (c1lambda-params lambda-expr)
  (let loop ((params (cadr lambda-expr)))
    (cond ((null? params)
           '())
          ((cscm:var? params)
           (list params))
          (else
           (cons (car params)
                 (loop (cdr params)))))))
;;

(define (c1let args)
  (let ((body '())
        (defs '()))
    (let ((tmp *env*))
      ;;
      (let dolist ((lst (car args)))
        (if (null? lst)
            '()
            (let ((def (car lst)))
              (let ((var (make-var (car def))))
                (set! defs (cons (list var (cadr def)) defs))
                (set! *env* (cons var *env*)))
              (dolist (cdr lst)))))
      (set! body (c1body (cdr args)))
      ;;
      (set! *env* tmp))
      
    (let dolist ((lst defs))
      (if (null? lst)
          '()
          (let ((def (car lst)))
            (let ((var (car def))
                  (form (cadr def))
                  (free-vars (cscm:difference *env* '(CB))))
              (if (and (pair? form)
                       (eq? (car form) 'lambda)) ;; cscm
                  (if (or (var-funarg var)
                          (var-assigned var)
                          (var-closed var))
                      (set-car! (cdr def) (c1lambda (cdr form)))
                      (begin (set-car! (cdr def) (cons 'lambda (c1lam (cdr form))))
                             (let ((params (c1lambda-params (cadr def))))
                               (set-var-local-fun var (cscm:reduction (cscm:difference free-vars params))))
                             (set-var-local-fun-args var (cadr def))))
                  (set-car! (cdr def) (c1expr form))))
            (dolist (cdr lst)))))
    `(let ,(reverse defs) ,body)))

(define (c1let* args)
  (let ((body '())
        (defs '()))
    (let ((tmp *env*))
      ;;
      (let dolist ((lst (car args)))
        (if (null? lst)
            '()
            (let ((def (car lst)))
              (let ((var (make-var (car def))))
                (set! defs (cons (list var (cadr def)) defs))
                (set! *env* (cons var *env*)))
              (dolist (cdr lst)))))
      (set! body (c1body (cdr args)))

      (let dolist ((lst defs))
        (if (null? lst)
            '()
            (let ((def (car lst)))
              (set! *env* (cdr *env*))
              (let ((var (car def))
                    (form (cadr def))
                    (free-vars (cscm:difference *env* '(CB))))
                (if (and (pair? form)
                         (eq? (car form) 'lambda))
                    (if (or (var-funarg var)
                            (var-assigned var)
                            (var-closed var))
                        (set-car! (cdr def) (c1lambda (cdr form)))
                        (begin (set-car! (cdr def) (cons 'lambda (c1lam (cdr form))))
                               (let ((params (c1lambda-params (cadr def))))
                                 (set-var-local-fun var (cscm:reduction (cscm:difference free-vars params))))
                               (set-var-local-fun var (cadr def))))
                    (set-car! (cdr def) (c1expr form))))
              (dolist (cdr lst)))))
      ;;
      (set! *env* tmp))
    `(let* ,(reverse defs) ,body)))

(define (c1letrec args)
  (let ((body '())
        (defs '()))
    (let ((tmp *env*))
      ;;
      (let dolist ((lst (car args)))
        (if (null? lst)
            '()
            (let ((def (car lst)))
              (let ((var (make-var (car def))))
                (set! defs (cons (list var '() (cadr def)) defs))
                (set! *env* (cons var *env*)))
              (dolist (cdr lst)))))
      (set! body (c1body (cdr args)))

      (let dolist ((lst defs))
        (if (null? lst)
            '()
            (let ((def (car lst)))
              (let ((var (car def))
                    (form (caddr def))) ;; (var '() form)
                (let ((free-vars (cscm:difference *env* (list 'CB var))))
                  (if (and (pair? form)
                           (eq? (car form) 'lambda))
                      (if (or (var-funarg var)
                              (var-assigned var)
                              (var-closed var))
                          (set-car! (cdr def) (c1lambda (cdr form)))
                          (begin (set-car! (cdr def) (cons 'lambda (c1lam (cdr form))))
                                 (let ((params (c1lambda-params (cadr def))))
                                   (set-var-local-fun var (cscm:reduction (cscm:difference free-vars params))))
                                 (set-var-local-fun-args var (cadr def))))
                      (set-car! (cdr def) (c1expr form)))))
              (dolist (cdr lst)))))

      (c1letrec-aux defs)
      ;;
      (set! *env* tmp))
    (let loop ((defs defs))
      (if (null? defs)
          #t
          (let ((def (car defs)))
            (let ((var (car def))
                  (cform (cadr def))
                  (form (caddr def)))
              (set-car! defs (list var cform)))
            (loop (cdr defs)))))
    `(letrec ,(reverse defs) ,body)))

(define (c1letrec-aux defs)
  (define (parse defs)
    (if (null? defs)
        #f
        (let* ((def (car defs))
               (var (car def))
               (form (caddr def)))
          (if (and (var-local-fun var)
                   (or (var-funarg var)
                       (var-assigned var)
                       (var-closed var)))
              (begin (set-var-local-fun var #f)
                     (set-car! (cdr def) (c1lambda (cdr form)))
                     (parse (cdr defs))
                     #t)
              (parse (cdr defs))))))
  (if (parse defs) (c1letrec-aux defs)))
                                                         


;;; SCMISC  Miscellaneous Special Forms.

(define (c1quote args)
  (case (car args)
    ((#f) c1false)
    ((#t) c1true)
    ((()) c1null)
    (else (c1constant (car args)))))


;;; SCVREF  Variable References.

(define (c1vref name)
  (let lookup ((env *env*) (ccb #f))
    (if (null? env)
        name
        (let ((var (car env)))
          (cond ((eq? var 'CB) (lookup (cdr env) #t))
                ((eq? (var-name var) name)
                 (if ccb (set-var-closed var #t))
                 (set-var-funarg var #t)
                 var)
                (else (lookup (cdr env) ccb)))))))

(define (c1lookup name)
  (let lookup ((env *env*))
    (cond ((null? env) #f)
          ((and (not (eq? (car env) 'CB))
                (eq? (var-name (car env)) name))
           #t)
          (else (lookup (cdr env))))))

(define (c1set! args)
  (let ((name (car args))
        (form (cadr args)))
    (let lookup ((env *env*)
                 (ccb #f))
      (if (null? env)
          (list 'set! name (c1expr form))
          (let ((var (car env)))
            (cond ((eq? var 'CB)
                   (lookup (cdr env) #t))
                  ((eq? (var-name var) name)
                   (if ccb (set-var-closed var #t))
                   (set-var-assigned var #t)
                   (list 'set! var (c1expr form)))
                  (else
                   (lookup (cdr env) ccb))))))))

(define (end? x)
  (cond ((pair? x) #f) 
        ((null? x) #t)
        (else (error "CSCM:ERROR, end?, not a pair" x))))

(define (c1map fun l)
  (if (end? l)
      '()
      (cons (fun (car l)) (c1map fun (cdr l)))))
