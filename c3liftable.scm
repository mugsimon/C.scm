(define (c3liftable x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c3def-func first
                            name
                            form)
               (c3def-expr first
                            name
                            form))))
        (else
         (error "CSCM:ERROR, c3liftable, not a definition" x)))
      (error "CSCM:ERROR, c3liftable, not a definition" x)))

(define (c3def-func first name lambda-expr)
  (let ((tmp *env*)
        (ret #f))
    (set! *env* '())
    ;;
    (let ((x (c3lam (cdr lambda-expr))))
      (set! ret `(,first ,name (lambda ,@x))))
    ;;
    (set! *env* tmp)
    ret))

(define (c3def-expr first name expr)
  (let ((tmp *env*)
        (ret #f))
    (set! *env* '())
    ;;
    (let ((x (c3expr expr)))
      (set! ret `(,first ,name ,x)))
    ;;
    (set! *env* tmp)
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intrinsic constants
(define (c3constant x)
  (if (cscm:self-eval? x)
      x
      `(quote ,x)))

(define c3null ''())
(define c3false #f)
(define c3true #t)
(define c3if-else-default ''())
(define c3cond-else-default ''())
(define c3begin-empty-default ''())


;;; SCCOND  Conditionals.

(define (c3if args)
  (list 'if
        (c3fmla (car args))
        (c3expr (cadr args))
        (c3expr (caddr args))))

(define (c3fmla fmla)
  (if (cscm:pair? fmla)
      (case (car fmla)
        ((and) (cons 'and (map c3fmla (cdr fmla))))
        ((or) (cons 'or (map c3fmla (cdr fmla))))
        ((not) (list 'not (c3fmla (cadr fmla))))
        (else (c3expr fmla)))
      (c3expr fmla)))

(define (c3and args)
  `(and ,@(c3args args)))

(define (c3or args)
  `(or ,@(c3args args)))


;;; SCEVAL  The Expression Dispatcher.

(define (c3expr form)
  ;;(print "cscm:debug, c3expr, form->" form) ;; debug
  (cond ((cscm:symbol? form)
         (c3vref form))
        ((cscm:pair? form)
         (let ((fun (car form)) (args (cdr form)))
           (cond ((cscm:symbol? fun)
                  (case fun
                    ((if) (c3if args)) 
                    ((and) (c3and args))
                    ((or) (c3or args))
                    ((begin) (c3begin args))
                    ((lambda) (c3lambda args))
                    ((delay) (c3delay args))
                    ((let) (c3let args))
                    ((let*) (c3let* args))
                    ((letrec) (c3letrec args))
                    ((set!) (c3set! args))
                    ((quote) (c3quote args))
                    (else
                     (c3symbol-fun fun args))))
                 (else
                  `(,(c3expr fun) ,@(c3args args))))))
        (else
          (case form
            ((#f) c3false)
            ((#t) c3true)
            ((()) c3null)
            (else (c3constant form))))))

(define (c3symbol-fun name args)
  `(,name ,@(c3args args)))

(define (c3args forms)
  (if (null? forms)
      '()
      (cons (c3expr (car forms)) (c3args (cdr forms)))))

(define (c3begin forms)
  `(begin ,@(map c3expr forms)))

(define (c3body body)
  (c3expr body))


;;; SCFUNC  Lambda Expression.

(define (c3lambda args)
  (let ((tmp *env*)
        (ret #f))
    (set! *env* (cons 'CB *env*))
    ;;
    (set! ret (cons 'lambda (c3lam args)))
    ;;
    (set! *env* tmp)
    ret))

(define (c3delay args)
  (let ((tmp *env*)
        (ret #f))
    (set! *env* (cons 'CB *env*))
    ;;
    (set! ret (cons 'delay (c3lam (cons '() args))))
    ;;
    (set! *env* tmp)
    ret))

(define (c3lam lambda-expr) ;; (params body)
  (let ((tmp *env*)
        (ret #f))
    (set! *env* *env*)
    ;;
    (do ((vl (car lambda-expr) (cdr vl)))
        ((not (cscm:pair? vl))
         (if (not (null? vl)) ;; paramsがsymbol単体のとき
             (let ((var vl))
               (set! *env* (cons var *env*)))
             (let ((params (car lambda-expr)))
               (set! ret (list params (c3body (cadr lambda-expr)))))))
      (let ((var (car vl)))
        (set! *env* (cons var *env*))))
    ;;
    (set! *env* tmp)
    ret))


;;; SCLETS  Let, Let*, and Letrec.

(define *liftable* #f)

;;(define (c3set-liftable def)
;;  (let ((var (car def))
;;        (exp (cadr def)))
    ;;(print "cscm:debug, c3set-liftable, var->" var) ;; debug
;;    (if (var-local-fun var)
;;        (let ((tmp *liftable*))
;;          (set! *liftable* #t)
          ;;
;;          (c3expr exp)
;;          (set-var-liftable var *liftable*)
          ;;
;;          (set! *liftable* tmp))
;;        (c3expr exp))))

(define (c3set-liftable def)
  (let ((var (car def))
        (exp (cadr def)))
    ;;(print "cscm:debug, c3set-liftable, var->" var) ;; debug
    (if (and (pair? exp)
             (eq? (car exp) 'lambda))
        (if (var-local-fun var)
            (let ((tmp *liftable*))
              (set! *liftable* #t)
              ;;
              (c3expr exp) ;; 自由変数への代入, ノンリフタブルなローカル関数とクロージャーを持たないか検査
              (if *liftable* 
                  (begin (set-var-liftable var *liftable*)
                         ;;
                         (set! *liftable* tmp)) ;; リフタブル
                  (set! *liftable* #f)))
            (c3expr exp))
        (c3expr exp)))) 

(define (c3let args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((tmp *env*))
      ;;
      (let dolist ((lst defs))
        (if (null? lst)
            '()
            (let ((def (car lst)))
              (let ((var (car def)))
                (set! *env* (cons var *env*)))
              (dolist (cdr lst)))))
      (c3body body)
      ;;
      (set! *env* tmp))
    
    (let dolist ((lst defs))
      (if (null? lst)
          '()
          (let ((def (car lst)))
            (c3set-liftable def)
            (dolist (cdr lst)))))
    `(let ,defs ,body)))

(define (c3let* args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((tmp *env*))
      ;;
      (let dolist ((lst defs))
        (if (null? lst)
            '()
            (let ((def (car lst)))
              (let ((var (car def)))
                (set! *env* (cons var *env*)))
              (dolist (cdr lst)))))
      (c3body body)

      (let dolist ((lst defs))
        (if (null? lst)
            '()
            (let ((def (car lst)))
              (set! *env* (cdr *env*))
              (c3set-liftable def)
              (dolist (cdr lst)))))
      ;;
      (set! *env* tmp))
    `(let* ,defs ,body)))

(define (c3letrec args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((tmp *env*))
      ;;
      (let dolist ((lst defs))
        (if (null? lst)
            '()
            (let ((def (car lst)))
              (let ((var (car def)))
                (set! *env* (cons var *env*)))
              (dolist (cdr lst)))))
      (c3body body)

      (let dolist ((lst defs))
        (if (null? lst)
            '()
            (let ((def (car lst)))
              (c3set-liftable def)
              (dolist (cdr lst)))))
      ;;
      (set! *env* tmp))
    `(letrec ,defs ,body)))


;;; SCMISC  Miscellaneous Special Forms.

(define (c3quote args)
  (case (car args)
    ((#f) c3false)
    ((#t) c3true)
    ((()) c3null)
    (else (c3constant (car args)))))


;;; SCVREF  Variable References.

;(define (c3vref name)
;  name)

(define (c3vref name)
  (if (cscm:var? name)
      (if (var-assigned name)
          (let lookup ((env *env*)
                       (ccb #f))
            (if (null? env)
                (error "CSCM:ERROR, c3vref, ローカル変数が見つかりません")
                (let ((var (car env)))
                  (cond ((eq? var 'CB)
                         (lookup (cdr env) #t))
                        ((eq? (var-name var) (var-name name))
                         (if ccb (set! *liftable* #f))
                         name)
                        (else
                         (lookup (cdr env) ccb)))))))
      name))
                        

(define (c3set! args)
  (let ((name (car args))
        (form (cadr args)))
    (if (cscm:var? name)
        (let lookup ((env *env*)
                     (ccb #f))
          (if (null? env)
              (error "CSCM:ERROR, c3set!, 致命的なエラーです. ローカル変数が見つかりません")
              (let ((var (car env)))
                (cond ((eq? var 'CB)
                       (lookup (cdr env) #t))
                      ((eq? (var-name var) (var-name name))
                       (if ccb (set! *liftable* #f)) ;; 自由変数へのset!があるならリフタブルではない
                       (list 'set! var (c3expr form)))
                      (else
                       (lookup (cdr env) ccb))))))
        (list 'set! name (c3expr form)))))
