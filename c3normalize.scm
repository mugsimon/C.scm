;;; tut c1後の内部形式をSchemeの標準の構文に戻す
;;; ただしmake-varはそのまま

;; (define var (lambda requireds rest body))
;; (define var expr)
(define (c3normalize x)
  ;; (print "cscm:debug, c3normalize, x -> " x) ;; debug
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
         (error "CSCM:ERROR, c3normalize, not a definition" x)))
      (error "CSCM:ERROR, c3normalize, not a definition" x)))

(define (c3def-func first name lambda-expr)
  (let ((x (c3lam (cdr lambda-expr))))
    (list first name (cons 'lambda x))))

(define (c3def-expr first name expr)
  (list first name (c3expr expr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c3expr form)
  (cond ((cscm:symbol? form)
         (c3vref form))
        ((cscm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((cscm:symbol? fun)
                  (case fun
                    ((if) (c3if args))
                    ((and) (c3and args))
                    ((or) (c3or args))
                    ((begin) (c3begin args))
                    ((lambda) (c3lambda args))
                    ((delay) (c3delay args)) ;; (delay args)
                    ((let) (c3let args)) 
                    ((let*) (c3let* args))
                    ((letrec) (c3letrec args))
                    ((set!) (c3set! args)) ;;
                    ((quote) (c3quote args))
                    (else
                     (c3symbol-fun fun args))))
                 (else
                  (if (null? args)
                      `(,(c3expr fun))
                      `(,(c3expr fun) ,@(c3args (car args))))))))
        (else
         form)))

(define (c3vref name)
  name)

(define (c3if args)
  `(if ,(c3fmla (car args))
       ,(c3expr (cadr args))
       ,(c3expr (caddr args))))

(define (c3fmla fmla)
  (if (cscm:pair? fmla)
      (case (car fmla)
        ((and)
         (cons 'and (map c3fmla (cdr fmla))))
        ((or)
         (cons 'or (map c3fmla (cdr fmla))))
        ((not)
         (list 'not (c3fmla (cadr fmla))))
        (else
         (c3expr fmla)))
      (c3expr fmla)))

(define (c3and args)
  `(and ,@(c3args (car args))))

(define (c3or args)
  `(or ,@(c3args (car args))))

(define (c3begin args)
  `(begin ,@(map c3expr (car args))))

(define (c3lambda args)
  (let ((requireds (cadr args))
        (rest (caddr args))
        (body (cadddr args)))
    (let ((params (cond ((and (null? requireds)
                              (null? rest))
                         '())
                        ((null? requireds)
                         rest)
                        ((null? rest)
                         requireds)
                        (else
                         (let ((req requireds))
                           (let loop ((res req))
                             (cond ((null? (cdr res))
                                    (set-cdr! res rest)
                                    req)
                                   (else
                                    (loop (cdr res))))))))))
      `(lambda ,params ,(c3expr body)))))

(define (c3lam args)
  (let ((requireds (car args))
        (rest (cadr args))
        (body (caddr args)))
    (let ((params (cond ((and (null? requireds)
                              (null? rest))
                         '())
                        ((null? requireds)
                         rest)
                        ((null? rest)
                         requireds)
                        (else
                         (let ((req requireds))
                           (let loop ((res req))
                             (cond ((null? (cdr res))
                                    (set-cdr! res rest)
                                    req)
                                   (else
                                    (loop (cdr res))))))))))
      (list params (c3expr body)))))

(define (c3let form)
  (let loop ((defs (car form))
             (ndefs '()))
    (cond ((null? defs)
           `(let ,(reverse ndefs) ,(c3expr (cadr form))))
          (else
           (loop (cdr defs)
                 (cons (c3let-def (car defs))
                       ndefs))))))

(define (c3let-def def)
  (let ((var (car def))
        (expr (cdr def)))
    (if (var-local-fun var)
        (list var `(lambda ,@(c3lam expr)))
        (list var (c3expr expr)))))

(define (c3let* form)
  (let loop ((defs (car form))
             (ndefs '()))
    (cond ((null? defs)
           `(let* ,(reverse ndefs) ,(c3expr (cadr form))))
          (else
               (loop (cdr defs)
                     (cons (c3let-def (car defs))
                           ndefs))))))

(define (c3letrec form)
  (let loop ((defs (car form))
             (ndefs '()))
    (cond ((null? defs)
           `(letrec ,(reverse ndefs) ,(c3expr (cadr form))))
          (else
           (loop (cdr defs)
                 (cons (c3letrec-def (car defs))
                       ndefs))))))

(define (c3letrec-def def)
  (let ((var (car def))
        (expr (cadr def))) ;; c3let-defとはここが異なる
    (if (var-local-fun var)
        (list var `(lambda ,@(c3lam expr)))
        (list var (c3expr expr)))))

(define (c3set! args)
  (let ((var (car args)))
    (if (cscm:var? var)
        (let ((exp (caddr args)))
          `(set! ,var ,(c3expr exp)))
        (let ((exp (cadr args)))
          `(set! ,var ,(c3expr exp))))))

(define (c3quote args)
  `(quote ,@args))

(define (c3symbol-fun name args)
  `(,name ,@(c3args (car args))))

(define (c3args forms)
  (if (null? forms)
      '()
      (cons (c3expr (car forms))
            (c3args (cdr forms)))))
