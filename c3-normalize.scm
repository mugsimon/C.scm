;;; tut c1後の内部形式をSchemeの標準の構文に戻す
;;; ただしmake-varはそのまま

;; (define var (lambda requireds rest body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
(define (c.scm:c3normalize x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c3normalize-function (car x) ;; define
                                               (cadr x) ;; name
                                               (caddr x)) ;; (lambda required rest body)
                  `(,(car x) ,(cadr x) ,(c.scm:c3normalize-expr form)))))
           ((begin)
            `(begin ,@(map c.scm:c3normalize (cdr x))))
           (else
            (c.scm:c3normalize-expr x)))
         (c.scm:c3normalize-expr x)))

(define (c.scm:c3normalize-function first name lambda-expr)
  (let ((x (c3lam (cdr lambda-expr))))
    (list first name (cons 'lambda x))))

;; form->expr
(define (c.scm:c3normalize-expr form)
  (c3expr form))

(define (c.scm:symbol? x)
  (or (c.scm:var? x)
      (symbol? x)))

(define (c.scm:pair? x)
  (and (not (c.scm:var? x))
       (pair? x)))

(define (c.scm:var? x)
  (and (list? x)
       (= (length x) 7)
       (symbol? (var-name x))
       (boolean? (var-funarg x))
       (boolean? (var-assigned x))
       (boolean? (var-closed x))
       (or (boolean? (var-local-fun x))
           (list? (var-local-fun x)))
       (list? (var-local-fun-args x))
       (list? (var-loc x))))  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c3expr form)
  (cond ((c.scm:symbol? form)
         (c3vref form))
        ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
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
                  `(,(c3expr fun) ,@(c3args (car args)))))))
        (else
         form)))

(define (c3vref name)
  name)

(define (c3if args)
  `(if ,(c3expr (car args))
       ,(c3expr (cadr args))
       ,(c3expr (caddr args))))

(define (c3and args)
  `(and ,@(c3args (car args))))

(define (c3or args)
  `(or ,@(c3args (car args))))

(define (c3begin args)
  `(begin ,@(c3map c3expr args)))

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
  (if (c.scm:var? (car form))
      (c3named-let form)
      (let loop ((defs (car form))
                 (ndefs '()))
        (cond ((null? defs)
               `(let ,(reverse ndefs) ,(c3expr (cadr form))))
              (else
               (loop (cdr defs)
                     (cons (c3let-def (car defs))
                           ndefs)))))))

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

(define (c3named-let form)
  (let ((var (car form))
        (inits (map c3expr (cadr form))) ;; lambda式が束縛されているとしてもc1ではc1exprに通しているのでc1lamの結果となっていることはない
        (fun (caddr form))) ;; エスケープされるかどうかでc1lambda, c1lamが違う
    (let ((params (if (var-local-fun var)
                      (car fun)
                      (caddr fun))))
      `(let ,var ,(map list params inits) ,(if (var-local-fun var)
                                               (cadr (c3lam fun)) ;; 本体部分だけほしい
                                               (caddr (c3lambda (cdr fun)))))))) ;; 本体部分だけほしい

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
        (expr (cadr def)))
    (if (var-local-fun var)
        (list var `(lambda ,@(c3lam expr)))
        (list var (c3expr expr)))))

(define (c3set! args)
  `(set! ,(c3expr (car args)) ,(c3expr (cadr args))))

(define (c3quote args)
  `(quote ,@args))

(define (c3symbol-fun name args)
  `(,name ,@(c3args (car args))))

(define (c3args forms)
  (if (null? forms)
      '()
      (cons (c3expr (car forms))
            (c3args (cdr forms)))))
