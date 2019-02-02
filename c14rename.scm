;;; c.scm:c14renameはホイストが終了した各関数を受け取り、Cにできない名前を含む文字を変更する
;; (define var (lambda params body))
;; (define var expr)
;; 20190119 make-varに対応させる
(define (c14rename-def x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (cscm:pair? form)
                    (eq? (car form) 'lambda))
               (c14def-func first
                            name
                            form)
               (c14def-expr first
                            name
                            form))))
        (else
         (error "CSCM:ERROR, c14rename, not a definition")))
      (error "CSCM:ERROR, c14rename, not a definition")))

(define (c14def-func first name lambda-expr)
  `(,first ,(c14vref name) ,(c14expr lambda-expr)))

(define (c14def-expr first name expr)
  `(,first ,(c14vref name) ,(c14expr expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c14expr form)
  (cond ((cscm:symbol? form)
         (c14vref form))
        ((cscm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((cscm:symbol? fun)
                  (case fun
                    ((if) (c14if args))
                    ((and) (c14and args))
                    ((or) (c14or args))
                    ((begin) (c14begin args))
                    ((lambda) (c14lambda args))
                    ((delay) (c14delay args))
                    ((let) (c14let args))
                    ((let*) (c14let* args))
                    ((letrec) (c14letrec args))
                    ((set!) (c14set! args))
                    ((quote) (c14quote args))
                    (else
                     (c14symbol-fun fun args))))
                 (else
                  `(,(c14expr fun) ,@(c14args args))))))
        (else
         (case form
           ((#t) #t)
           ((#f) #f)
           ((()) '())
           (else
            form)))))

(define (c14if args)
  `(if ,(c14expr (car args))
       ,(c14expr (cadr args))
       ,(c14expr (caddr args))))

(define (c14and args)
  `(and ,@(map c14expr args)))

(define (c14or args)
  `(or ,@(map c14expr args)))

(define (c14begin args)
  `(begin ,@(map c14expr args)))
  
(define (c14lambda args)
  (let ((params (c14params (car args)))
        (body (cadr args)))
    `(lambda ,params ,(c14expr body))))

(define (c14params params)
  (cond ((cscm:list? params)
         (map c14vref params))
        ((cscm:pair? params)
         (let loop ((params params)
                    (_params '()))
           (if (cscm:symbol? params)
               (apply list* (reverse (cons (c14vref params) _params)))
               (loop (cdr params)
                     (cons (c14vref (car params)) _params)))))
        ((cscm:symbol? params)
         (c14vref params))
        (else
         '())))

(define (c14let args)
  (let ((defs (car args))
        (body (c14expr (cadr args))))
    (let ((vars (map c14expr (map car defs)))
          (exps (map c14expr (map cadr defs))))
      `(let ,(map list vars exps) ,body))))

(define (c14let* args)
  (let ((defs (car args))
        (body (c14expr (cadr args))))
    (let ((vars (map c14expr (map car defs)))
          (exps (map c14expr (map cadr defs))))
      `(let* ,(map list vars exps) ,body))))

(define (c14letrec args)
  (let ((defs (car args))
        (body (c14expr (cadr args))))
    (let ((vars (map c14expr (map car defs)))
          (exps (map c14expr (map cadr defs))))
      `(letrec ,(map list vars exps) ,body))))

(define (c14set! args)
  (let ((var (c14vref (car args)))
        (form (c14expr (cadr args))))
    `(set! ,var ,form)))

(define (c14quote args)
  `(quote ,@args))

(define (c14symbol-fun fun args)
  `(,(c14vref fun) ,@(c14args args)))

(define (c14args args)
  (if (null? args)
      '()
      (cons (c14expr (car args))
            (c14args (cdr args)))))

(define (c14vref form)
  (if (c14primitive? form)
      form
      (c14rename form)))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c14primitive? symbol)
  (or (memq symbol *primitive*)
      (memq symbol *library*)))

(define c14*chars* (list (cons #\! #\B)
                         (cons #\$ #\D)
                         (cons #\% #\R)
                         (cons #\& #\A)
                         (cons #\* #\S)
                         (cons #\+ #\T)
                         (cons #\- #\_)
                         (cons #\. #\X)
                         (cons #\/ #\H)
                         (cons #\: #\C)
                         (cons #\< #\L)
                         (cons #\= #\E)
                         (cons #\> #\G)
                         (cons #\? #\P)
                         (cons #\@ #\M)
                         (cons #\^ #\K)
                         (cons #\~ #\N)))

;;(define (c14rename symbol)
;;  (let ((s-symbol (string-copy (symbol->string symbol))))
;;    (let ((l (string-length s-symbol)))
;;      (let loop ((i 0))
;;        (if (= i l)
;;            (string->symbol s-symbol)
;;            (let ((c (string-ref s-symbol i)))
;;              (let ((ch (assq c c14*chars*)))
;;                (if ch
;;                    (begin (string-set! s-symbol i (cdr ch))
;;                           (loop (+ i 1)))
;;                    (loop (+ i 1))))))))))
  
(define (c14rename symbol)
  (let ((s-symbol (string-copy (symbol->string (cscm:var-name symbol)))))
    (let ((l (string-length s-symbol)))
      (let loop ((i 0))
        (if (= i l)
            (if (cscm:var? symbol)
                (begin (set-var-name symbol (string->symbol s-symbol))
                       symbol)                       
                (string->symbol s-symbol))
            (let ((c (string-ref s-symbol i)))
              (let ((ch (assq c c14*chars*)))
                (if ch
                    (begin (string-set! s-symbol i (cdr ch))
                           (loop (+ i 1)))
                    (loop (+ i 1))))))))))
