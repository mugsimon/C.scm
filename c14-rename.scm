;;; c.scm:c14renameはホイストが終了した各関数を受け取り、Cにできない名前を含む文字を変更する
;; (define var (lambda params body))
;; (define var expr)
;; (fun ...)
(define (c.scm:c14rename x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c14function (car x) ;; define
                                     (cadr x) ;; name
                                     (caddr x)) ;; (lambda params body)
                  `(,(car x) ,(c14vref (cadr x)) ,(c14expr form)))))
           (else
            (c14expr x)))
         (c14expr x)))

(define (c.scm:c14function first name lambda-expr)
  `(,first ,(c14vref name) ,(c14expr lambda-expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c14expr form)
  #;(print "c.scm:debug, c14expr, form -> " form) ;; debug
  (cond ((symbol? form)
         (c14vref form))
        ((pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((symbol? fun)
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
  (cond ((list? params)
         (map c14vref params))
        ((pair? params)
         (let loop ((params params)
                    (_params '()))
           (if (symbol? params)
               (apply list* (reverse (cons (c14vref params) _params)))
               (loop (cdr params)
                     (cons (c14vref (car params)) _params)))))
        ((symbol? params)
         (c14vref params))
        (else
         '())))

(define (c14let args)
  (if (symbol? (car args))
      (c14named-let args)
      (let ((defs (car args))
            (body (c14expr (cadr args))))
        (let ((vars (map c14expr (map car defs)))
              (exps (map c14expr (map cadr defs))))
          `(let ,(map list vars exps) ,body)))))

(define (c14named-let args)
  (let ((name (c14vref (car args)))
        (defs (cadr args))
        (body (c14expr (caddr args))))
    (let ((vars (map c14expr (map car defs)))
          (exps (map c14expr (map cadr defs))))
      `(let ,name ,(map list vars exps) ,body))))

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
  `(quote ,args))

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
  (memq symbol (list '+ '- '* '/ '= '> '<
                     )))

(define c14*chars* (list (cons #\? #\P)
                         (cons #\! #\B)
                         (cons #\- #\_)
                         (cons #\* #\S)
                         ))

(define (c14rename symbol)
  (let ((s-symbol (symbol->string symbol)))
    (let ((cl-symbol (string->list s-symbol)))
      (let loop ((ch-lst cl-symbol))
        (cond ((null? ch-lst)
               (string->symbol (list->string cl-symbol)))
              (else
               (let ((ch (assq (car ch-lst) c14*chars*)))
                 (if ch
                     (begin (set-car! ch-lst (cdr ch))
                            (loop (cdr ch-lst)))
                     (loop (cdr ch-lst))))))))))
               
