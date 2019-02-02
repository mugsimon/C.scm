;;; Schemeの定義を受け取る
;;; make-varされた変数を通常の変数に戻す

;; (define var (lambda params body))
;; (define var expr)
(define (c7scheme x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c7def-func first
                           name
                           form)
               (c7def-expr first
                           name
                           form))))
        (else
         (error "CSCM:ERROR, c7scheme, not a definition" x)))
      (error "CSCM:ERROR, c7scheme, not a definition" x)))

(define (c7scheme x)
  (if (cscm:pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (cscm:pair? form)
                    (eq? (car form) 'lambda))
               (c7def-func first
                           name
                           form)
               (c7def-expr first
                           name
                           form))))
        (else
         (error "CSCM:ERROR, c7scheme, not a definition" x)))
      (error "CSCM:ERROR, c7scheme, not a definition" x)))

(define (c7def-func first name lambda-expr)
  (let ((name (if (cscm:var? name)
                  (var-name name)
                  name))
        (x (c7expr lambda-expr)))
    `(,first ,name ,x)))

(define (c7def-expr first name expr)
  (let ((name (if (cscm:var? name)
                  (var-name name)
                  name))
        (x (c7expr expr)))
    `(,first ,name ,x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c7expr form)
  (cond ((cscm:symbol? form)
         (c7vref form))
        ((cscm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((cscm:symbol? fun)
                  (case fun
                    ((if) (c7if args))
                    ((and) (c7and args))
                    ((or) (c7or args))
                    ((begin) (c7begin args))
                    ((lambda) (c7lambda args))
                    ((delay) (c7delay args))
                    ((let) (c7let args))
                    ((let*) (c7let* args))
                    ((letrec) (c7letrec args))
                    ((set!) (c7set! args))
                    ((quote) (c7quote args))
                    (else
                     (c7symbol-fun fun args))))
                 (else
                  `(,(c7expr fun) ,@(c7args args))))))
        (else
         (case form
           ((#f) #f)
           ((#t) #t)
           ((()) '())
           (else
            form)))))

(define (c7vref form)
  (if (cscm:var? form)
      (var-name form)
      form))

(define (c7if args)
  `(if ,(c7expr (car args))
       ,(c7expr (cadr args))
       ,(c7expr (caddr args))))

(define (c7and args)
  `(and ,@(map c7expr args)))

(define (c7or args)
  `(or ,@(map c7expr args)))

(define (c7begin args)
  `(begin ,@(map c7expr args)))

(define (c7lambda args)
  (let ((params (c7params (car args)))
        (body (cadr args)))
    (list 'lambda params (c7expr body))))

(define (c7params params)
  (cond ((null? params)
         '())
        ((cscm:var? params)
         (var-name params))
        (else
         (cons (var-name (car params)) (c7params (cdr params))))))

(define (c7params params)
  (cond ((null? params)
         '())
        ((cscm:var? params)
         (var-name params))
        (else
         (let ((var (car params)))
           (let ((name (if (cscm:var? var)
                           (var-name var)
                           var)))
         (cons name (c7params (cdr params))))))))

(define (c7let args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(let ,(reverse cdefs) ,(c7expr (cadr args))))
          (else
           (let ((def (car defs)))
             (loop (cdr defs)
                   (cons (list (var-name (car def))
                               (c7expr (cadr def)))
                         cdefs)))))))

(define (c7let args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(let ,(reverse cdefs) ,(c7expr (cadr args))))
          (else
           (let ((def (car defs)))
             (let ((var (car def))
                   (exp (cadr def)))
               (let ((name (if (cscm:var? var)
                               (var-name var)
                               var)))
                 (loop (cdr defs)
                       (cons (list name
                                   (c7expr exp))
                             cdefs)))))))))

(define (c7let* args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(let* ,(reverse cdefs) ,(c7expr (cadr args))))
          (else
           (let ((def (car defs)))
             (loop (cdr defs)
                   (cons (list (var-name (car def))
                               (c7expr (cadr def)))
                         cdefs)))))))

(define (c7let* args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(let* ,(reverse cdefs) ,(c7expr (cadr args))))
          (else
           (let ((def (car defs)))
             (let ((var (car def))
                   (exp (cadr def)))
               (let ((name (if (cscm:var? var)
                               (var-name var)
                               var)))
                 (loop (cdr defs)
                       (cons (list name
                                   (c7expr exp))
                             cdefs)))))))))

(define (c7letrec args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(letrec ,(reverse cdefs) ,(c7expr (cadr args))))
          (else
           (let ((var (caar defs))
                 (form (cadar defs)))
             (loop (cdr defs)
                   (cons (list (var-name var)
                               (c7expr form))
                         cdefs)))))))

(define (c7letrec args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(letrec ,(reverse cdefs) ,(c7expr (cadr args))))
          (else
           (let ((var (caar defs))
                 (form (cadar defs)))
             (let ((name (if (cscm:var? var)
                             (var-name var)
                             var)))
               (loop (cdr defs)
                     (cons (list name
                                 (c7expr form))
                           cdefs))))))))

(define (c7set! args)
  (let ((var (car args))
        (exp (cadr args)))
    (if (cscm:var? var)
        `(set! ,(var-name var) ,(c7expr exp))
        `(set! ,var ,(c7expr exp)))))

(define (c7quote args)
  `(quote ,@args))

(define (c7symbol-fun fun args)
  (if (cscm:var? fun)
      `(,(var-name fun) ,@(c7args args))
      `(,fun ,@(c7args args))))

(define (c7args args)
  (if (null? args)
      '()
      (cons (c7expr (car args))
            (c7args (cdr args)))))
