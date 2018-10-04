;; (define var (lambda params body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
;; sexp
(define (c.scm:c7scheme x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c7scheme-function (car x) ;; define
                                            (cadr x) ;; name
                                            (caddr x)) ;; (lambda params body)
                  `(,(car x) ,(cadr x) ,(c.scm:c7scheme-expr form)))))
           ((begin)
            `(begin ,@(map c.scm:c7scheme (cdr x))))
           (else
            (c.scm:c7scheme-expr x)))
         (c.scm:c7scheme-expr x)))

(define (c.scm:c7scheme-function first name lambda-expr)
  (let ((x (c7expr lambda-expr))
        (name (if (c.scm:var? name)
                  (var-name name)
                  name)))
    `(,first ,name ,x)))

;; form->expr
(define (c.scm:c7scheme-expr form)
  (let ((x (c7expr form)))
    x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c7expr form)
  (cond ((c.scm:symbol? form)
         (c7vref form))
        ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c7if args))
                    ((and) (c7and args))
                    ((or) (c7or args))
                    ((begin) (c7begin args))
                    ((lambda) (c7lambda args))
                    ((delay) (c7delay args))
                    ((let) (c7let args))
                    ((let*) (c7let args))
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
  (if (c.scm:var? form)
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
        ((c.scm:var? params)
         (var-name params))
        (else
         (cons (var-name (car params)) (c7params (cdr params))))))

(define (c7let args)
  (if (c.scm:var? (car args))
      (c7named-let args)
      (let loop ((defs (car args))
                 (cdefs '()))
        (cond ((null? defs)
               `(let ,(reverse cdefs) ,(c7expr (cadr args))))
              (else
               (let ((def (car defs)))
                 (loop (cdr defs)
                       (cons (list (var-name (car def))
                                   (c7expr (cadr def)))
                             cdefs))))))))

(define (c7named-let args)
  (let loop ((defs (cadr args))
             (cdefs '()))
    (cond ((null? defs)
           (let ((var (car args))
                 (body (caddr args)))
             `(let ,(var-name var) ,(reverse cdefs) ,(c7expr body))))
          (else
           (let ((def (car defs)))
             (loop (cdr defs)
                   (cons (list (var-name (car def))
                               (c7expr (cadr def)))
                         cdefs)))))))

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

(define (c7set! args)
  (if (c.scm:var? (car args))
      `(set! ,(var-name (car args)) ,(c7expr (cadr args)))
      `(set! ,(car args) ,(c7expr (cadr args)))))

(define (c7quote args)
  `(quote ,@args))

(define (c7symbol-fun fun args)
  (if (c.scm:var? fun)
      `(,(var-name fun) ,@(c7args args))
      `(,fun ,@(c7args args))))

(define (c7args args)
  (if (null? args)
      '()
      (cons (c7expr (car args))
            (c7args (cdr args)))))


