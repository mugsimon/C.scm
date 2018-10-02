;; (define var (lambda params body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
;; sexp
(define (c.scm:c5hoist x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c5hoist-function (car x) ;; define
                                            (cadr x) ;; name
                                            (caddr x)) ;; (lambda params body)
                  `(,(car x) ,(cadr x) ,(c.scm:c5hoist-expr form)))))
           ((begin)
            `(begin ,@(map c.scm:c5hoist (cdr x))))
           (else
            (c.scm:c5hoist-expr x)))
         (c.scm:c5hoist-expr x)))

(define (c.scm:c5hoist-function first name lambda-expr)
  (let ((x (c5expr lambda-expr)))
    `(,first ,name ,x)))

;; form->expr
(define (c.scm:c5hoist-expr form)
  (let ((x (c5expr form)))
    x))

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

(define-syntax dolist
  (syntax-rules ()
    ((dolist (var lst) body ...)
     (let loop ((rest lst))
       (cond ((null? rest)
              '())
             (else
              (let ((var (car rest)))
                (begin body ...)
                (loop (cdr rest)))))))))

(define (c.scm:union x y)
  (cond ((null? x)
         y)
        ((member (car x) y)
         (c.scm:union (cdr x) y))
        (else
         (cons (car x) (c.scm:union (cdr x) y)))))

(define (c.scm:difference x y)
  (cond ((null? x)
         '())
        ((member (car x) y)
         (c.scm:difference (cdr x) y))
        (else
         (cons (car x) (c.scm:difference (cdr x) y)))))

(define c.scm:*c5local-functions* '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c5expr form)
  (cond ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c5if args))
                    ((and) (c5and args))
                    ((or) (c5or args))
                    ((begin) (c5begin args))
                    ((lambda) (c5lambda args))
                    ((delay) (c5delay args))
                    ((let) (c5let args))
                    ((let*) (c5let args))
                    ((letrec) (c5letrec args))
                    ((set!) (c5set! args))
                    ((quote) (c5quote args))
                    (else
                     (c5symbol-fun fun args))))
                 (else
                  `(,(c5expr fun) ,@(c5args args))))))
        (else
         (case form
           ((#f) #f)
           ((#t) #t)
           ((()) '())
           (else
            form)))))

(define (c5if args)
  `(if ,(c5expr (car args))
       ,(c5expr (cadr args))
       ,(c5expr (caddr args))))

(define (c5and args)
  `(and ,@(map c5expr args)))

(define (c5or args)
  `(or ,@(map c5expr args)))

(define (c5begin args)
  `(begin ,@(map c5expr args)))

(define (c5lambda args)
  (let ((params (car args))
        (body (cadr args)))
    (list 'lambda params (c5expr body))))

#;(define (c5lam args)
  (let ((requireds (car args))
        (rest (cadr args))
        (body (caddr args)))
    (list requireds rest (c5expr body))))

(define (c5let args)
  (if (c.scm:var? (car args))
      (c5named-let args)
      (let loop ((defs (car args))
                 (cdefs '()))
        (cond ((null? defs)
               (if (null? cdefs)
                   (c5expr (cadr args))
                   `(let ,(reverse cdefs) ,(c5expr (cadr args)))))
              (else
               (let ((def (car defs)))
                 (if (var-local-fun (car def))
                     (begin (c5hoist-fun def)
                            (loop (cdr defs)
                                  cdefs))
                     (loop (cdr defs)
                           (cons (list (car def)
                                       (c5expr (cadr def)))
                                 cdefs)))))))))

(define (c5hoist-fun def)
  (set! c.scm:*c5local-functions* (cons def c.scm:*c5local-functions*)))

(define (c5letrec args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           (if (null? cdefs)
               (c5expr (cadr args))
               `(letrec ,(reverse cdefs) ,(c5expr (cadr args)))))
          (else
           (let ((var (caar defs))
                 (form (cadar defs)))
             (if (var-local-fun var)
                 (begin (c5hoist-fun (car defs))
                        (loop (cdr defs)
                              cdefs))
                 (loop (cdr defs)
                       (cons (list var
                                   (c5expr form))
                             cdefs))))))))

(define (c5set! args)
  `(set! ,(car args) ,(c5expr (cadr args))))

(define (c5quote args)
  `(quote ,@args))

(define (c5symbol-fun fun args)
  `(,fun ,@(c5args args)))

(define (c5args args)
  (if (null? args)
      '()
      (cons (c5expr (car args))
            (c5args (cdr args)))))


