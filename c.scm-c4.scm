;; (define var (lambda requireds rest body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
(define (c.scm:c4hoist x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c4hoist-function (car x) ;; define
                                            (cadr x) ;; name
                                            (caddr x)) ;; (lambda required rest body)
                  `(,(car x) ,(cadr x) ,(c.scm:c4hoist-expr form)))))
           ((begin)
            `(begin ,@(map c.scm:c4hoist (cdr x))))
           (else
            (c.scm:c4hoist-expr x)))
         (c.scm:c4hoist-expr x)))

(define (c.scm:c4hoist-function first name lambda-expr)
  (let ((x (c4expr lambda-expr)))
    `(,first ,name ,x)))

;; form->expr
(define (c.scm:c4hoist-expr form)
  (let ((x (c4expr form)))
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

(define c.scm:*c4local-functions* '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c4expr form)
  (cond ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c4if args))
                    ((and) (c4and args))
                    ((or) (c4or args))
                    ((begin) (c4begin args))
                    ((lambda) (c4lambda args))
                    ((delay) (c4delay args))
                    ((let) (c4let args))
                    ((let*) (c4let args))
                    ((letrec) (c4letrec args))
                    ((set!) (c4set! args))
                    ((quote) (c4quote args))
                    (else
                     (c4symbol-fun fun args))))
                 (else
                  (list (c4expr fun) (c4args (car args)))))))
        (else
         (case form
           ((#f) #f)
           ((#t) #t)
           ((()) '())
           (else
            form)))))

(define (c4if args)
  `(if ,(c4expr (car args))
       ,(c4expr (cadr args))
       ,(c4expr (caddr args))))

(define (c4and args)
  `(and ,(map c4expr (car args))))

(define (c4or args)
  `(or ,(map c4expr (car args))))

(define (c4begin args)
  `(begin ,(map c4expr (car args))))  

(define (c4lambda args)
  (let ((requireds (car args))
        (rest (cadr args))
        (body (caddr args)))
    (list 'lambda requireds rest (c4expr body))))

(define (c4lam args)
  (let ((requireds (car args))
        (rest (cadr args))
        (body (caddr args)))
    (list requireds rest (c4expr body))))

(define (c4let args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(let ,(reverse cdefs) ,(c4expr (cadr args))))
          (else
           (let ((def (car defs)))
             (loop (cdr defs)
                   (cons (cons (car def)
                               (c4expr (cdr def)))
                         cdefs)))))))

(define (c4letrec args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           (if (null? cdefs)
               (c4expr (cadr args))
               `(letrec ,(reverse cdefs) ,(c4expr (cadr args)))))
          (else
           (let ((var (caar defs))
                 (form (cadar defs))
                 (form0 (caddar defs)))
             (if (var-local-fun var)
                 (begin (c4hoist-local-fun (car defs))
                        (loop (cdr defs)
                              cdefs))
                 (loop (cdr defs)
                       (cons (list var
                                   (c4expr form)
                                   form0)
                             cdefs))))))))

(define (c4hoist-local-fun def)
  (let ((var (car def))
        (form (cadr def))
        (form0 (caddr def)))
    (set! c.scm:*c4local-functions* (cons (list var (c4lam form) form0) c.scm:*c4local-functions*))))

(define (c4set! args)
  `(set! ,(car args) ,(c4expr (cadr args))))

(define (c4quote args)
  `(quote ,(car args)))

(define (c4symbol-fun fun args)
  (list fun (c4args (car args))))

(define (c4args args)
  (if (null? args)
      '()
      (cons (c4expr (car args))
            (c4args (cdr args)))))


