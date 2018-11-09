;; orとandをif文に展開する
;; (define var (lambda params body))
;; (define var expr)

(define (c10or-and x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c10def-func first
                            name
                            form)
               (c10def-expr first
                            name
                            form))))
        (else
         (error "CSCM:ERROR, c10or-and, not a definition" x)))
      (error "CSCM:ERROR, c100or-and, not a definition" x)))

(define (c10def-func first name lambda-expr)
  (let ((x (c10expr lambda-expr)))
    `(,first ,name ,x)))

(define (c10def-expr first name expr)
  (let ((x (c10expr expr)))
    `(,first ,name ,x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c10expr form)
  (cond ((pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((symbol? fun)
                  (case fun
                    ((if) (c10if args))
                    ((and) (c10and args))
                    ((or) (c10or args))
                    ((begin) (c10begin args))
                    ((lambda) (c10lambda args))
                    ((delay) (c10delay args))
                    ((let) (c10let args))
                    ((let*) (c10let* args))
                    ((letrec) (c10letrec args))
                    ((set!) (c10set! args))
                    ((quote) (c10quote args))
                    (else
                     (c10symbol-fun fun args))))
                 (else
                  `(,(c10expr fun) ,@(c10args args))))))
        (else
         (case form
           ((#f) #f)
           ((#t) #t)
           ((()) '())
           (else
            form)))))

(define (c10if args)
  `(if ,(c10expr (car args))
       ,(c10expr (cadr args))
       ,(c10expr (caddr args))))

(define (c10and args)
  (cond ((null? args)
         #t)
        ((null? (cdr args))
         (car args))
        ((null? (cddr args))
         `(if ,(c10expr (car args))
              ,(c10expr (cadr args))
              #f))
        (else
         `(if ,(c10expr (car args))
              ,(c10and (cdr args))
              #f))))

(define (c10or args)
  (cond ((null? args)
         #f)
        ((null? (cdr args))
         (car args))
        ((null? (cddr args))
         (let ((tmp (newvar "or")))
           `(let ((,tmp ,(c10expr (car args))))
              (if ,tmp
                  ,tmp
                  ,(c10expr (cadr args))))))
        (else
         (let ((tmp (newvar "or")))
           `(let ((,tmp ,(c10expr (car args))))
              (if ,tmp
                  ,tmp
                  ,(c10or (cdr args))))))))

(define (c10begin args)
  `(begin ,@(map c10expr args)))

(define (c10lambda args)
  (let ((params (car args))
        (body (cadr args)))
    (list 'lambda params (c10expr body))))

(define (c10let args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           (if (null? cdefs)
               (c10expr (cadr args))
               `(let ,(reverse cdefs) ,(c10expr (cadr args)))))
          (else
           (let ((def (car defs)))
             (loop (cdr defs)
                   (cons (list (car def)
                               (c10expr (cadr def)))
                         cdefs)))))))

(define (c10let* args)
  (let loop ((defs (car args))
                 (cdefs '()))
        (cond ((null? defs)
               (if (null? cdefs)
                   (c10expr (cadr args))
                   `(let* ,(reverse cdefs) ,(c10expr (cadr args)))))
              (else
               (let ((def (car defs)))
                 (loop (cdr defs)
                       (cons (list (car def)
                                   (c10expr (cadr def)))
                             cdefs)))))))

(define (c10letrec args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           (if (null? cdefs)
               (c10expr (cadr args))
               `(letrec ,(reverse cdefs) ,(c10expr (cadr args)))))
          (else
           (let ((var (caar defs))
                 (form (cadar defs)))
             (loop (cdr defs)
                   (cons (list var
                               (c10expr form))
                         cdefs)))))))

(define (c10set! args)
  `(set! ,(car args) ,(c10expr (cadr args))))

(define (c10quote args)
  `(quote ,@args))

(define (c10symbol-fun fun args)
  `(,fun ,@(c10args args)))

(define (c10args args)
  (if (null? args)
      '()
      (cons (c10expr (car args))
            (c10args (cdr args)))))
