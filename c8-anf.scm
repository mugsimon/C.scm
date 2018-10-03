;; (define var (lambda params body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
;; sexp
(define (c.scm:c8anf x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c8anf-function (car x) ;; define
                                            (cadr x) ;; name
                                            (caddr x)) ;; (lambda params body)
                  `(,(car x) ,(cadr x) ,(c.scm:c8anf-expr form)))))
           ((begin)
            `(begin ,@(map c.scm:c8anf (cdr x))))
           (else
            (c.scm:c8anf-expr x)))
         (c.scm:c8anf-expr x)))

(define (c.scm:c8anf-function first name lambda-expr)
  (let ((x (c8expr lambda-expr)))
    `(,first ,name ,x)))

;; form->expr
(define (c.scm:c8anf-expr form)
  (let ((x (c8expr form)))
    x))

(define c8*primitive* '(+ - * / = < > car cdr cons pair? list set-car! set-cdr! null? display not remainder memq member symbol? eq? cadr caddr append error map apply assoc))
(define c8*special* '(define set! lambda if quote and or let let* letrec begin delay))
(define *newvar-name* "c.scm")
(define *newvar* 0)
(define (newvar . name)
  (set! *newvar* (+ *newvar* 1))
  (if (null? name)
      (string->symbol
       (string-append *newvar-name* (number->string *newvar*)))	 
      (string->symbol
       (string-append (car name) (number->string *newvar*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c8primop? fn)
  (memq fn c8*primitive*))

(define (c8value? n)
  (if (pair? n)
      (memq (car n) c8*special*)
      #t))

(define (c8normalize-term m)
  (c8normalize m (lambda (x) x)))

(define (c8normalize m k)
  (cond ((pair? m)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((symbol? fun)
                  (case fun
                    ((lambda) (c8lambda args k))
                    ((let) (c8let args k))
                    ((let*) (c8let* args k))
                    ((if) (c8if args k))
                    ((set!) (c8set! args k))
           ;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ((begin) (c8begin args k))
                    ((delay) (c8delay args k))
                    ((letrec) (c8letrec args k))
                    ((quote) (c8quote args))
                    (else
                     (c8symbol-fun fun args k))))
                 (else
                  `(,(c8normalize fun) ,@(c8args args))))))
        (else
         (k m))))




        
(define (c8expr form)
  (cond ((c.scm:symbol? form)
         (c8vref form))
        ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c8if args))
                    ((and) (c8and args))
                    ((or) (c8or args))
                    ((begin) (c8begin args))
                    ((lambda) (c8lambda args))
                    ((delay) (c8delay args))
                    ((let) (c8let args))
                    ((let*) (c8let args))
                    ((letrec) (c8letrec args))
                    ((set!) (c8set! args))
                    ((quote) (c8quote args))
                    (else
                     (c8symbol-fun fun args))))
                 (else
                  `(,(c8expr fun) ,@(c8args args))))))
        (else
         (case form
           ((#f) #f)
           ((#t) #t)
           ((()) '())
           (else
            form)))))

(define (c8vref form)
  (if (c.scm:var? form)
      (var-name form)
      form))

(define (c8if args)
  `(if ,(c8expr (car args))
       ,(c8expr (cadr args))
       ,(c8expr (caddr args))))

(define (c8and args)
  `(and ,@(map c8expr args)))

(define (c8or args)
  `(or ,@(map c8expr args)))

(define (c8begin args)
  `(begin ,@(map c8expr args)))

(define (c8lambda args)
  (let ((params (car args))
        (body (cadr args)))
    (list 'lambda params (c8expr body))))

(define (c8let args)
  (if (c.scm:var? (car args))
      (c8named-let args)
      (let loop ((defs (car args))
                 (cdefs '()))
        (cond ((null? defs)
               `(let ,(reverse cdefs) ,(c8expr (cadr args)))))
              (else
               (let ((def (car defs)))
                 (loop (cdr defs)
                       (cons (list (var-name (car def))
                                   (c8expr (cadr def)))
                             cdefs)))))))

(define (c8named-let args)
  (let loop ((defs (cadr args))
             (cdefs '()))
    (cond ((null? defs)
           (let ((var (car args))
                 (body (caddr args)))
             `(let ,(var-name var) ,(reverse cdefs) ,(c8expr body))))
          (else
           (let ((def (car defs)))
             (loop (cdr defs)
                   (cons (list (var-name (car def))
                               (c8expr (cadr def)))
                         cdefs)))))))

(define (c8letrec args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(letrec ,(reverse cdefs) ,(c8expr (cadr args)))))
          (else
           (let ((var (caar defs))
                 (form (cadar defs)))
             (loop (cdr defs)
                   (cons (list (var-name var)
                               (c8expr form))
                         cdefs))))))

(define (c8set! args)
  (if (c.scm:var? (car args))
      `(set! ,(var-name (car args)) ,(c8expr (cadr args)))
      `(set! ,(car args) ,(c8expr (cadr args)))))

(define (c8quote args)
  `(quote ,@args))

(define (c8symbol-fun fun args)
  (if (c.scm:var? (car args))
      `(,(var-name fun) ,@(c8args args))
      `(,fun ,@(c8args args))))

(define (c8args args)
  (if (null? args)
      '()
      (cons (c8expr (car args))
            (c8args (cdr args)))))


