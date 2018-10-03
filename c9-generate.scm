;; A-Normal-FormのSchemeを受け取り、Cコードの文字列のリストを返す

(define c9cscm "CSCM")
(define c9make-number "CSCM_MAKE_NUMBER")
(define c9+ "CSCM_PLUS")
(define c9- "CSCM_MINUS")
(define c9* "CSCM_TIMES")
(define c9= "CSCM_EQUAL")
(define c9< "CSCM_LESS")
(define c9> "CSCM_GREATER")
(define c9car "CSCM_CAR")
(define c9cdr "CSCM_CDR")
(define c9cons "CSCM_CONS")
(define c9pair? "CSCM_PAIR_P")

(define c9*output-port* (current-output-port))

(define (c9print . a)
  (let loop ((a a))
    (cond ((null? a)
	   (newline c9*output-port*))
	  (else
	   (display (car a) c9*output-port*)
	   (loop (cdr a))))))

(define (c9display . a)
  (let loop ((a a))
    (cond ((null? a))
	  (else
	   (display (car a) c9*output-port*)
	   (loop (cdr a))))))

;; (define var (lambda params body))
;; (define var expr)
;; sexp
(define (c.scm:c8generate x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c8generate-function (car x) ;; define
                                             (cadr x) ;; name
                                             (caddr x)) ;; (lambda params body)
                  (begin (c9display c9cscm " " (cadr x) " = ")
                         (c.scm:c9generate-expr form))))))
           (else
            ))))

(define (c.scm:c9generate-function first name lambda-expr)
  (c9display 
  (let ((x (c8expr lambda-expr)))
    `(,(stringc9cscm ,name ,x)

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

(define (c8lambda args k)
  (let ((params (car args))
        (body (cadr args)))
    (k `(lambda ,params ,(c8normalize-term body)))))

(define (c8let* args k)
  (let ((defs (car args))
        (body (cadr args)))
    (if (null? defs)
        (c8normalize body k)
        (let ((def (car defs)))
          (c8normalize (cadr def)
                       (lambda (n1)
                         `(let ((,(car def) ,n1))
                            ,(c8normalize `(let ,(cdr defs) ,body) k))))))))

(define (c8if args k)
  (let ((m1 (car args))
        (m2 (cadr args))
        (m3 (caddr args)))
    (c8normalize-name m1
                    (lambda (t)
                      (k `(if ,t
                              ,(c8normalize-term m2)
                              ,(c8normalize-term m3)))))))

(define (c8set! args k)
  (let ((x (car args))
        (m (cadr args)))
    (c8normalize-name m
                      (lambda (t)
                        `(let ((,(newvar "c.scm:set!") (set! ,x ,t)))
                           ,(k x))))))

(define (c8symbol-fun fn m* k)
  (if (c8primop? fn)
      (c8normalize-name* m*
                         (lambda (t*)
                           (k `(,fn . ,t*))))
      (c8normalize-name fn
                        (lambda (t)
                          (c8normalize-name* m*
                                             (lambda (t*)
                                               (k `(,t . ,t*))))))))




#|        
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

|#
