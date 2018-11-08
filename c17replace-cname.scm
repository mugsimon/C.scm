(define (c17replace-cname x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c17def-func first
                            name
                            form)
               (c17def-expr first
                            name
                            form))))
        (else
         (error "CSCM:ERROR, c17replace-cname, not a definition" x)))
      (error "CSCM:ERROR, c17replace-cname, not a definition" x)))

(define (c17def-func first name lambda-expr)
  (let ((x (c17expr lambda-expr)))
    `(,first ,name ,x)))

(define (c17def-expr first name expr)
  (let ((x (c17expr expr)))
    `(,first ,name ,x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (c17expr form)
  (cond ((cscm:symbol? form)
         (c17vref form))
        ((cscm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((cscm:symbol? fun)
                  (case fun
                    ((if) (c17if args))
                    ((and) (c17and args))
                    ((or) (c17or args))
                    ((begin) (c17begin args))
                    ((lambda) (c17lambda args))
                    ((delay) (c17delay args))
                    ((let) (c17let args))
                    ((let*) (c17let* args))
                    ((letrec) (c17letrec args))
                    ((set!) (c17set! args))
                    ((quote) (c17quote args))
                    (else
                     (c17symbol-fun fun args))))
                 (else
                  `(,(c17expr fun) ,@(c17args args))))))
        (else
         (case form
           ((#t) #t)
           ((#f) #f)
           ((()) '())
           (else
            form)))))

(define (c17if args)
  `(if ,(c17expr (car args))
       ,(c17expr (cadr args))
       ,(c17expr (caddr args))))

(define (c17and args)
  `(and ,@(map c17expr args)))

(define (c17or args)
  `(or ,@(map c17expr args)))

(define (c17begin args)
  `(begin ,@(map c17expr args)))
  
(define (c17lambda args)
  (let ((params (car args))
        (body (cadr args)))
    `(lambda ,params ,(c17expr body))))

(define (c17let args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((vars (map c17expr (map car defs)))
          (exps (map c17expr (map cadr defs))))
      `(let ,(map list vars exps) ,(c17expr body)))))

(define (c17let* args)
  (let ((defs (car args))
        (body (cadr args)))
        (let ((vars (map c17expr (map car defs)))
              (exps (map c17expr (map cadr defs))))
          `(let* ,(map list vars exps) ,(c17expr body)))))

(define (c17letrec args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((vars (map c17expr (map car defs)))
          (exps (map c17expr (map cadr defs))))
      `(letrec ,(map list vars exps) ,(c17expr body)))))

(define (c17set! args)
  (let ((var (car args))
        (exp (cadr args)))
    `(set! ,(c17vref var) ,(c17expr exp))))

(define (c17quote args)
  `(quote ,@args))

(define (c17symbol-fun fun args)
  `(,(c17vref fun) ,@(c17args args)))

(define (c17args args)
  (if (null? args)
      '()
      (cons (c17expr (car args))
            (c17args (cdr args)))))

(define (c17vref form)
  (if (cscm:var? form)
      form
      (let ((cname (assq form *rename-alist*)))
        (if cname
            (cdr cname)
            form))))           
