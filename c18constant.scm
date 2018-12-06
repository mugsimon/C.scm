(define (c18constant x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c18def-func first
                            name
                            form)
               (c18def-expr first
                            name
                            form))))
        (else
         (error "CSCM:ERROR, c18constant, not a definition" x)))
      (error "CSCM:ERROR, c18constant, not a definition" x)))

(define (c18def-func first name lambda-expr)
  (let ((x (c18expr lambda-expr)))
    `(,first ,name ,x)))

(define (c18def-expr first name expr)
  (let ((x (c18expr expr)))
    `(,first ,name ,x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (c18expr form)
  (cond ((cscm:symbol? form)
         (c18vref form))
        ((cscm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((cscm:symbol? fun)
                  (case fun
                    ((if) (c18if args))
                    ((and) (c18and args))
                    ((or) (c18or args))
                    ((begin) (c18begin args))
                    ((lambda) (c18lambda args))
                    ((delay) (c18delay args))
                    ((let) (c18let args))
                    ((let*) (c18let* args))
                    ((letrec) (c18letrec args))
                    ((set!) (c18set! args))
                    ((quote) (c18quote args))
                    (else
                     (c18symbol-fun fun args))))
                 (else
                  `(,(c18expr fun) ,@(c18args args))))))
        (else
         (case form
           ((#t) #t)
           ((#f) #f)
           ((()) '())
           (else
            form)))))

(define (c18if args)
  `(if ,(c18expr (car args))
       ,(c18expr (cadr args))
       ,(c18expr (caddr args))))

(define (c18and args)
  `(and ,@(map c18expr args)))

(define (c18or args)
  `(or ,@(map c18expr args)))

(define (c18begin args)
  `(begin ,@(map c18expr args)))
  
(define (c18lambda args)
  (let ((params (car args))
        (body (cadr args)))
    `(lambda ,params ,(c18expr body))))

(define (c18let args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((vars (map c18expr (map car defs)))
          (exps (map c18expr (map cadr defs))))
      `(let ,(map list vars exps) ,(c18expr body)))))

(define (c18let* args)
  (let ((defs (car args))
        (body (cadr args)))
        (let ((vars (map c18expr (map car defs)))
              (exps (map c18expr (map cadr defs))))
          `(let* ,(map list vars exps) ,(c18expr body)))))

(define (c18letrec args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((vars (map c18expr (map car defs)))
          (exps (map c18expr (map cadr defs))))
      `(letrec ,(map list vars exps) ,(c18expr body)))))

(define (c18set! args)
  (let ((var (car args))
        (exp (cadr args)))
    `(set! ,(c18vref var) ,(c18expr exp))))

(define (c18quote args)
  (if (cscm:pair? (car args))
      (let ((name (newvar "cscm_constant")))
        (set! *cscm-constant* (cons `(define ,name (quote ,@args)) *cscm-constant*))
        name)
      `(quote ,@args)))


(define (c18symbol-fun fun args)
  `(,(c18vref fun) ,@(c18args args)))

(define (c18args args)
  (if (null? args)
      '()
      (cons (c18expr (car args))
            (c18args (cdr args)))))

(define (c18vref form)
  form)
