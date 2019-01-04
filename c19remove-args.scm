(define (c19remove-args x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c19def-func first
                            name
                            form)
               (c19def-expr first
                            name
                            form))))
        (else
         (error "CSCM:ERROR, c19remove-args, not a definition" x)))
      (error "CSCM:ERROR, c19remove-args, not a definition" x)))

(define (c19def-func first name lambda-expr)
  (let ((x (c19expr lambda-expr)))
    `(,first ,name ,x)))

(define (c19def-expr first name expr)
  (let ((x (c19expr expr)))
    `(,first ,name ,x)))

(define (c19cfunc? arg)
  ;;(print "cscm:debug, c19cfun? arg-> " (if (cscm:var? arg) (var-name arg) #f)) ;; debug
  (if (cscm:var? arg)
      (let ((name (symbol->string (var-name arg))))
        (let ((len (string-length name)))
          (if (< len 6)
              #f
              (let ((head (substring name 0 6)))
                (equal? head "c_cscm")))))
      #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (c19expr form)
  (cond ((cscm:symbol? form)
         (c19vref form))
        ((cscm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((cscm:symbol? fun)
                  (case fun
                    ((if) (c19if args))
                    ((and) (c19and args))
                    ((or) (c19or args))
                    ((begin) (c19begin args))
                    ((lambda) (c19lambda args))
                    ((delay) (c19delay args))
                    ((let) (c19let args))
                    ((let*) (c19let* args))
                    ((letrec) (c19letrec args))
                    ((set!) (c19set! args))
                    ((quote) (c19quote args))
                    (else
                     (c19symbol-fun fun args))))
                 (else
                  `(,(c19expr fun) ,@(c19args args))))))
        (else
         (case form
           ((#t) #t)
           ((#f) #f)
           ((()) '())
           (else
            form)))))

(define (c19if args)
  `(if ,(c19expr (car args))
       ,(c19expr (cadr args))
       ,(c19expr (caddr args))))

(define (c19and args)
  `(and ,@(map c19expr args)))

(define (c19or args)
  `(or ,@(map c19expr args)))

(define (c19begin args)
  `(begin ,@(map c19expr args)))
  
(define (c19lambda args)
  (let ((params (c19params (car args)))
        (body (cadr args)))
    `(lambda ,params ,(c19expr body))))

(define (c19params params)
  (if (null? params)
      '()
      (if (c19cfunc? (car params))
          (c19params (cdr params))
          (cons (car params)
                (c19params (cdr params))))))
      

(define (c19let args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((vars (map c19expr (map car defs)))
          (exps (map c19expr (map cadr defs))))
      `(let ,(map list vars exps) ,(c19expr body)))))

(define (c19let* args)
  (let ((defs (car args))
        (body (cadr args)))
        (let ((vars (map c19expr (map car defs)))
              (exps (map c19expr (map cadr defs))))
          `(let* ,(map list vars exps) ,(c19expr body)))))

(define (c19letrec args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((vars (map c19expr (map car defs)))
          (exps (map c19expr (map cadr defs))))
      `(letrec ,(map list vars exps) ,(c19expr body)))))

(define (c19set! args)
  (let ((var (car args))
        (exp (cadr args)))
    `(set! ,(c19vref var) ,(c19expr exp))))

(define (c19quote args)
  `(quote ,@args))

(define (c19symbol-fun fun args)
  `(,(c19vref fun) ,@(c19args args)))

(define (c19args args)
  (if (null? args)
      '()
      (if (c19cfunc? (car args))
          (c19args (cdr args))
          (cons (c19expr (car args))
                (c19args (cdr args))))))

(define (c19vref form)
  form)
