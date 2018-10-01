;; (define var (lambda requireds rest body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
(define (c.scm:c6normalize x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c6normalize-function (car x) ;; define
                                               (cadr x) ;; name
                                               (caddr x)) ;; (lambda required rest body)
                  `(,(car x) ,(cadr x) ,(c.scm:c6normalize-expr form)))))
           ((begin)
            `(begin ,@(map c.scm:c6normalize (cdr x))))
           (else
            (c.scm:c6normalize-expr x)))
         (c.scm:c6normalize-expr x)))

(define (c.scm:c6normalize-function first name lambda-expr)
  (let ((x (c6lam (cdr lambda-expr))))
    (list first name (cons 'lambda x))))

;; form->expr
(define (c.scm:c6normalize-expr form)
  (c6expr form))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c6expr form)
  (cond ((c.scm:symbol? form)
         (c6vref form))
        ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c6if args))
                    ((and) (c6and args))
                    ((or) (c6or args))
                    ((begin) (c6begin args))
                    ((lambda) (c6lambda args))
                    ((delay) (c0delay args)) ;; (delay args)
                    ((let) (c0let args)) 
                    ((let*) (c0let* args))
                    ((letrec) (c0letrec args))
                    ((set!) (c0set! args)) ;;
                    ((quote) (c0quote args))
                    (else
                     (c6symbol-fun fun args))))
                 (else
                  `(,(c6expr fun) ,@(c6args (car args)))))))
        (else
         form)))

(define (c6vref name)
  (if (c.scm:var? name)
      (var-name name)
      name))

(define (c6if args)
  `(if ,(c6expr (car args))
       ,(c6expr (cadr args))
       ,(c6expr (caddr args))))

(define (c6and args)
  `(and ,@(c6args (car args))))

(define (c6or args)
  `(or ,@(c6args (car args))))

(define (c6begin args)
  `(begin ,@(c6map c6expr args)))

(define (c6lambda args)
  (let ((requireds (cadr args))
        (rest (caddr args))
        (body (cadddr args)))
    (let ((params (cond ((and (null? requireds)
                         (null? rest))
                    '())
                   ((null? requireds)
                    (var-name rest))
                   (else))))
    params)))

