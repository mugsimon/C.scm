;; A-Normal-Formへの変更を行う
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
        (else
         (c.scm:c8anf-expr x)))
      (c.scm:c8anf-expr x)))

(define (c.scm:c8anf-function first name lambda-expr)
  (let ((x (c8normalize lambda-expr (lambda (x) x))))
    `(,first ,name ,x)))

;; form->expr
(define (c.scm:c8anf-expr form)
  (c8normalize form (lambda (x) x)))

(define c8*primitive* '(+ - * / = < > car cdr cons pair? list set-car! set-cdr! null? display not remainder memq member symbol? eq? cadr caddr append error map apply assoc))
(define c8*special* '(define set! lambda if quote and or let let* letrec begin delay))

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
  ;;(print "c.scm:debug, c8normalize, m -> " m) ;; debug
  (cond ((pair? m)
         (let ((fun (car m))
               (args (cdr m)))
           (cond ((symbol? fun)
                  (case fun
                    ((lambda) (c8lambda args k))
                    ((let) (c8let args k))
                    ((let*) (c8let* args k))
                    ((if) (c8if args k))
                    ((set!) (c8set! args k))
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ((begin) (c8begin args k))
                    #;((delay) (c8delay args k))
                    ((letrec) (c8letrec args k))
                    ((quote) (c8quote args))
                    (else
                     (c8symbol-fun fun args k))))
                 (else
                  (c8symbol-fun fun args k)))))
        (else
         (k m))))

(define (c8lambda args k)
  (let ((params (car args))
        (body (cadr args)))
    (k `(lambda ,params ,(c8normalize-term body)))))

(define (c8let args k)
  (let ((defs (car args))
        (body (cadr args)))
    (if (null? defs)
        (c8normalize body k)
        (let loop ((exps (map cadr defs))
                   (tmps '()))
          (cond ((null? exps)
                 (c8normalize `(let* ,(map list (map car defs) (reverse tmps)) ,body) k))
                (else
                 (let ((t (newvar)))
                   (c8normalize (car exps)
                                (lambda (n)
                                  (if (c.scm:self-eval? n) ;; 自己評価的データであればわざわざ束縛を回避する必要はない
                                      (loop (cdr exps)
                                            (cons n tmps))
                                      `(let ((,t ,n))
                                         ,(loop (cdr exps)
                                                (cons t tmps)))))))))))))

(define (c8let* args k)
  (let ((defs (car args))
        (body (cadr args)))
    (if (null? defs)
        (c8normalize body k)
        (let ((def (car defs)))
          (c8normalize (cadr def)
                       (lambda (n1)
                         `(let ((,(car def) ,n1))
                            ,(c8normalize `(let* ,(cdr defs) ,body) k))))))))

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
    (c8normalize m
                 (lambda (t)
                   `(let ((,x ,t))
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

(define (c8begin args k)
  (if (null? (cdr args))
      (c8normalize (car args) k)
      (c8normalize-name (car args)
                        (lambda (t)
                          (c8normalize `(begin ,@(cdr args)) k)))))

(define (c8normalize-name m k)
  (c8normalize m (lambda (n)
                   (if (c8value? n)
                       (k n)
                       (let ((t (newvar)))
                         `(let ((,t ,n))
                            ,(k t)))))))

(define (c8normalize-name* m* k)
  (if (null? m*)
      (k '())
      (c8normalize-name (car m*)
                        (lambda (t)
                          (c8normalize-name* (cdr m*)
                                             (lambda (t*)
                                               (k `(,t . ,t*))))))))
