;;; 自由変数のクロージングを行う
;;; letrec式の束縛内になる自由変数を持つlambda式の引数に自由変数を追加する
;;; 自由変数を追加したローカル関数呼び出しの引数に自由変数を追加する

;; (define var (lambda requireds rest body))
;; (define var expr)
;; (begin ...)
;; (fun ...)
(define (c.scm:c3close x)
     (if (pair? x)
         (case (car x)
           ((define)
            (let ((form (caddr x)))
              (if (and (pair? form)
                       (eq? (car form) 'lambda))
                  (c.scm:c3close-function (car x) ;; define
                                            (cadr x) ;; name
                                            (caddr x)) ;; (lambda required rest body)
                  `(,(car x) ,(cadr x) ,(c.scm:c3close-expr form)))))
           ((begin)
            `(begin ,@(map c.scm:c3close (cdr x))))
           (else
            (c.scm:c3close-expr x)))
         (c.scm:c3close-expr x)))

(define (c.scm:c3close-function first name lambda-expr)
  (let ((x (c3expr lambda-expr)))
    `(,first ,name ,x)))

;; form->expr
(define (c.scm:c3close-expr form)
  (let ((x (c3expr form)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c3expr form)
  (cond ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c3if args))
                    ((and) (c3and args))
                    ((or) (c3or args))
                    ((begin) (c3begin args))
                    ((lambda) (c3lambda args))
                    ((delay) (c3delay args))
                    ((let) (c3let args))
                    ((let*) (c3let args))
                    ((letrec) (c3letrec args))
                    ((set!) (c3set! args))
                    ((quote) (c3quote args))
                    (else
                     (c3symbol-fun fun args))))
                 (else
                  (list (c3expr fun) (c3args (car args)))))))
        (else
         (case form
           ((#f) #f)
           ((#t) #t)
           ((()) '())
           (else
            form)))))

(define (c3if args)
  `(if ,(c3expr (car args))
       ,(c3expr (cadr args))
       ,(c3expr (caddr args))))

(define (c3and args)
  `(and ,(map c3expr (car args))))

(define (c3or args)
  `(or ,(map c3expr (car args))))

(define (c3begin args)
  `(begin ,(map c3expr (car args))))  

(define (c3lambda args)
  (let ((requireds (car args))
        (rest (cadr args))
        (body (caddr args)))
    (list 'lambda requireds rest (c3expr body))))

(define (c3lam args . free-vars)
  (let ((requireds (car args))
        (rest (cadr args))
        (body (caddr args)))
    (list (c.scm:union (if (null? free-vars)
                           '()
                           (car free-vars))
                       requireds) rest (c3expr body))))

(define (c3let args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(let ,(reverse cdefs) ,(c3expr (cadr args))))
          (else
           (let ((def (car defs)))
             (loop (cdr defs)
                   (cons (cons (car def)
                               (c3expr (cdr def)))
                         cdefs)))))))

(define (c3letrec args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(letrec ,(reverse cdefs) ,(c3expr (cadr args))))
          (else
           (let ((var (caar defs))
                 (form (cadar defs))
                 (form0 (caddar defs)))
             (loop (cdr defs)
                   (cons (list var
                               (if (var-local-fun var)
                                   (c3lam form (var-local-fun var))
                                   (c3expr form))
                               form0)
                         cdefs)))))))

(define (c3set! args)
  `(set! ,(car args) ,(c3expr (cadr args))))

(define (c3quote args)
  `(quote ,(car args)))

(define (c3symbol-fun fun args)
  (if (and (c.scm:var? fun)
           (var-local-fun fun))
      (list fun (append (var-local-fun fun) (c3args (car args)))) ;; ローカル関数呼び出し
      (list fun (c3args (car args)))))

(define (c3args args)
  (if (null? args)
      '()
      (cons (c3expr (car args))
            (c3args (cdr args)))))

