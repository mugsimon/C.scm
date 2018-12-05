;; CからSchemeのグローバル関数, CからSchemeのグローバル変数, CからSchemeのローカル関数を呼び出すコードを追加する

(define (c16call-code x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c16def-func first
                            name
                            form)
               (c16def-expr first
                            name
                            form))))
        (else
         (error "CSCM:ERROR, c16call-code, not a definition" x)))
      (error "CSCM:ERROR,  c16call-code, not a definition" x)))

(define *cflag* #f)

(define (c16def-func first name lambda-expr)
  (let ((name (if (cscm:var? name)
                  (var-name name)
                  name)))
    (if (c16c? name)
        (set! *cflag* #t)
        (set! *cflag* #f)))             
  (let ((x (c16expr lambda-expr)))
    `(,first ,name ,x)))

(define (c16def-expr first name expr)
  (let ((name (if (cscm:var? name)
                  (var-name name)
                  name)))
    (if (c16c? name)
        (set! *cflag* #t)
        (set! *cflag* #f)))
  (let ((x (c16expr expr)))
    `(,first ,name ,x)))

(define (c16c? name)
  ;;(print "cscm:debug, c16c?, name -> " name) ;; debug
  (let ((name (symbol->string name)))
    (let ((len (string-length name)))
      (if (< len 3)
          #f
          (let ((head (substring name 0 2)))
            (if (equal? head "c_")
                #t
                #f))))))
 
;;;;;;;;;;;;;;;;;;;;;;
(define (c16constant x)
  (if (cscm:self-eval? x)
      x
      `(quote ,x)))
(define c16null
  ''())
(define c16false
  #f)
(define c16true
  #t)

;; if
(define (c16if args)
  (let ((t (c16fmla (car args)))
        (m1 (c16expr (cadr args)))
        (m2 (c16expr (caddr args))))
    `(if ,t ,m1 ,m2)))

;; if fmla
(define (c16fmla fmla)
  (if (pair? fmla)
      (case (car fmla)
        ((and)
         (cons 'and (map c16fmla (cdr fmla))))
        ((or)
         (cons 'or (map c16fmla (cdr fmla))))
        ((not)
         (list 'not (c16fmla (cadr fmla))))
        (else
         (c16expr fmla)))
      (c16expr fmla)))

;; and
(define (c16and args)
  `(and ,@(c16args args)))
              
;; or
(define (c16or args)
  `(or ,@(c16args args)))

(define (c16expr form)
  ;;(print "c.scm:debug, c16expr, form -> " form) ;; debug
  (cond ((cscm:symbol? form)
         (c16vref form))
        ((cscm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((cscm:symbol? fun)
                  (case fun
                    ((if) (c16if args))
                    ((and) (c16and args))
                    ((or) (c16or args))
                    ((begin) (c16begin args))
                    ((lambda) (c16lambda args))
                    ((delay) (c16delay args))
                    ((let) (c16let args))
                    ((let*) (c16let* args))
                    ((letrec) (c16letrec args))
                    ((set!) (c16set! args))
                    ((quote) (c16quote args))
                    ((list) (c16list args))
                    ((append) (c16append args))
                    (else
                     (c16symbol-fun fun args))))
                 (else
                  `(,(c16expr fun) ,@(c16args args))))))
        (else
         (case form
           ((#f) c16false)
           ((#t) c16true)
           ((()) c16null)
           (else (c16constant form))))))

;; (f args)
(define (c16symbol-fun name args)
  ;;(print "cscm:debug, c16symbol-fun, name -> " name) ;; debug
  (cond ((or (and *cflag* ;; 自分がC
                  (cscm:var? name) ;; 呼び出し対象がトップレベル（もとからグローバル）ではない
                  (var-local-fun name) ;; ホイストされている
                  (not (c16c? (var-name name)))) ;; schemeである
             (and *cflag* ;; 自分がC
                  (not (cscm:var? name)) ;; 呼び出し対象がトップレベル
                  (not (cscm:primitive? name)) ;; プリミティブではない
                  (not (cscm:library? name)) ;; ライブラリではない
                  (not (c16c? name)))) ;; schemeである
         (let ((n (length args)))
           (case n
             ((0) `(cscm_apply0 (cscm_gvref ,name) ,@(c16args args)))
             ((1) `(cscm_apply1 (cscm_gvref ,name) ,@(c16args args)))
             ((2) `(cscm_apply2 (cscm_gvref ,name) ,@(c16args args)))
             ((3) `(cscm_apply3 (cscm_gvref ,name) ,@(c16args args)))
             ((4) `(cscm_apply4 (cscm_gvref ,name) ,@(c16args args)))
             ((5) `(cscm_apply5 (cscm_gvref ,name) ,@(c16args args)))
             ((6) `(cscm_apply6 (cscm_gvref ,name) ,@(c16args args)))
             ((7) `(cscm_apply7 (cscm_gvref ,name) ,@(c16args args)))
             ((8) `(cscm_apply8 (cscm_gvref ,name) ,@(c16args args)))
             (else
              (error "CSCM:ERROR, c16symbol-fun, too many argument" n)))))
        ((and *cflag* ;; 自分がC
              (cscm:var? name) ;; 呼び出し対象がトップレベルではない
              (not (var-local-fun name)) ;; ホイストされていない
              (not (c16c? (var-name name)))) ;;schemeである
         (let ((n (length args)))
           (case n
             ((0) `(cscm_apply0 ,name ,@(c16args args)))
             ((1) `(cscm_apply1 ,name ,@(c16args args)))
             ((2) `(cscm_apply2 ,name ,@(c16args args)))
             ((3) `(cscm_apply3 ,name ,@(c16args args)))
             ((4) `(cscm_apply4 ,name ,@(c16args args)))
             ((5) `(cscm_apply5 ,name ,@(c16args args)))
             ((6) `(cscm_apply6 ,name ,@(c16args args)))
             ((7) `(cscm_apply7 ,name ,@(c16args args)))
             ((8) `(cscm_apply8 ,name ,@(c16args args)))
             (else
              (error "CSCM:ERROR, c16symbol-fun, too many argument" n)))))
        (else
         `(,name ,@(c16args args)))))

;;
(define (c16args forms)
  (if (null? forms)
      '()
      (cons (c16expr (car forms))
            (c16args (cdr forms)))))

;;
(define (c16begin forms)
  `(begin ,@(map c16expr forms)))

(define (c16body body)
  (c16expr body))

;;
(define (c16lambda args)
  (let ((params (car args))
        (body (cadr args)))
    (list `lambda params (c16expr body))))
          
(define (c16let args)
  (let ((body (cadr args))
        (defs (car args)))
    (let ((vars (map car defs))
          (exps (map c16expr (map cadr defs))))
      `(let ,(map list vars exps) ,(c16expr body)))))

(define (c16let* args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((vars (map car defs))
          (exps (map c16expr (map cadr defs))))
      `(let* ,(map list vars exps) ,(c16expr body)))))

(define (c16letrec args)
  (let ((defs (car args))
        (body (cadr args)))
    (let ((vars (map car defs))
          (exps (map c16expr (map cadr defs))))
      `(letrec ,(map list vars exps) ,(c16expr body)))))

(define (c16quote args)
  (case (car args)
    ((()) c16null)
    (else (c16constant (car args)))))

(define (c16vref name)
  ;;(print "c.scm:debug, c16vref, name -> " name) ;; debug
  (cond ((and *cflag*
              (not (cscm:var? name)))
         `(cscm_gvref ,name))
        (else
         name)))
               
(define (c16set! args)
  (let ((var (car args))
        (exp (cadr args)))
    `(set! ,var ,(c16expr exp))))

;; list
(define (c16list args)
  (let ((n (length args)))
    (case n
      ((0) `(cscm_apply0 (cscm_gvref list) ,@(c16args args)))
      ((1) `(cscm_apply1 (cscm_gvref list) ,@(c16args args)))
      ((2) `(cscm_apply2 (cscm_gvref list) ,@(c16args args)))
      ((3) `(cscm_apply3 (cscm_gvref list) ,@(c16args args)))
      ((4) `(cscm_apply4 (cscm_gvref list) ,@(c16args args)))
      ((5) `(cscm_apply5 (cscm_gvref list) ,@(c16args args)))
      ((6) `(cscm_apply6 (cscm_gvref list) ,@(c16args args)))
      ((7) `(cscm_apply7 (cscm_gvref list) ,@(c16args args)))
      ((8) `(cscm_apply8 (cscm_gvref list) ,@(c16args args))))))

(define (c16append args)
  (let ((n (length args)))
    (case n
      ((0) `(cscm_apply0 (cscm_gvref append) ,@(c16args args)))
      ((1) `(cscm_apply1 (cscm_gvref append) ,@(c16args args)))
      ((2) `(cscm_apply2 (cscm_gvref append) ,@(c16args args)))
      ((3) `(cscm_apply3 (cscm_gvref append) ,@(c16args args)))
      ((4) `(cscm_apply4 (cscm_gvref append) ,@(c16args args)))
      ((5) `(cscm_apply5 (cscm_gvref append) ,@(c16args args)))
      ((6) `(cscm_apply6 (cscm_gvref append) ,@(c16args args)))
      ((7) `(cscm_apply7 (cscm_gvref append) ,@(c16args args)))
      ((8) `(cscm_apply8 (cscm_gvref append) ,@(c16args args))))))
