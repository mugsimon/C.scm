;; A-Normal-FormのSchemeを受け取り、Cコードの文字列のリストを返す

(define c9cscm "CSCM")
(define c9void "CSCM_VOID")
(define c9make-number "CSCM_MAKE_NUMBER")

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
(define (c.scm:c9generate x)
  (if (pair? x)
      (case (car x)
        ((define)
         (let ((form (caddr x)))
           (if (and (pair? form)
                    (eq? (car form) 'lambda))
               (c.scm:c9generate-function (car x) ;; define
                                          (cadr x) ;; name
                                          (caddr x)) ;; (lambda params body)
               (begin (c9display c9cscm " " (cadr x) " = ")
                      (c.scm:c9generate-expr form))))))))

(define (c.scm:c9generate-function first name lambda-expr)
  (c9display c9cscm " " name)
  (c9expr lambda-expr #t))

;; form->expr
(define (c.scm:c9generate-expr form)
  (c9expr lambda-expr #t))

;;(define c9*primitive* '(+ - * / = < > car cdr cons pair? list set-car! set-cdr! null? display not remainder memq member symbol? eq? cadr caddr append error map apply assoc))
(define c9*primitive* (list (cons '+ "CSCM_PLUS")
                            (cons '- "CSCM_MINUS")
                            (cons '* "CSCM_TIMES")
                            (cons '/ "CSCM_DIFF")
                            (cons '= "CSCM_EQUAL")
                            (cons '< "CSCM_LESS")
                            (cons '> "CSCM_MORE")
                            (cons 'car "CSCM_CAR")
                            (cons 'cdr "CSCM_CDR")
                            (cons 'cons "CSCM_CONS")
                            (cons 'pair? "CSCM_PAIR_P")))
(define c9*special* '(define set! lambda if quote and or let let* letrec begin delay))
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
(define (c9primop? fn)
  (memq fn c9*primitive*))

(define (c9value? n)
  (if (pair? n)
      (memq (car n) c9*special*)
      #t))

(define (c9expr form r)
  (cond ((pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((symbol? fun)
                  (case fun
                    ((lambda) (c9lambda args r))
                    ((let) (c9let args r))
                    ((if) (c9if args r))
                    (else (c9symbol-fun fun args r)))))))
        (else
         (cond ((symbol? form)
                (c9symbol form r))
               ((number? form)
                (c9number form r))))))

(define (c9lambda args r)
  (c9display "(")
  (let ((params (car args))
        (body (cadr args)))
    (if (null? params)
        (c9display c9void)
        (let loop ((params params))
          (cond ((null? (cdr params))
                 (c9display c9cscm " " (car params)))
                (else
                 (c9display c9cscm " " (car params) ", ")
                 (loop (cdr params))))))
    (c9print "){")
    (c9expr body #t)
    (c9print "}")))

(define (c9let args r)
  (let ((def (car args)))
    (let ((var (car def))
          (val (cadr def))
          (m (caddr def)))
      (c9display c9cscm " " var " = ")
      (c9expr val #f)
      (c9print ";")
      (c9expr m #t))))

(define (c9if args r)
  (let ((m1 (car args))
        (m2 (cadr args))
        (m3 (caddr args)))
    (c9display "if (")
    (c9expr m1 #f)
    (c9print ") {")
    (c9expr m2 #t)
    (c9print "} else {")
    (c9expr m3 #t)
    (c9print "}")))

(define (c9symbol-fun fun args r)
  (if r (c9display "return "))
  (c9expr fun #f)
  (c9display "(")
  (if (not (null? args))
      (let loop ((args args))
        (cond ((null? (cdr args))
               (c9expr (car args) #f))
              (else
               (c9expr (car args) #f)
               (c9display ", ")
               (loop (cdr args))))))
  (c9display ")")
  (if r (c9print ";")))

(define (c9symbol x r)
  (cond ((memq x c9*primitive*)
         (c9display (cdr (memq x c9*primitive*))))
        (else
         (c9display x))))

(define (c9number x r)
  (if r (c9display "return "))
  (c9display c9make-number "(" x ")")
  (if r (c9display ";")))
