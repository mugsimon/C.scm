;; A-Normal-FormのSchemeを受け取り、Cコードの文字列のリストを返す

(define c9cscm "CSCM")
(define c9void "CSCM_VOID")
(define c9make-number "CSCM_MAKE_NUMBER")
(define c9make-symbol "CSCM_MAKE_SYMBOL")
(define c9make-string "CSCM_MAKE_STRING")
(define c9nfalsep "CSCM_NFALSEP")
(define c9true "CSCM_TRUE")
(define c9false "CSCM_FALSE")
(define c9nil "CSCM_NIL")

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

(define (c9write a)
  (write a c9*output-port*))



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
  (c9expr form #t))

;;(define c9*primitive* '(+ - * / = < > car cdr cons pair? list set-car! set-cdr! null? display not remainder memq member symbol? eq? cadr caddr append error map apply assoc))
(define c9*primitive* (list (cons 'eqv? "CSCM_EQV_P") (cons 'eq? "CSCM_EQ_P")
                            (cons 'number? "CSCM_NUMBER_P")
                            (cons '= "CSCM_EQUAL") (cons '< "CSCM_LESS") (cons '> "CSCM_MORE") (cons '<= "CSCM_LESSE") (cons '>= "CSCM_MOREE")
                            (cons '+ "CSCM_PLUS") (cons '* "CSCM_TIMES") (cons '- "CSCM_MINUS") (cons '/ "CSCM_DIFF")
                            (cons 'pair? "CSCM_PAIR_P") (cons 'cons "CSCM_CONS") (cons 'car "CSCM_CAR") (cons 'cdr "CSCM_CDR") (cons 'set-car! "CSCM_SETCAR_B") (cons 'set-cdr! "CSCM_SETCDR_B")
                            (cons 'caar "CSCM_CAAR") (cons 'cadr "CSCM_CADR") (cons 'cddr "CSCM_CDDR")
                            (cons 'symbol? "CSCM_SYMBOL_P")
                            (cons 'string? "CSCM_STRING_P")
                            (cons 'apply "CSCM_APPLY")
                            ;;; cscm apply
                            (cons 'cscm_apply0 "CSCM_APPLY0") (cons 'cscm_apply1 "CSCM_APPLY1") (cons 'cscm_apply2 "CSCM_APPLY2") (cons 'cscm_apply3 "CSCM_APPLY3")
                            ;;; library
                            (cons 'equal? "CSCM_EQUAL_P")
                            (cons 'not "CSCM_NOT")
                            (cons 'caaar "CSCM_CAAAR") (cons 'caadr "CSCM_CAADR") (cons 'cadar "CSCM_CADAR") (cons 'caddr "CSCM_CADDR")
                            (cons 'cdaar "CSCM_CDAAR") (cons 'cdadr "CSCM_CDADR") (cons 'cddar "CSCM_CDDAR") (cons 'cdddr "CSCM_CDDDR")
                            (cons 'null? "CSCM_NULL_P")
                            (cons 'length "CSCM_LENGTH") (cons 'append "CSCM_APPEND") (cons 'reverse "CSCM_REVERSE")
                            (cons 'memq "CSCM_MEMQ") (cons 'memv "CSCM_MEMV") (cons 'member "CSCM_MEMBER")
                            (cons 'assq "CSCM_ASSQ") (cons 'assv "CSCM_ASSV") (cons 'assoc "CSCM_ASSOC")))

(define c9*cscm* (list (cons 'cscm_vref "CSCM_VREF")
                       ))

(define c9*special* '(define set! lambda if quote and or let let* letrec begin delay))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c9primop? fn)
  (memq fn c9*primitive*))

(define (c9value? n)
  (if (pair? n)
      (memq (car n) c9*special*)
      #t))

(define (c9expr form r)
  ;(print "c.scm:debug, c9expr, form -> " form) ;; debug
  (cond ((pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           ;(print "c.scm:debug, c9expr, fun -> " fun) ;; debug
           (cond ((symbol? fun)
                  (case fun
                    ((lambda) (c9lambda args r))
                    ((let) (c9let args r))
                    ((if) (c9if args r))
                    ((quote) (c9quote args r))
                    (else (c9symbol-fun fun args r)))))))
        (else
         (cond ((symbol? form)
                (c9symbol form r))
               ((number? form)
                (c9number form r))
               ((boolean? form)
                (c9boolean form r))
               ((string? form)
                (c9string form r))
               ((null? form)
                (c9null form r))))))

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
  (let ((def (caar args)))
    (let ((var (car def))
          (val (cadr def))
          (m (cadr args)))
      (c9display c9cscm " " var " = ")
      (c9expr val #f)
      (c9print ";")
      (c9expr m #t))))

(define (c9if args r)
  (let ((m1 (car args))
        (m2 (cadr args))
        (m3 (caddr args)))
    (c9display "if (" c9nfalsep "(")
    (c9expr m1 #f)
    (c9print ")) {")
    (c9expr m2 #t)
    (c9print "} else {")
    (c9expr m3 #t)
    (c9print "}")))

(define (c9symbol-fun fun args r)
  (if r (c9display "return ("))
  (if (assq fun c9*cscm*)
      (c9cscm_vref fun args r) ;; cscm_vref
      (begin 
        (cond ((assq fun c9*primitive*)
               (c9display (cdr (assq fun c9*primitive*))))
              (else
               (c9display fun)))
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
        (if r (c9print ");")))))

(define (c9cscm_vref fun args r)
  (let ((cscm (assq fun c9*cscm*)))
    (c9display (cdr cscm) "(") ;; CSCM_VREF("変数")
    (c9write (symbol->string (car args)))
    (c9display ")"))
  (if r (c9print ");")))

(define (c9symbol x r)
  (cond ((assq x c9*primitive*)
         (c9display (cdr (assq x c9*primitive*))))
        (else
         (if r (c9display "return ("))
         (c9display x)
         (if r (c9print ");")))))

(define (c9number x r)
  (if r (c9display "return ("))
  (c9display c9make-number "(" x ")")
  (if r (c9print ");")))

(define (c9boolean x r)
  (if r (c9display "return ("))
  (let ((b (if x
               c9true
               c9false)))
    (c9display b))
  (if r (c9print ");")))

(define (c9null x r)
  (if r (c9display "return ("))
  (c9display c9nil)
  (if r (c9print ");")))

(define (c9string x r)
  (if r (c9display "return ("))
  (let ((l (string-length x)))
    (c9display c9make-string "(")
    (c9write x)
    (c9display ", " l ")"))
  (if r (c9print ");")))

(define (c9quote x r)
  (if r (c9display "return ("))
  (let ((x (car x)))
  (cond ((c.scm:self-eval? x)
         (c9expr x #f))
        ((symbol? x)
         (let ((s-symbol (symbol->string x)))
           (c9display c9make-symbol "(")
           (c9write s-symbol)
           (c9display ")")))
        ((string? x)
         (c9string x #f))
        (else
         (c9expr x #f))))
  (if r (c9print ");")))
