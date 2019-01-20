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

(define c9subr_0 "CSCM_SUBR_0")
(define c9subr_1 "CSCM_SUBR_1")
(define c9subr_2 "CSCM_SUBR_2")
(define c9subr_3 "CSCM_SUBR_3")
(define c9subr_4 "CSCM_SUBR_4")
(define c9subr_5 "CSCM_SUBR_5")
;; 追加の関数定義
(define c9subr_6 "CSCM_SUBR_6")
(define c9subr_7 "CSCM_SUBR_7")
(define c9subr_8 "CSCM_SUBR_8")
(define c9subr_9 "CSCM_SUBR_9")
(define c9subr_10 "CSCM_SUBR_10") 

(define (c9print . a)
  (let loop ((a a))
    (cond ((null? a)
           (newline *c-port*))
          (else
           (display (car a) *c-port*)
           (loop (cdr a))))))

(define (c9display . a)
  (let loop ((a a))
    (cond ((null? a))
          (else
           (display (car a) *c-port*)
           (loop (cdr a))))))

(define (c9write a)
  (write a *c-port*))

(define (return? r)
  (and (boolean? r)
       r))

;; (define var (lambda params body))
;; (define var expr)
(define (c9generate x)
  (if (cscm:pair? x)
      (case (car x)
        ((define)
         (let ((first (car x))
               (name (cadr x))
               (form (caddr x)))
           (if (and (cscm:pair? form)
                    (eq? (car form) 'lambda))
               (c9def-func first
                           name
                           form)
               (error "CSCM:ERROR, c9generate, not a function definition" x))))
        (else
         (error "CSCM:ERROR, c9generate, not a definition" x)))
      (error "CSCM:ERROR, c9generate, not a definition" x)))

(define (c9def-func first name lambda-expr)
  (c9display c9cscm " " (cscm:var-name name))
  (c9expr lambda-expr #t)) ;; CSCM name

(define (dec-func) ;; 宣言
  (let loop ((cscm *cscm*))
    (if (null? cscm)
        (newline *c-port*)
        (let ((sexp (car cscm)))
          (let ((name (cadr sexp))
                (lambda-expr (caddr sexp)))
            (let ((params (cadr lambda-expr)))
              (c9display c9cscm " " (cscm:var-name name) "(")
              (if (null? params)
                  (c9display c9void)
                  (let loop ((params params))
                    (cond ((null? (cdr params))
                           (c9display c9cscm " " (cscm:var-name (car params))))
                          (else
                           (c9display c9cscm " " (cscm:var-name (car params)) ", ")
                           (loop (cdr params))))))
              (c9print ");")))
          (loop (cdr cscm))))))

(define (init-func)
  (let loop ((cscm *cscm*))
    (if (null? cscm)
        (newline *c-port*)
        (let ((expr (car cscm)))
          (let ((name (cscm:var-name (cadr expr)))
                (lambda-expr (caddr expr)))
            (let ((params (cadr lambda-expr)))
              (let ((n (if (or (cscm:pair? params)
                               (null? params))
                           (length params)
                           1)))
                (case n
                  ((0)
                   (c9display c9subr_0 "(")
                   (c9write (symbol->string name))
                   (c9print ", " name ");"))
                  ((1)
                   (c9display c9subr_1 "(")
                   (c9write (symbol->string name))
                   (c9print ", " name ");"))
                  ((2)
                   (c9display c9subr_2 "(")
                   (c9write (symbol->string name))
                   (c9print ", " name ");"))
                  ((3)
                   (c9display c9subr_3 "(")
                   (c9write (symbol->string name))
                   (c9print ", " name ");"))
                  ((4)
                   (c9display c9subr_4 "(")
                   (c9write (symbol->string name))
                   (c9print ", " name ");"))
                  ((5)
                   (c9display c9subr_5 "(")
                   (c9write (symbol->string name))
                   (c9print ", " name ");"))
                  ;; 追加の関数定義
                  ((6)
                   (c9display c9subr_6 "(")
                   (c9write (symbol->string name))
                   (c9print ", " name ");"))
                  ((7)
                   (c9display c9subr_7 "(")
                   (c9write (symbol->string name))
                   (c9print ", " name ");"))
                  ((8)
                   (c9display c9subr_8 "(")
                   (c9write (symbol->string name))
                   (c9print ", " name ");"))
                  ((9)
                   (c9display c9subr_9 "(")
                   (c9write (symbol->string name))
                   (c9print ", " name ");"))
                  ((10)
                   (c9display c9subr_10 "(")
                   (c9write (symbol->string name))
                   (c9print ", " name ");"))
                  (else
                   (error "CSCM:ERROR, init-func" expr))))))
          (loop (cdr cscm))))))

(define c9*primitive* (list (cons 'eqv? "CSCM_EQV_P") (cons 'eq? "CSCM_EQ_P")
                            (cons 'number? "CSCM_NUMBER_P")
                            (cons '= "CSCM_EQUAL") (cons '< "CSCM_LESS") (cons '> "CSCM_MORE") (cons '<= "CSCM_LESSE") (cons '>= "CSCM_MOREE")
                            (cons '+ "CSCM_PLUS") (cons '* "CSCM_TIMES") (cons '- "CSCM_MINUS") (cons '/ "CSCM_DIFF")
                            (cons 'quotient "CSCM_QUOTIENT") (cons 'modulo "CSCM_MODULO") (cons 'mod "CSCM_MODULO")
                            ;;(cons 'exp "CSCM_EXP")
                            (cons 'log "CSCM_LOG")
                            (cons 'sin "CSCM_SIN") (cons 'cos "CSCM_CONS") (cons 'tan "CSCM_TAN")
                            (cons 'asin "CSCM_ASIN") (cons 'acos "CSCM_ACOS")(cons 'atan "CSCM_ATAN")

                            (cons 'sqrt "CSCM_SQRT") (cons 'expt "CSCM_EXPT")
                            
                            (cons 'number->string "CSCM_NUMBER2STRING") (cons 'string->number "CSCM_STRING2NUMBER")
                            
                            (cons 'pair? "CSCM_PAIR_P") (cons 'cons "CSCM_CONS") (cons 'car "CSCM_CAR") (cons 'cdr "CSCM_CDR") (cons 'set-car! "CSCM_SETCAR_B") (cons 'set-cdr! "CSCM_SETCDR_B")
                            (cons 'caar "CSCM_CAAR") (cons 'cadr "CSCM_CADR") (cons 'cddr "CSCM_CDDR")
                            
                            (cons 'symbol? "CSCM_SYMBOL_P") (cons 'symbol->string "CSCM_SYMBOL2STRING") (cons 'string->symbol "CSCM_STRING2SYMBOL")
                            (cons 'string? "CSCM_STRING_P") (cons 'string-ref "CSCM_STRING_REF") (cons 'string-set! "CSCM_STRING_SETB")
                            (cons 'apply "CSCM_APPLY")
                            ;;; cscm apply
                            (cons 'cscm_apply0 "CSCM_APPLY0") (cons 'cscm_apply1 "CSCM_APPLY1") (cons 'cscm_apply2 "CSCM_APPLY2") (cons 'cscm_apply3 "CSCM_APPLY3")
                            (cons 'cscm_apply4 "CSCM_APPLY4") (cons 'cscm_apply5 "CSCM_APPLY5") (cons 'cscm_apply6 "CSCM_APPLY6") (cons 'cscm_apply7 "CSCM_APPLY7")
                            (cons 'cscm_apply8 "CSCM_APPLY8") (cons 'cscm_apply9 "CSCM_APPLY9") (cons 'cscm_apply10 "CSCM_APPLY10")
                            ;;; library
                            (cons 'equal? "CSCM_EQUAL_P")
                            (cons 'zero? "CSCM_ZERO_P")
                            (cons 'not "CSCM_NOT")
                            (cons 'caaar "CSCM_CAAAR") (cons 'caadr "CSCM_CAADR") (cons 'cadar "CSCM_CADAR") (cons 'caddr "CSCM_CADDR")
                            (cons 'cdaar "CSCM_CDAAR") (cons 'cdadr "CSCM_CDADR") (cons 'cddar "CSCM_CDDAR") (cons 'cdddr "CSCM_CDDDR")
                            (cons 'null? "CSCM_NULL_P")
                            (cons 'length "CSCM_LENGTH")
                            ;;(cons 'append "CSCM_APPEND")
                            (cons 'reverse "CSCM_REVERSE")
                            (cons 'memq "CSCM_MEMQ") (cons 'memv "CSCM_MEMV") (cons 'member "CSCM_MEMBER")
                            (cons 'assq "CSCM_ASSQ") (cons 'assv "CSCM_ASSV") (cons 'assoc "CSCM_ASSOC")
                            (cons 'display "CSCM_DISPLAY") (cons 'newline "CSCM_NEWLINE")
                            (cons 'load "CSCM_LOAD")
                            ))

(define c9*cscm* (list (cons 'cscm_gvref "CSCM_GVREF")
                       
                       ))

(define c9*special* '(define set! lambda if quote and or let let* letrec begin delay))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c9primop? fn)
  (memq fn c9*primitive*))

(define (c9value? n)
  (if (pair? n)
      (memq (car n) c9*special*)
      #t))

(define (c9expr form r) ;; リターン情報はそのまま渡す
  (cond ((cscm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((cscm:symbol? fun)
                  (case fun
                    ((lambda) (c9lambda args r))
                    ((let) (c9let args r))
                    ((if) (c9if args r))
                    ((set!) (c9set! args r))
                    ((quote) (c9quote args r))
                    ((list append map) (c9optional form r)) ;; fun args
                    ;;((+ - * / min max) (c9optional2n form r)) ;; fun args
                    (else (c9symbol-fun fun args r)))))))
        (else
         (cond ((cscm:symbol? form)
                (c9symbol form r))
               ((number? form)
                (c9number form r))
               ((boolean? form)
                (c9boolean form r))
               ((string? form)
                (c9string form r))
               ((null? form)
                (c9null form r))))))

;; (CSCM p1, CSCM p2){
;;   return ;
;; }
(define (c9lambda args r)
  (c9display "(")
  (let ((params (car args))
        (body (cadr args)))
    (if (null? params)
        (c9display c9void) ;; (void)
        (let loop ((params params))
          (cond ((null? (cdr params))
                 (c9display c9cscm " " (cscm:var-name (car params))))
                (else
                 (c9display c9cscm " " (cscm:var-name (car params)) ", ")
                 (loop (cdr params))))))
    (c9print "){")
    (c9expr body #t) ;; リターン
    (c9print "}")))

(define (c9let args r)
  (let ((def (caar args)))
    (let ((var (car def)) ;; 変数
          (val (cadr def)) ;; 値
          (m (cadr args))) ;; 本体
      (cond ((and (pair? val) ;; anf~への束縛がif式のとき (let ((anf (if ...))) m)
                  (eq? (car val) 'if))
             (let ((name (cscm:var-name var)))
               (c9print c9cscm " " name ";") ;; CSCM var; 変数宣言
               (c9expr val name))) ;; リターン先は宣言した変数
            ((and (pair? val) ;;anf~への束縛がset!式のとき, ローカル変数への代入
                  (eq? (car val) 'set!))
             (c9expr val #f)) ;; 代入式はリターンしないし, 一時変数に結果を束縛する必要もない
            (else
             (c9display c9cscm " " (cscm:var-name var) " = ") ;; CSCM var = 
             (c9expr val #f) ;; リターンしない
             (c9print ";")))
      (c9expr m r)))) 


(define (c9if args r)
  (let ((m1 (car args))
        (m2 (cadr args))
        (m3 (caddr args)))
    (c9display "if (" c9nfalsep "(") ;; if(NFALSEP(m1)){...
    (c9expr m1 #f) ;; test式はリターンしない
    (c9print ")) {")
    (c9expr m2 r)
    (c9print "} else {")
    (c9expr m3 r)
    (c9print "}")))

;; return (f(...));
;; var = f(...);
(define (c9symbol-fun fun args r)
  (cond ((return? r)
         (c9display "return ("))
        ((symbol? r)
         (c9display r " = ")))
  (begin 
    (cond ((assq fun c9*primitive*)
           (c9display (cdr (assq fun c9*primitive*))))
          (else
           (c9display (cscm:var-name fun))))
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
    (cond ((return? r)
           (c9print ");"))
          ((symbol? r)
           (c9print ";")))))

(define (c9symbol-fun fun args r)
  (cond ((return? r)
         (c9display "return ("))
        ((symbol? r)
         (c9display r " = ")))
  (cond ((assq fun c9*primitive*)
         (c9display (cdr (assq fun c9*primitive*)))         
         (if (null? args)
             (c9display "(")
             (begin (c9display "(" (c9expr (car args) #f))
                    (c9args (cdr args)))))
        ((c9c? fun)
         (c9display (cscm:var-name fun))
         (if (null? args)
             (c9display "(")
             (begin (c9display "(" (c9expr (car args) #f))
                    (c9args (cdr args)))))
        ((cscm:var? fun)
         (let ((n (length args)))
           (case n
             ((0) (c9display "CSCM_APPLY0(" (var-name fun)))
             ((1) (c9display "CSCM_APPLY1(" (var-name fun))
              (c9args args))
             ((2) (c9display "CSCM_APPLY2(" (var-name fun))
              (c9args args))
             ((3) (c9display "CSCM_APPLY3(" (var-name fun))
              (c9args args))
             ((4) (c9display "CSCM_APPLY4(" (var-name fun))
              (c9args args))
             ((5) (c9display "CSCM_APPLY5(" (var-name fun))
              (c9args args))
             ((6) (c9display "CSCM_APPLY6(" (var-name fun))
              (c9args args))
             ((7) (c9display "CSCM_APPLY7(" (var-name fun))
              (c9args args))
             ((8) (c9display "CSCM_APPLY8(" (var-name fun))
              (c9args args))
             ((9) (c9display "CSCM_APPLY9(" (var-name fun))
              (c9args args))
             ((10) (c9display "CSCM_APPLY10(" (var-name fun))
              (c9args args))
             (else
              (error "CSCM:ERROR, c9symbol-fun, too many argument" n)))))
        (else
         (let ((n (length args))
               (fun-str (symbol->string fun)))
           (case n
             ((0) (c9display "CSCM_APPLY0(CSCM_GVREF(")
              (c9write fun-str) (c9display ")"))
             ((1) (c9display "CSCM_APPLY1(CSCM_GVREF(")
              (c9write fun-str) (c9display ")") (c9args args))
             ((2) (c9display "CSCM_APPLY2(CSCM_GVREF(")
              (c9write fun-str) (c9display ")") (c9args args))
             ((3) (c9display "CSCM_APPLY3(CSCM_GVREF(")
              (c9write fun-str) (c9display ")") (c9args args))
             ((4) (c9display "CSCM_APPLY4(CSCM_GVREF(")
              (c9write fun-str) (c9display ")") (c9args args))
             ((5) (c9display "CSCM_APPLY5(CSCM_GVREF(")
              (c9write fun-str) (c9display ")") (c9args args))
             ((6) (c9display "CSCM_APPLY6(CSCM_GVREF(")
              (c9write fun-str) (c9display ")") (c9args args))
             ((7) (c9display "CSCM_APPLY7(CSCM_GVREF(")
              (c9write fun-str) (c9display ")") (c9args args))
             ((8) (c9display "CSCM_APPLY8(CSCM_GVREF(")
              (c9write fun-str) (c9display ")") (c9args args))
             ((9) (c9display "CSCM_APPLY9(CSCM_GVREF(")
              (c9write fun-str) (c9display ")") (c9args args))
             ((10) (c9display "CSCM_APPLY10(CSCM_GVREF(")
              (c9write fun-str) (c9display ")") (c9args args))))))
  (c9display ")")
  (cond ((return? r)
           (c9print ");"))
          ((symbol? r)
           (c9print ";"))))
          
(define (c9c? name)
  (let ((name (symbol->string (cscm:var-name name))))
    (let ((len (string-length name)))
      (if (< len 3)
          #f
          (let ((head (substring name 0 2)))
            (if (equal? head "c_")
                #t
                #f))))))
  

(define (c9cscm_gvref fun args r)
  (let ((cscm (assq fun c9*cscm*)))
    (c9display (cdr cscm) "(") ;; CSCM_GVREF("変数")
    (c9write (symbol->string (car args)))
    (c9display ")"))
  (cond ((return? r)
         (c9print ");"))
        ((symbol? r)
         (c9print ";"))))

(define (c9symbol x r)
  (cond ((return? r)
         (c9display "return ("))
        ((symbol? r)
         (c9display r " = ")))
  (cond ((cscm:var? x)
         (c9display (var-name x)))
        ((c9anf? x)
         (c9display x))
        (else
         (c9display "CSCM_GVREF(")
         (c9write (symbol->string x))
         (c9display ")")))
  (cond ((return? r)
         (c9print ");"))
        ((symbol? r)
         (c9print ";"))))

(define (c9anf? x)
  (let ((x0 (symbol->string x)))
    (let ((x1 (substring x0 0 3)))
      (equal? x1 "anf"))))
          
(define (c9number x r)
  (cond ((return? r)
         (c9display "return ("))
        ((symbol? r)
         (c9display r " = ")))
  (c9display c9make-number "(" x ")")
  (cond ((return? r)
         (c9print ");"))
        ((symbol? r)
         (c9print ";"))))

(define (c9boolean x r)
  (cond ((return? r)
         (c9display "return ("))
        ((symbol? r)
         (c9display r " = ")))
  (let ((b (if x
               c9true
               c9false)))
    (c9display b))
  (cond ((return? r)
         (c9print ");"))
        ((symbol? r)
         (c9print ";"))))

(define (c9null x r)
  (cond ((return? r)
         (c9display "return ("))
        ((symbol? r)
         (c9display r " = ")))
  (c9display c9nil)
  (cond ((return? r)
         (c9print ");"))
        ((symbol? r)
         (c9print ";"))))

(define (c9string x r)
  (cond ((return? r)
         (c9display "return ("))
        ((symbol? r)
         (c9display r " = ")))
  (let ((l (string-length x)))
    (c9display c9make-string "(")
    (c9write x)
    (c9display ", " l ")"))
  (cond ((return? r)
         (c9print ");"))
        ((symbol? r)
         (c9print ";"))))

(define (c9set! x r)
  (let ((var (car x))
        (exp (cadr x)))
    (cond ((cscm:var? x)
           (c9display (var-name var) " = ")
           (c9expr exp #f)
           (c9print ";"))
          (else
           (c9display "CSCM_GVREF(")
           (c9write (symbol->string var))
           (c9display ") = ")
           (c9expr exp #f)
           (c9print ";")))
    (if (return? r)
        (begin (c9display "return (")
               (c9expr exp #f)
               (c9print ");")))))
    
(define (c9quote x r)
  (cond ((return? r)
         (c9display "return ("))
        ((symbol? r)
         (c9display r " = ")))
  (let ((x (car x)))
    (cond ((cscm:self-eval? x)
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
  (cond ((return? r)
         (c9print ");"))
        ((symbol? r)
         (c9print ";"))))

(define (c9gset! x r)
  (let ((var (car x))
        (exp (cadr x)))
    (c9display "CSCM_GVREF(")
    (c9write (symbol->string var))
    (c9display ") = ")
    (c9expr exp #f)
    (c9print ";")
    (if (return? r)
        (begin (c9display "return (")
               (c9expr exp #f)
               (c9print ");")))))

(define (c9optional form r)
  (if (return? r)
      (c9display "return (")
      (if (symbol? r)
          (c9display " = ")))
  (let ((fun (car form))
        (args (cdr form)))
    (let ((len (length args)))
      (c9display "CSCM_SYM_APPLY")
      (case len
        ((0) (c9display "0"))
        ((1) (c9display "1"))
        ((2) (c9display "2"))
        ((3) (c9display "3"))
        ((4) (c9display "4"))
        ((5) (c9display "5"))
        ((6) (c9display "6"))
        ((7) (c9display "7"))
        ((8) (c9display "8"))
        ((9) (c9display "9"))
        ((10) (c9display "10")))
      (case fun
        ((list) (c9list args r))
        ((append) (c9append args r))
        ((map) (c9map args r)))))
  (if (return? r)
      (c9print ");")
      (if (symbol? r)
          (c9display ";"))))

(define (c9list args r)
    (c9display "(CSCM_LIST")
    (c9args args r)
    (c9display ")"))

(define (c9append args r)
    (c9display "(CSCM_APPEND")
    (c9args args r)
    (c9display ")"))

(define (c9map args r)
  (c9display "(CSCM_MAP")
  (c9args args r)
  (c9display ")"))

(define (c9args args r)
  (if (not (null? args))
      (let loop ((args args))
        (cond ((null? args)
               #t)
              (else
               (c9display ", ")
               (c9expr (car args) #f)
               (loop (cdr args)))))))

