;;; 各変数の情報を集める
;;; 変数名, 関数か, 代入されるか, 

;;; for Gauche
(use util.match)

(define c.scm:*top-level-continuation* #f)
(define c.scm:*primitive* '(+ - * / = < > car cdr cons pair? list set-car! set-cdr! null? display not remainder memq member symbol? eq? cadr caddr append error map apply assoc))
(define c.scm:*special* '(define set! lambda if quote cond case and or let let* letrec quasiquote unquote begin do delay))

(define c.scm:*number-of-define* 0)
(define c.scm:*internal-define* 0)
(define c.scm:*internal-lambda* 0)
(define c.scm:*tail-call* 0)
(define c.scm:*set!* 0)
(define c.scm:*function-name* #f)

(define (cadaddr l) (car (cdr (car (cdr (cdr l))))))

(define c.scm:error
  (lambda msg
    (display "C.SCM ERROR")
    (let loop ((msg msg))
      (if (null? msg)
          (begin (newline)
                 (c.scm:*top-level-continuation* #t))
          (begin (display ": ")
                 (display (car msg))
                 (loop (cdr msg)))))))

(define (c.scm:separator input)
  (call/cc (lambda (cont)
             (set! c.scm:*top-level-continuation* cont)
             (if (string? input)
                 (c.scm:file-separator input)
                 (c.scm:exp-separator input)))))

(define (c.scm:file-separator input)
  (set! c.scm:*number-of-define* 0)
  (set! c.scm:*internal-define* 0)
  (set! c.scm:*internal-lambda* 0)
  (set! c.scm:*tail-call* 0)
  (set! c.scm:*set!* 0)
  (let ((iport (open-input-file input)))
    (let loop ((exp (read iport))
               (cscm-list '())
               (scheme-list '()))
      (cond ((eof-object? exp)
             (close-input-port iport)
             (print "C.scm: " (length cscm-list) " of " c.scm:*number-of-define*)
             (c.scm:output-file (reverse cscm-list) (string-append "C.scm-" input))
             (print "Scheme: " (length scheme-list) " of " c.scm:*number-of-define*)
             (c.scm:output-file (reverse scheme-list) (string-append "Scheme-" input))
             (print "set!: " c.scm:*set!*)
             (print "internal lambda: " c.scm:*internal-lambda*)
             (print "internal define: " c.scm:*internal-define*)
             (print "tail call: " c.scm:*tail-call*)
             (string-append "C.scm-" input))
            (else
             (if (c.scm:exp-separator exp)
                 (loop (read iport) (cons exp cscm-list) scheme-list)
                 (loop (read iport) cscm-list (cons exp scheme-list))))))))

(define (c.scm:output-file olist output-file)
  (let loop ((oport (open-output-file output-file))
             (olist olist))
    (cond ((null? olist)
           (newline oport)
           (close-output-port oport))
          (else
           (write (car olist) oport)
           (newline oport)
           (loop oport (cdr olist))))))

(define (c.scm:exp-separator define-exp)
  (match define-exp	 
         (`(define ,(x . params) . ,exp)
          (begin (set! c.scm:*function-name* x)
                 (c.scm:exp-check exp)
                 #;(print "(define ,(x . params) ,exp): " exp)))
         (`(define ,x (lambda ,params . ,exp))
          (begin (set! c.scm:*function-name* x)
                 (c.scm:exp-check exp)
                 #;(print "(define ,x (lambda ,params ,exp)): " exp)))
         (`(define ,x ,exp)
          (begin (set! c.scm:*function-name* x)
                 (c.scm:exp-check exp)
                 #;(print "(define ,x ,exp): " exp)))
         (else
          (c.scm:error "define expression required, but got " define-exp))))

(define (c.scm:exp-check exp-list)
  (set! c.scm:*number-of-define* (+ c.scm:*number-of-define* 1))
  (let loop ((exp-list exp-list))
    (cond ((null? exp-list)
           #t)
          ((not (pair? exp-list))
           #t)
          ((null? (cdr exp-list))
           (and (c.scm:check (car exp-list))
                #;(c.scm:no-tail-call? (car exp-list))
                #t))
          (else
           (and (c.scm:check (car exp-list))
                (loop (cdr exp-list)))))))

(define (c.scm:no-tail-call? exp)
  (match exp
         (`(if ,m1 ,m2 ,m3)
          (and (c.scm:no-tail-call? m2)
               (c.scm:no-tail-call? m3)))
         (`(if ,m1 ,m2)
          (c.scm:no-tail-call? m2))
         (`(cond (,test . ,then) . ,clause)
          (and (c.scm:no-tail-call? (c.scm:find-tail then))
               (let loop ((clause clause))
                 (if (null? clause)
                     #t
                     (and (c.scm:no-tail-call? (c.scm:find-tail (cdar clause)))
                          (loop (cdr clause)))))))
         (`(case ,key (,test ,then) . ,clause)	  
          (and (c.scm:no-tail-call? (c.scm:find-tail then))
               (let loop ((clause clause))
                 (if (null? clause)
                     #t
                     (and (c.scm:no-tail-call? (c.scm:find-tail (cdar clause)))
                          (loop (cdr clause)))))))
         (`(and . ,m)
          (c.scm:no-tail-call? (c.scm:find-tail m)))
         (`(or . ,m)
          (c.scm:no-tail-call? (c.scm:find-tail m)))
         (`(when ,test ,m1 . ,m2)
          (if (null? m2)
              (c.scm:no-tail-call? m1)
              (c.scm:no-tail-call? (c.scm:find-tail m2))))
         (`(unless ,test ,m1 . ,m2)
          (if (null? m2)
              (c.scm:no-tail-call? m1)
              (c.scm:no-tail-call? (c.scm:find-tail m2))))
         (`(let () . ,m)
          (c.scm:no-tail-call? (c.scm:find-tail m)))
         (`(let ((,x ,m1) . ,clause) . ,m2)
          (c.scm:no-tail-call? (c.scm:find-tail m2)))
         (`(let* () . ,m)
          (c.scm:no-tail-call? (c.scm:find-tail m)))
         (`(let* ((,x ,m1) . ,clause) . ,m2)
          (c.scm:no-tail-call? (c.scm:find-tail m2)))
         (`(let ,f () . ,m)
          (c.scm:no-tail-call? (c.scm:find-tail m)))
         (`(let ,f ((,x ,m1) . ,clause) . ,m2)
          (c.scm:no-tail-call? (c.scm:find-tail m2)))
         (`(letrec () . ,m)
          (c.scm:no-tail-call? (c.scm:find-tail m)))
         (`(letrec ((,x ,m1) . ,clause) . ,m2)
          (and (c.scm:no-tail-call? (c.scm:find-tail m2))
               (c.scm:no-tail-call? m1)
               (let loop ((clause clause))
                 (if (null? clause)
                     #t
                     (and (c.scm:no-tail-call? (cadar clause))
                          (loop (cdr clause)))))))
         (`(letrec* () . ,m)
          (c.scm:no-tail-call? (c.scm:find-tail m)))
         (`(letrec* ,((x m1) . clause) . ,m2)
          (and (c.scm:no-tail-call? (c.scm:find-tail m2))
               (c.scm:no-tail-call? m1)
               (let loop ((clause clause))
                 (if (null? clause)
                     #t
                     (and (c.scm:no-tail-call? (cadar clause))
                          (loop (cdr clause)))))))
         (`(begin . ,m)
          (c.scm:no-tail-call? (c.scm:find-tail m)))
         (`(lambda () . ,m)
          (c.scm:no-tail-call? (c.scm:find-tail m)))
         (`(lambda (,params) . ,m)
          (c.scm:no-tail-call? (c.scm:find-tail m)))
         (`(lambda ,param . ,m)
          (c.scm:no-tail-call? (c.scm:find-tail m)))
         (`(do () ,(test . m) . ,body)
          (and (c.scm:no-tail-call? (c.scm:find-tail m))
               (c.scm:no-tail-call? (c.scm:find-tail body))))
         (`(do (,bind . ,binds) (,test . ,m) . ,body)
          (and (c.scm:no-tail-call? (c.scm:find-tail m))
               (c.scm:no-tail-call? (c.scm:find-tail body))))
         (else
          (not (c.scm:call? exp)))))

(define (c.scm:find-tail exp)
  (let loop ((m exp))
    (cond ((null? m)
           m)
          ((null? (cdr m))
           (car m))
          (else
           (loop (cdr m))))))

(define (c.scm:call? exp)
  (if (and (pair? exp)
           (not (memq (car exp) c.scm:*primitive*))
           (not (memq (car exp) c.scm:*special*)))
      (begin (print "C.SCM: tail-call: " exp " in: " c.scm:*function-name*)
             (set! c.scm:*tail-call* (+ c.scm:*tail-call* 1)) ;; tailcall count
             #t)
      #f))

(define (c.scm:check exp)
  ;;(print "c.scm:debug: c.scm:check " exp) ;; debug
  (and (c.scm:no-assignment? exp)
       #;(c.scm:no-lambda? exp)
       #;(c.scm:no-internal-define? exp)))

(define (c.scm:no-assignment? exp)
  (cond ((null? exp)
         #t)
        ((pair? exp)
         (cond ((eq? (car exp) 'set!)	 
                (print "C.SCM: set!: " exp " in: " c.scm:*function-name*) ;;debug
                (set! c.scm:*set!* (+ c.scm:*set!* 1))
                #f)
               ((pair? (car exp))
                (and (c.scm:no-assignment? (car exp))
                     (c.scm:no-assignment? (cdr exp))))
               (else
                (c.scm:no-assignment? (cdr exp)))))
        (else
         #t)))

(define (c.scm:no-lambda? exp)
  (cond ((null? exp)
         #t)
        ((pair? exp)
         (cond ((eq? (car exp) 'lambda)
                (print "C.SCM: lambda: " exp " in: " c.scm:*function-name*) ;;debug
                (set! c.scm:*internal-lambda* (+ c.scm:*internal-lambda* 1))
                #f)
               ((pair? (car exp))
                (and (c.scm:no-lambda? (car exp))
                     (c.scm:no-lambda? (cdr exp))))
               (else
                (c.scm:no-lambda? (cdr exp)))))
        (else
         #t)))

(define (c.scm:no-internal-define? exp)
  (cond ((null? exp)
         #t)
        ((pair? exp)
         (cond ((eq? (car exp) 'define)
                (print "C.SCM: internal define: " exp " in: " c.scm:*function-name*) ;;debug
                (set! c.scm:*internal-define* (+ c.scm:*internal-define* 1))
                #f)
               ((pair? (car exp))
                (and (c.scm:no-internal-define? (car exp))
                     (c.scm:no-internal-define? (cdr exp))))
               (else
                (c.scm:no-internal-define? (cdr exp)))))
        (else
         #t)))



;;;;;;;;;;;;;;;;;;;
;;; A-Normal-Form
;;; A-Normalization
;;;;;;;;;;;;;;;;;;;
(define *primitive* c.scm:*primitive*)
(define *special* '(define lambda quote let if set!))
(define *function-name* "cscm")
(define *newvar* 0)
(define (newvar . name)
  (set! *newvar* (+ *newvar* 1))
  (if (null? name)
      (string->symbol
       (string-append *function-name* (number->string *newvar*)))	 
      (string->symbol
       (string-append (car name) (number->string *newvar*)))))

(define (primop? fn)
  (memq fn *primitive*))

(define (value? n)
  (if (pair? n)
      (memq (car n) *special*)
      #t))

(define (normalize-sexp sexp)
  (set! *function-name* "cscm")
  (match sexp
         (`(define . ,_)
          (normalize-define sexp))
         (else
          (normalize-term sexp))))

(define (normalize-define def)
  (match def	 
         (`(define ,(x . params) ,m)	  
          (set! *function-name* (symbol->string x))
          `(define ,x ,(normalize-term `(lambda ,params ,m))))
         (`(define ,x ,m)	  
          (set! *function-name* (symbol->string x))
          `(define ,x ,(normalize-term m)))))

(define (normalize-term m)
  (normalize m (lambda (x) x)))

(define (normalize m k)
  (match m
         (`(lambda ,params ,body)
          (k `(lambda ,params ,(normalize-term body))))
         (`(let () ,m)
          (normalize m k))
         #;(`(let ((,x ,m1) . ,clause) ,m2)
         (normalize m1 (lambda (n1)
         `(let ((,x ,n1))
         ,(normalize `(let (,@clause) ,m2) k)))))
         (`(let* () ,m)
          (normalize m k))
         (`(let* ((,x ,m1) . ,clause) ,m2)
          (normalize m1 (lambda (n1)
                          `(let ((,x ,n1))
                             ,(normalize `(let (,@clause) ,m2) k)))))
         (`(if ,m1 ,m2 ,m3)
          (normalize-name m1 (lambda (t)
                               (k `(if ,t
                                       ,(normalize-term m2)
                                       ,(normalize-term m3))))))
         (`(set! ,x ,m)
          (normalize-name m (lambda (t)
                              `(let ((,(newvar "cscm:set!") (set! ,x ,t)))
                                 ,(k x)))))
         (`(,fn . ,m*)
          (if (primop? fn)
              (normalize-name* m* (lambda (t*)
                                    (k `(,fn . ,t*))))
              (normalize-name fn (lambda (t)
                                   (normalize-name* m* (lambda (t*)
                                                         (k `(,t . ,t*))))))))
         (v
          (k v))))

(define (normalize-name m k)
  (normalize m (lambda (n)
                 (if (value? n)
                     (k n)
                     (let ((t (newvar *function-name*)))
                       `(let ((,t ,n))
                          ,(k t)))))))

(define (normalize-name* m* k)
  (if (null? m*)
      (k '())
      (normalize-name (car m*) (lambda (t)
                                 (normalize-name* (cdr m*)
                                                  (lambda (t*)
                                                    (k `(,t . ,t*))))))))		      

;;;;;;;;;;;;;;;;;;;;;;;;
;;; A-Normal-Form to C
;;;;;;;;;;;;;;;;;;;;;;;;
(define (atc:print . a)
  (let loop ((a a))
    (cond ((null? a)
           (newline))
          (else
           (display (car a))
           (loop (cdr a))))))

(define (atc:display . a)
  (let loop ((a a))
    (cond ((null? a))
          (else
           (display (car a))
           (loop (cdr a))))))

(define (atc:null? x)
  (equal? ''() x))

(define atc:*primitive* c.scm:*primitive*)
(define atc:*special* '(let lambda if let if))

(define (cadaadr x)
  (car (cdr (car (car (cdr x))))))

(define (when test then)
  (if test
      then))

(define atc:cscm "CSCM")
(define atc:make-number "CSCM_MAKE_NUMBER")
(define atc:+ "CSCM_PLUS")
(define atc:- "CSCM_MINUS")
(define atc:* "CSCM_TIMES")
(define atc:= "CSCM_EQUAL")
(define atc:< "CSCM_LESS")
(define atc:> "CSCM_GREATER")
(define atc:car "CSCM_CAR")
(define atc:cdr "CSCM_CDR")
(define atc:cons "CSCM_CONS")
(define atc:pair? "CSCM_PAIR_P")

(define (atc anf)
  (cond ((atc:define? anf)
         (atc:define anf))
        (else
         (atc:compile anf))))

(define (atc:define? anf)
  (and (pair? anf)
       (eq? (car anf) 'define)))

(define (atc:define anf)
  (let ((var (cadr anf))
        (m (caddr anf)))
    (atc:emit-define var m)))

(define (atc:emit-define var anf)
  (atc:display "CSCM " var)
  (if (atc:lambda? anf)
      (atc:emit-function anf)
      (atc:compile anf)))

(define (atc:lambda? anf)
  (and (pair? anf)
       (eq? (car anf) 'lambda)))

(define (atc:emit-function anf)
  (atc:display "(")
  (let ((vars (cadr anf)))
    (if (null? vars)
        (atc:print "CSCM_VOID){")
        (let loop ((vars vars))
          (cond ((null? (cdr vars))
                 (atc:print "CSCM " (car vars) "){"))
                (else
                 (atc:display "CSCM " (car vars) ", ")
                 (loop (cdr vars)))))))
  (atc:compile (caddr anf))
  (atc:print "}"))

(define (atc:compile anf) ;;M
  ;;(print "atc:debug: " anf) ;;debug
  (cond ((atc:return? anf)
         (atc:return anf)) ;;V	
        ((atc:bind? anf)
         (atc:bind anf))  ;;(let ((x V)) M
        ((atc:branch? anf)
         (atc:branch anf)) ;;(if V M1 M2)
        ((atc:tail-call? anf)
         (atc:tail-call anf)) ;;(V V1 ... Vn)
        ((atc:call? anf)
         ;;(print "atc:debug: atc:compile atc:call") ;;debug
         (atc:call anf)) ;;(let ((x (V V1 ... Vn))) M)
        ((atc:tail-primop? anf)
         (atc:tail-primop anf)) ;;(O V1 ... Vn)
        ((atc:primop? anf)
         ;;(print "atc:debug: atc:compile: atc:primop") ;;debug
         (atc:primop anf)) ;;(let ((x (O V1 ... Vn))) M)
        (else
         (print "atc:error Unknown A-Normal-Form"))))

;; return
(define (atc:return? anf)
  (atc:values? anf))

(define (atc:values? anf)
  (or (atc:constant? anf)
      (atc:symbol? anf)
      (atc:lambda? anf)))

(define (atc:constant? anf)
  (or (number? anf)
      (boolean? anf)
      (atc:null? anf)))

(define (atc:symbol? anf)
  (and (symbol? anf)
       (not (memq anf atc:*primitive*))
       (not (memq anf atc:*special*))))

(define (atc:return anf)
  ;;(print "atc:debug atc:return") ;;debug
  (atc:emit-return anf))

(define (atc:emit-return val)
  (atc:display "return ")
  (cond ((atc:symbol? val)
         (atc:display val))
        ((atc:constant? val)
         (cond ((number? val)
                (atc:display atc:make-number "(" val ")")))))
  (atc:print ";"))

;;bind
(define (atc:bind? anf)
  (and (atc:let? anf)
       (atc:values? (cadaadr anf))))

(define (atc:let? anf)
  (and (pair? anf)
       (eq? (car anf) 'let)))

(define (atc:bind anf)
  (let ((var (caaadr anf))
        (val (cadaadr anf))
        (m (caddr anf)))
    (atc:emit-bind var val)
    (atc:compile m)))

(define (atc:emit-bind var val)
  (atc:print "CSCM " var " = " val ";"))

;; branch
(define (atc:branch? anf)
  (and (pair? anf)
       (eq? (car anf) 'if)))

(define (atc:branch anf)
  (let ((test (cadr anf))
        (th (caddr anf))
        (el (cadddr anf)))
    (atc:emit-branch test th el)))

(define (atc:emit-branch test th el)
  (atc:print "if (" test ") {")
  (atc:compile th)
  (atc:print "} else {")
  (atc:compile el)
  (atc:print "}"))

;; tail-call
(define (atc:tail-call? anf) ;;(V V1 ... Vn)
  (and (pair? anf)
       (not (atc:let? anf))
       (atc:symbol? (car anf))))

(define (atc:tail-call anf)
  (let ((f (car anf))
        (args (cdr anf)))
    (atc:emit-tail-call f args)))

(define (atc:emit-tail-call f args)
  (atc:display "return (")
  (atc:emit-call f args)
  (atc:print ");"))

(define (atc:emit-call f args)
  (atc:display f "(")
  (if (null? args)
      (atc:display ")")
      (let loop ((args args))
        (cond ((null? (cdr args))
               (atc:display (car args) ")"))
              (else
               (atc:display (car args) ", ")
               (loop (cdr args)))))))

;; call
(define (atc:call? anf)
  (and (atc:let? anf)
       (atc:tail-call? (cadaadr anf))))

(define (atc:call anf)
  (let ((var (caaadr anf))
        (call (cadaadr anf))
        (m (caddr anf)))
    (atc:display "CSCM " var " = ")
    (let ((f (car call))
          (args (cdr call)))
      (atc:display "(")
      (atc:emit-call f args))
    (atc:print ");")
    (atc:compile m)))

;; tail-primop
(define (atc:tail-primop? anf)
  (and (pair? anf)
       (memq (car anf) atc:*primitive*)))

(define (atc:tail-primop anf)
  (let ((primop (car anf))
        (args (cdr anf)))
    (atc:display "return (")    
    (atc:emit-primop primop args))
  (atc:print ");"))

(define (atc:emit-primop primop args)
  (case primop
    ((+) (atc:emit-primop-2 atc:+ args))
    ((-) (atc:emit-primop-2 atc:- args))
    ((*) (atc:emit-primop-2 atc:* args))
    ((=) (atc:emit-primop-2 atc:= args))
    ((<) (atc:emit-primop-2 atc:< args))
    ((>) (atc:emit-primop-2 atc:> args))
    ((car) (atc:emit-primop-1 atc:car args))
    ((cdr) (atc:emit-primop-1 atc:cdr args))
    ((cons) (atc:emit-primop-2 atc:cons args))
    ((pair?) (atc:emit-primop-1 atc:pair? args))))

(define (atc:emit-primop-2 primop args)
  (let ((n1 (car args))
        (n2 (cadr args)))
    (atc:display primop "(")
    (if (not (atc:symbol? n1))
        (atc:display atc:make-number "(" n1 "),")    
        (atc:display n1 ", "))
    (if (not (atc:symbol? n2))
        (atc:display atc:make-number "(" n2 ")")	
        (atc:display n2))
    (atc:display ")")))

(define (atc:emit-primop-1 primop args)
  (atc:display primop "(" (car args) ")"))

;; primop
(define (atc:primop? anf)
  (and (atc:let? anf)
       (atc:tail-primop? (cadaadr anf))))

(define (atc:primop anf)
  (let ((var (caaadr anf))
        (call (cadaadr anf))
        (m (caddr anf)))
                                        ;(print "atc:debug: " call) ;;debug
    (atc:display atc:cscm " " var " = (")
    (let ((primop (car call))
          (args (cdr call)))
      (atc:emit-primop primop args))
    (atc:print ");")
    (atc:compile m)))


;; for sicp
;; 入力:ファイル名.scm
;; 出力:2ファイル名.scm
;; readで読み込んだ式のうち, define以外で始まるものをコメントアウトする
(define (remove-no-define input)
  (let ((iport (open-input-file input))
        (oport (open-output-file (string-append "2" input))))
    (let loop ((exp (read iport)))
      (cond ((eof-object? exp)
             (close-input-port iport)
             (close-output-port oport))
            ((pair? exp)
             (if (eq? (car exp) 'define)
                 (write exp oport)
                 (begin (display "#;" oport)
                        (write exp oport)))
             (newline oport)
             (loop (read iport)))
            (else
             (begin (display "#;" oport)
                    (write exp oport))
             (loop (read iport)))))))

