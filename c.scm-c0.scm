;;; schemeの式を受け取り、変形を行う
;;; cond, case -> if
;;; ローカル関数 -> letrec
;;; do -> 名前付きlet
;;; 関数位置の生lambda -> let
;;; quasiquote, unquote, unquote-splicing -> cons, append, quote

(define c.scm:*c0cont* #f)

;; (define var (lambda params body))
;; (define var expr)
;; (define (var params) body)
;; (begin ...)
;; (fun ...)
(define (c.scm:c0transform x)
  (call/cc
   (lambda (cont)
     (set! c.scm:*c0cont* cont)
     (if (pair? x)
         (case (car x)
           ((define)
            (cond ((or (null? (cdr x)) ;; (define)
                       (null? (cddr x))) ;; (define var)
                   (error "C.SCM:ERROR, c.scm:compile-sexp, syntax-error:" x)) ;; 不正なdefine式
                  ((symbol? (cadr x)) ;; (define var ...)
                   (if (not (null? (cdddr x))) ;; (define var exp ...)
                       (error "C.SCM:ERROR, c.scm:compile-sexp, syntax-error:" x) ;; 不正なdefine式
                       (let ((form (caddr x)))
                         (if (and (pair? form)
                                  (eq? (car form) 'lambda))
                             (c.scm:c0compile-function (car x) ;; define
                                                       (cons (cadr x) (cadr form)) ;; (var params), (var .param)
                                                       (cddr form)) ;; (...) lambda式の本体
                             `(,(car x) ,(cadr x) ,(c.scm:c0compile-expr form)))))) ;; (define var form) トップレベル変数宣言
                  ((pair? (cadr x))
                   (c.scm:c0compile-function (car x) (cadr x) (cddr x))) ;; (define (var ...) ...) 省略記法のdefine式
                  (else
                   (error "C.SCM:ERROR, c.scm:compile-sexp, syntax-error:" x))))
           ((begin)
            `(begin ,@(map c.scm:c0transform (cdr x))))
           (else
            (c.scm:c0compile-expr x))) ;; (expr ...)
         (c.scm:c0compile-expr x))))) ;; atom

;; マクロ
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

;; first->define
;; args->(var params)|(var . param)
;; body->expr
(define (c.scm:c0compile-function first args body)
  (if (not (and (pair? args)
		        (symbol? (car args))))
      (scerror "~s is a bad arg to DEFINE." args))
  (let ((x (c0lam (cons (cdr args) body))))
    `(,first ,(car args) (lambda ,@x))))

;; form->expr
(define (c.scm:c0compile-expr form)
  (let ((x (c0expr form)))
    x))

;; 自己評価的データなら#tを返す
;; x->expr
(define (c.scm:self-eval? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))

(define *newvar-name* "c.scm:")
(define *newvar* 0)
(define (newvar . name)
  (set! *newvar* (+ *newvar* 1))
  (if (null? name)
      (string->symbol
       (string-append *newvar-name* (number->string *newvar*)))	 
      (string->symbol
       (string-append (car name) (number->string *newvar*)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intrinsic constants
(define (c0constant x)
  #;(list c2constant x)
  (if (c.scm:self-eval? x)
      x
      `(quote ,x)))

(define c0null #;(list c2constant '())
  `'())
(define c0false #;(list c2constant #f)
  #f)
(define c0true #;(list c2constant #t)
  #t)
(define c0if-else-default #;(list c2constant '())
  `'())
(define c0cond-else-default #;(list c2constant '())
  `'())
(define c0begin-empty-default #;(list c2constant '())
  `'())

;;; SCCOND  Conditionals.
(define (c0if args) 
  (if (or (end? args) ;; testがない
          (end? (cdr args)) ;; thenがない
          (and (not (end? (cddr args))) ;; else節があり
               (not (end? (cdddr args))))) ;; 更に節がある
      (scbad-args 'if args)) ;; 引数エラー
  (list 'if #;c2if
        (c0fmla (car args))
        (c0expr (cadr args))
        (if (null? (cddr args))
            c0if-else-default
            (c0expr (caddr args)))))

(define (c0fmla fmla)
  (if (pair? fmla)
      (case (car fmla)
        ((and #;AND) (cond
                 ((end? (cdr fmla)) c0true)
                 ((end? (cddr fmla)) (c0fmla (cadr fmla)))
                 (else (cons 'and #;'FMLA-AND (c0map c0fmla (cdr fmla))))))
        ((or #;OR) (cond
                ((end? (cdr fmla)) c0false)
                ((end? (cddr fmla)) (c0fmla (cadr fmla)))
                (else (cons 'or #;'FMLA-OR (c0map c0fmla (cdr fmla))))))
        ((not #;NOT) (cond #;((c0lookup 'not) (c0expr fmla))
                     ((or (end? (cdr fmla)) (not (end? (cddr fmla))))
                      (scbad-args 'not (cdr fmla)))
                     (else (list 'not #;'FMLA-NOT (c0fmla (cadr fmla))))))
        (else (c0expr fmla)))
      (c0expr fmla)))

(define (c0and args)
  (cond ((end? args) c0true)
        ((end? (cdr args)) (c0expr (car args)))
        (else `(and ,@(c0args args)) ;; c.scm
              #;(list c2and (c0args args))))) ;; 引数を解析

(define (c0or args)
  (cond ((end? args) c0false)
        ((end? (cdr args)) (c0expr (car args)))
        (else `(or ,@(c0args args)) ;; c.scm
              #;(list c2or (c0args args))))) ;; 引数を解析

(define (c0cond args)
  (c0expr (c0expand-cond args)))

;; cond式を展開する. 基本的に入れ子のifとbeginにする
(define (c0expand-cond args)
  (if (null? args)
      #;(cadr c0cond-else-default)
      c0cond-else-default ;; (cadr (list c2constant '()))->null
      (let ((clause (car args)) ;; cond式の最初の節
            (rest (cdr args))) ;; 残りの節
        (cond ((end? clause)
               (scbad-args 'cond clause)) ;; 節がない
              ((eq? (car clause) 'else)
               `(begin ,@(cdr clause)))
              ((end? (cdr clause)) ;; 節のtest式しかないとき
               (if (end? rest) 
                   (car clause) ;; 最後の節のときはtest式が結果
                   `(or ,(car clause) ,(c0expand-cond rest)))) 
              ((and (eq? (cadr clause) '=>) ;; Gauche p.102参照, 省略記法
                    (not (end? (cddr clause)))
                    (end? (cdddr clause)))
               (let ((temp (newvar) #;(gensym))) ;; c.scm:c0
                 `(let ((,temp ,(car clause))) ;; tempにtest式の結果を保持
                    (if ,temp
                        (,(caddr clause) ,temp) ;; test式が真ならこれを引数にthen式を呼び出す
                        ,(c0expand-cond rest)))))
              (else `(if ,(car clause) 
                         (begin ,@(cdr clause)) ;; test式が真ならその節の残りの式をすべて評価
                         ,(c0expand-cond rest)))))))

(define (c0case args) ;; (case key ((data ...) exp ...) ...)
  (cond ((end? args) (scbad-args 'case args))
        ((end? (cdr args)) ;; keyしかない
         (c0begin (list (car args) (cadr c0cond-else-default)))) ;; keyを評価してnullを返す
        (else
         (let ((temp (newvar) #;(gensym))) ;; c.scm:c0
           (c0expr `(let ((,temp ,(car args)))
                      ,(c0expand-case temp (cdr args))))))))

;; case式をcaseを使わない形に展開する
(define (c0expand-case key clauses)
  (if (end? clauses) 
      (cadr c0cond-else-default) ;; else節なし
      (let ((clause (car clauses)) (rest (cdr clauses))) ;; 最初の節, 残りの節
        (cond ((end? clause) (scbad-args 'case clause))
              ((eq? (car clause) 'else)
               `(begin ,@(cdr clause))) ;; else節なら節の式をすべて評価
              ((null? (car clause)) ;; keyと比較するデータがないとき
               (c0expand-case key rest))
              (else `(if (or ,@(c0map (lambda (sym) `(eqv? ,key ',sym))
                                      (car clause))) ;; c0mapの評価結果はリストなので,@でorの引数に展開している
                         (begin ,@(cdr clause)) 
                         ,(c0expand-case key rest)))))))

;;; SCEVAL  The Expression Dispatcher.
(define (c0expr form)
  (cond ((symbol? form) 
         (c0vref form)) ;; 変数参照
        ((pair? form)
         (let ((fun (car form))
               (args (cdr form))) ;; (fun args)
           (cond ((symbol? fun)
                  (case fun
                    ((if) (c0if args)) 
                    ((cond) (c0cond args)) ;; cond式をif式に展開して再度c0exprで評価する
                    ((case) (c0case args)) ;; case式をif式に展開
                    ((and) (c0and args))
                    ((or) (c0or args))
                    ((begin) (c0begin args))
                    ((lambda) (c0lambda args)) ;; (lambda args)
                    ((delay) (c0delay args)) ;; (delay args)
                    ((let) (c0let args)) 
                    ((let*) (c0let* args))
                    ((letrec) (c0letrec args))
                    ((do) (c0do args)) ;; 名前付きletへ
                    ((set!) (c0set! args)) ;;
                    ((quote) (c0quote args))
                    ((quasiquote) (c0quasiquote args))
                    ((define) (c0define args)) ;; エラー
                    ((macro) (c0macro args)) ;; エラー
                    (else
                     #;(if (>= *re-compile-level* 2)
                     (set-fun-data fun)) ;;;;; By A243  Nov, 1992 ;;;;;
                     #;(if (macro-function fun)
                     (c0expr ((macro-function fun) form))
                     (c0symbol-fun fun args))
                     (c0symbol-fun fun args)))) ;; (f args)の呼び出し
                 ((and (pair? fun)
                       (eq? (car fun) 'lambda)) ;; ((lambda ...) args)
                  (c0lambda-fun (cdr fun) args)) ;; ((...) ...) ;; let式に書き換える
                 (else
                  `(,(c0expr fun) ,@(c0args args)) ;; c.scm
                  #;(list c2funcall (c0expr fun) (c0args args))))))
        (else
         (case form
           ((#f) c0false)
           ((#t) c0true)
           ((()) c0null)
           (else (c0constant form))))))

(define (c0symbol-fun name args)
  `(,name ,@(c0args args)) ;; c.scm:c0
  #;(let parse ((env *env*) (ccb #f))
    (cond ((null? env) `(,c2funcall (,c2gvref ,name) ,(c0args args)))
          ((eq? (car env) 'CB) (parse (cdr env) #t))
          ((eq? (var-name (car env)) name)
           (if ccb (set-var-closed (car env) #t))
           `(,c2funcall (,c2vref ,(car env) ,ccb) ,(c0args args)))
  (else (parse (cdr env) ccb)))))

(define (c0lambda-fun lambda-expr args)
  (define (make-defs vl as) ;; lambda式の引数のリストとlambda式に渡す引数
    (cond ((null? vl)
           (if (not (null? as))
               (scerror "Illegal number of arguments.~%        In ~s"
                        (cons (cons 'lambda lambda-expr)
                              args))
               '()))
          ((symbol? vl) ;; 任意引数の場合
           `((,vl (list ,@as)))) ;; ((vl (arg ...)))
          (else
           (if (null? as) 
               (scerror "Illegal number of arguments.~%        In ~s"
                        (cons (cons 'lambda lambda-expr)
                              args))
               (cons (list (car vl) (car as)) ;; 仮引数と実引数のリスト
                     (make-defs (cdr vl) (cdr as)))))))
  (if (end? lambda-expr)
      (scbad-args 'lambda lambda-expr)
      (c0let (cons (make-defs (car lambda-expr) args)
                   (cdr lambda-expr))))) ;; (...), lambda式の実行本体部分

(define (c0args forms)
  (if (end? forms)
      '()
      (cons (c0expr (car forms))
            (c0args (cdr forms)))))

(define (c0begin forms)
  (cond ((end? forms)
         c0begin-empty-default)
        ((end? (cdr forms))
         (c0expr (car forms)))
        (else
         `(begin ,@(c0map c0expr forms)) ;; c.scm:c0
         #;(list c2begin (c0map c0expr forms)))))

(define (c0body body defs)
  (if (end? body)
      (if (not (null? defs))
          (c0letrec (list (reverse defs))) ;; ローカルdefineしかない
          c0begin-empty-default) ;; 実行する本体がない
      (let ((form (car body))) 
        (cond ((and (pair? form) (eq? (car form) 'define)) ;; ローカルdefineのとき
               (if (or (end? (cdr form)) 
                       (end? (cddr form)) )
                   ;                       (not (end? (cdddr form))))
                   (scbad-args 'define (cdr form)))
               (c0body (cdr body) ;; ローカルdefineをdefsに追加し, 残りの本体を再帰的に解析
                       (cons (if (pair? (cadr form)) ;; 省略記法を使ったローカル関数定義の場合
                                 `(,(caadr form) ;; 省略記法を展開した形に変更
                                   (lambda ,(cdadr form) ,@(cddr form))) ;; (define (f ...) ...) -> (f (lambda (...) ...))
                                 (cdr form)) ;; (f (lambda (...) ...)) ;; 要するにletrecと同じカタチにしている
                             defs))) ;; defsにdefineを取り除いた残りの部分をcons
              ((null? defs) ;; ローカルdefineがないとき
               (c0begin body)) ;;
              (else ;; ローカルdefineがあるとき
               (c0letrec (cons (reverse defs) body)))))))

(define (c0lambda args)
  (cons 'lambda (c0lam args)) ;; c.scm:c0
  #;(dlet ((*env* (cons 'CB *env*)))
        (cons c2lambda (cons (cons 'lambda args) (c0lam args)))))

(define (c0delay args)
  (cons 'delay (cdr (c0lam (cons '() args)))) ;; c.scm:c0
  #;(dlet ((*env* (cons 'CB *env*)))        
        (cons c2delay (cddr (c0lam (cons '() args)))))) ;; lambda式の無引数版

(define (c0lam lambda-expr)
  (if (end? lambda-expr) (scbad-args 'lambda lambda-expr))
  (list (car lambda-expr) (c0body (cdr lambda-expr) '())) ;; c.scm:c0
  #;(let ((requireds '()) (rest '()))
    (dlet ((*env* *env*))
      (do ((vl (car lambda-expr) (cdr vl)))
          ((not (pair? vl))
           (if (not (null? vl))
               (let ((var (make-var vl)))
                 (set! *env* (cons var *env*))
                 (set! rest var)))
           (list (reverse requireds)
                 rest
                 (c0body (cdr lambda-expr) '())))
        (let ((var (make-var (car vl))))
          (set! requireds (cons var requireds))
          (set! *env* (cons var *env*)))))))

;;; SCLETS  Let, Let*, and Letrec.
(define (c0let args)
  (if (end? args) (scbad-args 'let args))
  (if (symbol? (car args))
      (if (end? (cdr args))
          (scbad-args 'let args)
          (c0named-let (car args) (cadr args) (cddr args)))
      (let ((body '()) (defs '()))
        ;;(dlet ((*env* *env*))
          (dolist (def (car args))
            (if (or (end? def) (end? (cdr def)) (not (end? (cddr def))))
                (scbad-binding def))
            (set! defs (cons def defs)) ;; c.scm:c0
            #;(let ((var (make-var (car def))))
              (set! defs (cons (cons var (cadr def)) defs))
              (set! *env* (cons var *env*))))
          (set! body (c0body (cdr args) '()));;)
        (dolist (def defs)
          (let ((var (car def)) (form (cadr def) #;(cdr def))) ;; c.scm:c0
            (if (and (pair? form) (eq? (car form) 'lambda #;'LAMBDA))
                (set-cdr! def (list (c0lambda (cdr form)))) ;; c.scm:c0
                #;(if (or (var-funarg var) (var-assigned var) (var-closed var))
                    (set-cdr! def (c0lambda (cdr form)))
                    (begin (set-cdr! def (c0lam (cdr form)))
                           (set-var-local-fun var #t)
                           (set-var-local-fun-args var (cdr def))))
                (set-cdr! def (list (c0expr form)) #;(c0expr form))))) ;; c.scm:c0
        (list 'let #;c2let (reverse defs) body))))

(define (c0let* args)
  (if (end? args) (scbad-args 'let* args))
  (let ((body '()) (defs '()))
    ;;(dlet ((*env* *env*))
      (dolist (def (car args))
        (if (or (end? def) (end? (cdr def)) (not (end? (cddr def))))
            (scbad-binding def))
        (set! defs (cons def defs)) ;; c.scm:c0
        #;(let ((var (make-var (car def))))
          (set! defs (cons (cons var (cadr def)) defs))
          (set! *env* (cons var *env*))))
      (set! body (c0body (cdr args) '()))
      (dolist (def defs)
       #; (set! *env* (cdr *env*))
        (let ((var (car def)) (form (cadr def) #;(cdr def))) ;; c.scm:c0
          (if (and (pair? form) (eq? (car form) 'lambda #;'LAMBDA))
              (set-cdr! def (list (c0lambda (cdr form)))) ;; c.scm:c0
              #;(if (or (var-funarg var) (var-assigned var) (var-closed var))
                  (set-cdr! def (c0lambda (cdr form)))
                  (begin (set-cdr! def (c0lam (cdr form)))
                         (set-var-local-fun var #t)
                         (set-var-local-fun-args var (cdr def))))
              (set-cdr! def (list (c0expr form)) #;(c0expr form)))));;) ;; c.scm:c0
    (list 'let* #;c2let* (reverse defs) body)))

(define (c0letrec args)
  (if (end? args) (scbad-args 'letrec args))
  (let ((body '()) (defs '()))
    ;;(dlet ((*env* *env*))
      (dolist (def (car args))
        (if (or (end? def) (end? (cdr def)) (not (end? (cddr def))))
            (scbad-binding def))
        (set! defs (cons def defs)) ;; c.scm:c0
        #;(let ((var (make-var (car def))))
          (set! defs (cons (list var '() (cadr def)) defs))
          (set! *env* (cons var *env*))))
      (set! body (c0body (cdr args) '()))

      (dolist (def defs)
              (let ((var (car def))
                    (form (cadr def)) ;; c.scm:c0
                    #;(form (caddr def)))
          (if (and (pair? form) (eq? (car form) 'lambda #;'LAMBDA))
              (set-car! (cdr def) (c0lambda (cdr form))) ;; c.scm
              #;(if (or (var-funarg var) (var-assigned var) (var-closed var))
                  (set-car! (cdr def) (c0lambda (cdr form)))
                  (begin (set-car! (cdr def) (c0lam (cdr form)))
                         (set-var-local-fun var #t)
                         (set-var-local-fun-args var (cadr def))))
              (set-car! (cdr def) (c0expr form)))))

      #;(c0letrec-aux defs);;)
    (list 'letrec #;c2letrec (reverse defs) body)))

;; c0では名前付きletのまま返す
(define (c0named-let name bind body)
  (let (#;(var (make-var name))
        (inits (c0map (lambda (x)
                        (if (or (end? x) (end? (cdr x))
                                (not (end? (cddr x))))
                            (scbad-binding x))
                        (c0expr (cadr x))) 
                      bind))
        (vars (c0map car bind)))
    (list 'let name (map list vars inits) (cadr (c0lam (cons vars body)))) ;; c.scm:c0
    #;(dlet ((*env* (cons var *env*)))
      (let* ((arg-body (cons vars body)) 
             (fun (c0lam arg-body)))
        (cond ((or (var-funarg var) (var-assigned var) (var-closed var))
               (list c2named-let var inits (c0lambda arg-body)))
              (else (set-var-local-fun var #t)
                    (set-var-local-fun-args var (list vars #f))
                    (list c2named-let var inits fun)))))))

(define (c0do args)
  (if (or (end? args) (end? (cdr args)) (end? (cadr args)))
      (scbad-args 'do args))
  (let ((name (newvar) #;(gensym))) ;; c.scm:c0
;    (c0named-let name (car args)
    (c0named-let name (c0map (lambda (x) (list (car x) (cadr x))) (car args))
      `((if ,(caadr args)
            (begin ,@(cdadr args))
            (begin ,@(cddr args)
                   ,(cons name
                          (c0map (lambda (ite)
                                   (cond ((or (end? ite) (end? (cdr ite)))
                                          (scbad-binding ite))
                                         ((end? (cddr ite))
                                          (car ite))
                                         ((end? (cdddr ite))
                                          (caddr ite))
                                         (else (scbad-binding ite))))
                                 (car args)))))))))

;;; SCMISC  Miscellaneous Special Forms.

(define (c0quote args)
  (if (or (end? args) (not (end? (cdr args)))) (scbad-args 'quote args))
  (case (car args)
    ((#f) c0false)
    ((#t) c0true)
    ((()) c0null)
    (else (c0constant (car args)))))

                                        ;(define (c0quasiquote args)
                                        ;  (define (qqcons a b)
                                        ;    (if (pair? b)
                                        ;        (case (car b)
                                        ;          ((list) `(list ,a ,@(cdr b)))
                                        ;          ((list* cons) `(list* ,a ,@(cdr b)))
                                        ;          (else `(cons ,a ,b)))
                                        ;        `(cons ,a ,b)))
                                        ;  (define (qqquote x) (if (or (symbol? x) (pair? x)) `',x x))
                                        ;  (define (qq x)
                                        ;    (cond ((pair? x)
                                        ;           (let ((a (car x)) (b (cdr x)))
                                        ;             (cond ((eq? a 'unquote) (car b))
                                        ;                   ((null? b)
                                        ;                    (cond ((and (pair? a) (eq? (car a) 'unquote))
                                        ;                           `(list ,(cadr a)))
                                        ;                          ((and (pair? a) (eq? (car a) 'unquote-splicing))
                                        ;                           (cadr a))
                                        ;                          (else (let ((aa (qq a)))
                                        ;                                  (if (eq? aa a) x `(list ,aa))))))
                                        ;                   (else
                                        ;                     (let ((bb (qq b)))
                                        ;                       (cond ((and (pair? a) (eq? (car a) 'unquote))
                                        ;                              (if (eq? bb b)
                                        ;                                  `(cons ,(cadr a) ,(qqquote b))
                                        ;                                  (qqcons (cadr a) bb)))
                                        ;                             ((and (pair? a) (eq? (car a) 'unquote-splicing))
                                        ;                              (cond ((eq? bb b)
                                        ;                                     `(append ,(cadr a) ,(qqquote b)))
                                        ;                                    ((and (pair? bb) (eq? (car bb) 'append))
                                        ;                                     `(append ,(cadr a) ,@(cdr bb)))
                                        ;                                    (else `(append ,(cadr a) ,bb))))
                                        ;                             (else (let ((aa (qq a)))
                                        ;                                     (if (eq? aa a)
                                        ;                                         (if (eq? bb b) x
                                        ;                                             (qqcons (qqquote a) bb))
                                        ;                                         (if (eq? bb b)
                                        ;                                             (qqcons aa (qqquote b))
                                        ;                                             (qqcons aa bb)))))))))))
                                        ;      ((vector? x)
                                        ;       (if (let variable-length? ((i (- (vector-length x) 1)))
                                        ;             (if (negative? i) #f
                                        ;                 (or (and (pair? (vector-ref x i))
                                        ;                          (eq? (car (vector-ref x i)) 'unquote-splicing))
                                        ;                     (variable-length? (- i 1)))))
                                        ;           `(list->vector ,(qq (vector->list x)))
                                        ;           (let ((y (vector->list x)))
                                        ;             (if (eq? (qq y) y) x
                                        ;                 `(vector
                                        ;                    ,@(let qqelem ((l y))
                                        ;                        (if (null? l) '()
                                        ;                            (cons (let ((aa (qq (car l))))
                                        ;                                    (if (eq? aa (car l))
                                        ;                                        (qqquote aa)
                                        ;                                        aa))
                                        ;                                  (qqelem (cdr l))))))))))
                                        ;      (else x)))
                                        ;  (if (or (end? args) (not (end? (cdr args)))) (scbad-args 'quasiquote args))
                                        ;  (let ((xx (qq (car args))))
                                        ;    (c0expr (if (eq? xx (car args)) (qqquote xx) xx))))

;;; Quasiquote by T.Komiya

(let () (append))

;; c0expr, (c0quasiquote args)
;; `(1 ,a ,@b) -> (quasiquote (1 (unquote a) (unquote-splicing b))
(define (c0quasiquote args)
  (define (qq x level)
    (cond ((vector? x)
           `(list->vector ,(qq (vector->list x) level)))
          ((not (pair? x))
           (if (symbol? x)
               `',x 
               x)) ;; 文字列, 文字, 数値, 真偽値
          ((eq? (car x) 'quasiquote)
           `(cons #;$$cons ,''quasiquote ,(qq (cdr x) (set! level (+ level 1)) #;(1+ level)))) ;; 入れ子の準クオート ;; c.scm:c0
          ((eq? (car x) 'unquote) ;; x -> (unquote ...)
           (let ((xcdr (cdr x))) ;; (...)
             (if (or (not (pair? xcdr)) 
                     (not (null? (cdr xcdr))))
                 (scbad-args 'unquote xcdr) 
                 (if (zero? level)
                     (car xcdr) ;; ...を評価
                     `(cons #;$$cons ,''unquote ,(qq xcdr (set! level (- level 1)) #;(1- level))))))) ;; 入れ子のレベルを一つ下げる
          ((and (pair? (car x))
                (eq? (caar x) 'unquote-splicing)) ;; x -> (unquote-splicing ...)
           (let ((xcar (car x)) ;; unquote-splicing
                 (xcdr (cdr x))) ;; (...)
             (if (or (not (pair? (cdr xcar)))
                     (not (null? (cddr xcar))))
                 (scbad-args 'unquote-splicing xcar))
             (if (zero? level)
                 `(append #;$$append ,(cadr xcar) ,(qq xcdr level)) 
                 `(cons #;$$cons (cons #;$$cons ,''unquote-splicing ,(qq (cdr xcar) (set! level (- level 1)) #;(1- level))) ;; 入れ子のレベルを一つ下げる
                          ,(qq xcdr level))))) ;; 
          (else
           `(cons #;$$cons ,(qq (car x) level) ;; 
                    ,(qq (cdr x) level))))) 
  (if (or (end? args) (not (end? (cdr args)))) (scbad-args 'quasiquote args))
  (c0expr (qq (car args) 0)))

(define (c0define args)
  (scerror "Bad use of DEFINE."))

(define (c0macro args)
  (scerror "Bad use of MACRO."))

;;; SCVREF  Variable References.
(define (c0vref name)
  name
  #;(let lookup ((env *env*) (ccb #f))
    (if (null? env)
        (list c2gvref name)
        (let ((var (car env)))
          (cond ((eq? var 'CB) (lookup (cdr env) #t))
                ((eq? (var-name var) name)
                 (if ccb (set-var-closed var #t))
                 (set-var-funarg var #t)
                 (list c2vref var ccb))
                (else (lookup (cdr env) ccb)))))))

;; c0fmlaでnotへの束縛があるか調べている
#;(define (c0lookup name)
  (let lookup ((env *env*))
    (cond ((null? env) #f)
          ((and (not (eq? (car env) 'CB))
                (eq? (var-name (car env)) name))
           #t)
          (else (lookup (cdr env))))))

(define (c0set! args)
  (if (or (end? args) (end? (cdr args)) (not (end? (cddr args))))
      (scbad-args 'set! args))
  (let ((name (car args))
        (form (cadr args)))
    (cond ((not (symbol? name))
           (scerror "~s is not a symbol." name))
          ((member name '(if cond case and or begin lambda delay let let* 
                          letrec do set! quote quasiquote define macro))
           (scerror "Cannot set to the keyword ~s." name))
          ((eq? name form)
           (c0vref name))
          (else
           (list 'set! name (c0expr form)) ;; c.scm:c0
            #;(let lookup ((env *env*) (ccb #f))
              (if (null? env)
                  (list c2gset! name (c0expr form))
                  (let ((var (car env)))
                    (cond ((eq? var 'CB) (lookup (cdr env) #t))
                          ((eq? (var-name var) name)
                           (if ccb (set-var-closed var #t))
                           (set-var-assigned var #t)
                           (list c2set! var ccb (c0expr form)))
                          (else (lookup (cdr env) ccb))))))))))

(define (end? x)
  (cond ((pair? x) #f) 
        ((null? x) #t)
        (else (scerror "~s is not a pair." x))))

(define (c0map fun l) 
  (if (end? l)
      '()
      (cons (fun (car l)) (c0map fun (cdr l)))))

(define (scbad-args name args)
  (scerror "~s is bad arguments to ~s." name args))

(define (scbad-binding binding)
  (scerror "~s is bad variable binding." binding))

(define (scerror s . args)     ; Modefied by N.Watanabe
  ;; for UNIX
                                        ;  (error 'compile-error         
                                        ;	 'format-string    s
                                        ;	 'format-arguments args)
                                        ;  (return-to-sctop)
  ;;
  (format #t "Compile error : ") ;; c.scm:c0
  #;(format #t "~&Compile error : ")
  (apply format #t s args)
  (format #t "~%")
  ;; for DOS
                                        ;
  ;; for UNIX
  #;(error-break)
  (c.scm:*c0cont* #f) ;; c.scm:c0
  )
