;; let式, let*式の初期値の部分
;; 名前付きletは(let 名前 束縛変数のリスト 初期値のリスト 本体)
;;
(use util.match)
(define-syntax dlet
  (syntax-rules ()
    ((dlet ((var val) ...) body ...)
     (let ((stack (list var ...)))
       (define (pop)
         (let ((top (car stack)))
           (set! stack (cdr stack))
           top))
       (set! var val) ...
       (let ((retval (begin body ...)))
         (set! var (pop)) ...
         retval)))))

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

(define *re-compile-level* 0)


;;; SCTOPS  Compiler toplevel.
;;; 1パス目はc1, 2パス目はc2

(define *bc-code* '())
(define *bc-codep* '())

(define *local-funs* '())
(define *cclosures* '())

(define *inline-functions* #f)

(define set-fun-data '())            ;;;;; By A243  Dec, 1992 ;;;;;

;; 記号または文字列を受け取り, 拡張子lspのファイルを開く
;; ファイル内の式をコンパイルし, コンパイル結果を拡張子fasのファイルに出力する
(define (compile-file x)
  (let* ((fname (if (symbol? x)
		            (symbol->string x)
		            x))
         (out (open-output-file (string-append fname ".fas")))
         (in (open-input-file (string-append fname ".lsp"))))
    (do ((form (read in) (read in)))
        ((eof-object? form))
      (write-code (compile form) out)
                                        ;(write (compile form) out)
                                        ;(write (compile form))
      )
    (close-input-port in)
    (close-output-port out)))

(define (write-code code out)
  (case (car code)
    ((DEFINE)
     (case (car (caddr code))
       ((COMPILED-FUNCTION)
                                        ;        (format out "~&(DEFINE ~s (COMPILED-FUNCTION '~s ~d '(~%"
        (format out "~&(DEFINE ~s (COMPILED-FUNCTION ~s ~d '(~%"
                (cadr code) (cadr (caddr code)) (caddr (caddr code)))
        (write-code-body (cadr (cadddr (caddr code))) out)
        (format out ")))"))
       ((LOAD-TIME-EVAL)
        (format out "~&(DEFINE ~s (LOAD-TIME-EVAL '(~%" (cadr code))
        (write-code-body (cadr (cadr (caddr code))) out)
        (format out ")))"))))
    ((MACRO)
                                        ;     (format out "~&(MACRO ~s (COMPILED-FUNCTION '~s ~d '(~%"
     (format out "~&(MACRO ~s (COMPILED-FUNCTION ~s ~d '(~%"
             (cadr code) (cadr (caddr code)) (caddr (caddr code)))
     (write-code-body (cadr (cadddr (caddr code))) out)
     (format out ")))"))
    ((LOAD-TIME-EVAL)
     (format out "~&(LOAD-TIME-EVAL '(~%")
     (write-code-body (cadr (cadr code)) out)
     (format out "))"))
    ((BEGIN)
     (format out "~&(BEGIN~%")
     (write-code-body (cdr code) out)
     (format out ")"))))

#|
(define (write-code-body body out)
(do ((x body (cdr x)))
((null? x))
(if (symbol? (car x))
(format out "~&  ~s" (car x))
(format out "	~s~%" (car x)))))
|#

(define (write-code-body body out)
  (do ((x body (cdr x)))
      ((null? x))
    (if (symbol? (car x))
        (format out "~&  ~s" (car x))
        (if (eq? (caar x) 'LOCAL-FUNCTION)
            (begin
              (format out "~&  (LOCAL-FUNCTION~%")
              (write-code-body (cdar x) out)
              (format out "  )~%"))
            (format out "	~s~%" (car x))))))

;; 
;; x, readで読み込んだ式
(define (compile x) 
  (if (pair? x) ;; (...)
      (case (car x)
        ((define macro)
         (if (symbol? (cadr x)) ;; (define var ...)
             (let ((form  (caddr x) #;(macroexpand (caddr x)))) ;; ...をマクロ展開
               (if (and (pair? form)
			            (eq? (car form) 'lambda)) ;; form -> (lambda params ...) ;; 非省略記法による関数定義
                   (compile-function (car x) ;; define, macro
                                     (cons (cadr x) (cadr form)) ;; (var params), (var . param)
                                     (cddr form)) ;; (...) ;; lambda式の本体
                   `(,(car x) ,(cadr x) ,(compile-expr form)))) ;; (define var (compile-expr ...)), 変数の定義
             (compile-function (car x) (cadr x) (cddr x)))) ;; (define (x ...) ...) ;; 省略記法による関数定義
        ((begin)
	     (cons 'BEGIN (map compile (cdr x)))) 
	    ;; 関数適用, またはマクロのとき
        (else #;(if (and (symbol? (car x))
         (macro-function (car x)))
         (compile ((macro-function (car x)) x)) ;; マクロ展開したコードをコンパイル
         (compile-expr x))
         (compile-expr (car x) x))) ;; 関数適用
      (compile-expr x)))

;; compile, (compile-expr form)
(define (compile-expr form)
  (wt-begin) ;; *bc-code*, *bc-codep*を(())に初期化
  (let ((x (dlet ((*env* '())) 
		         (c1expr form)))) 
    (dlet ((*local-funs* '())
	       (*cclosures* '())
           (*level* 0)
	       (*sp* -1)
	       (*bp* 0)
	       (*last-label* 0)
           (*cenv-length* 0)
	       (*cenv-loc* '())
	       (*clink* '())
	       (*ccb-vs* 0)
           (*value-to-go* 'BX)
	       (*exit* 'XRETURN))
	      (c2expr x) ;; (apply (car x) (cdr x))
	      (do ()
	          ((and (null? *cclosures*)
		            (null? *local-funs*)))
            (if (not (null? *cclosures*))
		        (let ((y (car *cclosures*)))
                  (set! *cclosures* (cdr *cclosures*))
                  ;; for DOS
                                        ;(apply c2cclosure y)
                  ;; for UNIX
                  (apply (car y) (cdr y))
                  ;;
                  ))
            (if (not (null? *local-funs*))
		        (let ((y (car *local-funs*)))
                  (set! *local-funs* (cdr *local-funs*))
                  (apply c2local-fun y))))))
  (wt-end-expr))

;; compile
;; firstはdefineかmacro
;; (define (x ...) ...)の場合, argsはxも含む(x ...)である
(define (compile-function first args body)
  (if (not (and (pair? args)
		        (symbol? (car args))))
      (scerror "~s is a bad arg to DEFINE." args))
  (wt-begin) ;; *bc-code*, *bc-codep* <- (())
  (let ((x (dlet ((*env* '()))
		         (c1lam (cons (cdr args) body))))) ;; lambda式の引数と本体, ((...) ...)
    (dlet ((*local-funs* '())
           (*cclosures* '())
           (*level* 0) (*sp* -1) (*bp* 0) (*last-label* 0))
          (apply c2lam (next-label2) x)
          (do ()
              ((and (null? *cclosures*)
                    (null? *local-funs*)))
            (if (not (null? *cclosures*))
                (let ((y (car *cclosures*)))
                  (set! *cclosures* (cdr *cclosures*))
                  ;; for DOS
                                        ;(apply c2cclosure y) ; ; ; ;
                  ;; for UNIX
                  (apply (car y) (cdr y))
                  ;;
                  ))
            (if (not (null? *local-funs*))
                (let ((y (car *local-funs*)))
                  (set! *local-funs* (cdr *local-funs*))
                  (apply c2local-fun y)))))
    )
  (wt-end-function first (car args) args))






;; マクロ定義があればマクロ展開したプログラムを返す
;; macro-functionが何をやっているか確認する!!!
#;(define (macroexpand form)
(if (and (pair? form) (symbol? (car form)) (macro-function (car form)))
(macroexpand ((macro-function (car form)) form))
form))

;;; Intrinsic constants

;; c1quote
(define (c1constant x) #;(list c2constant x)
  (if (or (boolean? x)
          (number? x)
          (char? x)
          (string? x))
      x
      `(quote ,x)))

(define (c2constant x)
  (case *value-to-go*
    ((BX) (wt `(get-const ,x)))
    ((PUSH) (wt `(push-const ,x))))
  (cmp:exit))

(define c1null #;(list c2constant '())
  '())
(define c1false #;(list c2constant #f)
  #f)
(define c1true #;(list c2constant #t)
  #t)
(define c1if-else-default #;(list c2constant '())
  '())
(define c1cond-else-default #;(list c2constant '())
  '())
(define c1begin-empty-default #;(list c2constant '())
  '())

;;; SCBIND  Variable Binding.

(define *sp* 0) ;; スタックポインタ
(define *bp* 0)

(define *clink* '())
(define *ccb-vs* 0)
(define *cenv-length* 0)
(define *cenv-loc* '())
(define *level* 0)

(define (c2lambda-bind var)
  (set! *sp* (- *sp* 1))
  (c2bind var))

(define (c2bind var)
  (set-var-loc var (cons *level* *sp*))
  (cond ((var-closed var)
         (set-var-closed var *ccb-vs*)
         (set! *ccb-vs* (+ *ccb-vs* 1))
         (cond ((null? *clink*) 
                (wt `(lift-nil ,(- *sp* *bp*))))
               ((= (car *clink*) *level*)
                (wt `(lift0 ,(- *sp* *bp*) ,(- (cdr *clink*) *bp*))))
               (else (wt `(lift ,(- *sp* *bp*) ,*bp*
                                ,(- *level* (car *clink*)) ,(cdr *clink*)))))
         (set! *clink* (var-loc var)))
        ((var-assigned var)
         (wt `(lift-nil ,(- *sp* *bp*))))))

;;; SCCOND  Conditionals.
;; c1expr, (c1if args)
(define (c1if args) 
  (if (or (end? args) ;; testがない
          (end? (cdr args)) ;; thenがない
          (and (not (end? (cddr args))) ;; else節があり
               (not (end? (cdddr args))))) ;; 更に節がある
      (scbad-args 'if args)) ;; 引数エラー
  (list #;c2if
   'if
   (c1fmla (car args)) ;; test
   (c1expr (cadr args)) ;; then
   (if (null? (cddr args))
       c1if-else-default
       (c1expr (caddr args))))) ;; else

;; c1if, (c1fmla (car args)), test節
(define (c1fmla fmla)
  (if (pair? fmla) 
      (case (car fmla)
        ((#;AND and) (cond ;; (and ...)
                      ((end? (cdr fmla)) c1true) ;; 引数がない 真
                      ((end? (cddr fmla)) (c1fmla (cadr fmla))) ;; 引数が１つ 
                      (else (cons #;'FMLA-AND 'and (c1map c1fmla (cdr fmla)))))) ;; 引数が２つ以上
        ((#;OR or) (cond ;; (or ...)
                    ((end? (cdr fmla)) c1false) ;; 引数がない 偽
                    ((end? (cddr fmla)) (c1fmla (cadr fmla))) ;; 引数が１つ
                    (else (cons #;'FMLA-OR 'or (c1map c1fmla (cdr fmla)))))) ;; 引数が２つ以上
        ((#;NOT not) (cond ((c1lookup 'not) (c1expr fmla)) ;; (not ...)
                           ((or (end? (cdr fmla)) (not (end? (cddr fmla)))) ;; 引数なし, 引数２つ以上
                            (scbad-args 'not (cdr fmla))) ;; 引数エラー
                           (else (list #;'FMLA-NOT 'not (c1fmla (cadr fmla))))))
        (else (c1expr fmla))) 
      (c1expr fmla))) 

(define (c2if fmla form1 form2)
  (if (and (eq? (car form2) c2constant)
           (eq? *value-to-go* 'TRASH)
           (member *exit* '(NEXT ESCAPE)))
      (let ((Tlabel (next-label))
            (Flabel *exit-label*))
        (dlet ((*exit* 'NEXT)
               (*exit-label* Tlabel))
              (CJF fmla Flabel))
        (wt-label Tlabel)
        (c2expr form1))
      (let ((Tlabel (next-label))
            (Flabel (next-label)))
        (dlet ((*exit* 'NEXT)
               (*exit-label* Tlabel))
              (CJF fmla Flabel))
        (wt-label Tlabel)
        (case *exit*
          ((NEXT) (dlet ((*exit* 'ESCAPE))
                        (c2expr form1)))
          ((UNWIND-NEXT) (dlet ((*exit* 'UNWIND-ESCAPE))
                               (c2expr form1)))
          (else (c2expr form1)))
        (wt-label Flabel)
        (c2expr form2))))

(define (CJT fmla Tlabel)
  (case (car fmla)
    ((FMLA-AND) (do ((fs (cdr fmla) (cdr fs)))
                    ((null? (cdr fs))
                     (CJT (car fs) Tlabel))
                  (let ((Flabel *exit-label*))
                    (dlet ((*exit-label* (next-label)))
                          (CJF (car fs) Flabel)
                          (wt-label *exit-label*)))))
    ((FMLA-OR) (do ((fs (cdr fmla) (cdr fs)))
                   ((null? (cdr fs))
                    (CJT (car fs) Tlabel))
                 (dlet ((*exit-label* (next-label)))
                       (CJT (car fs) Tlabel)
                       (wt-label *exit-label*))))
    ((FMLA-NOT) (CJF (cadr fmla) Tlabel))
    (else (c2get-expr* fmla)
          (wt `(jmp-if-true ,(touch-label Tlabel))))))

(define (CJF fmla Flabel)
  (case (car fmla)
    ((FMLA-AND) (do ((fs (cdr fmla) (cdr fs)))
                    ((null? (cdr fs))
                     (CJF (car fs) Flabel))
                  (dlet ((*exit-label* (next-label)))
                        (CJF (car fs) Flabel)
                        (wt-label *exit-label*))))
    ((FMLA-OR) (do ((fs (cdr fmla) (cdr fs)))
                   ((null? (cdr fs))
                    (CJF (car fs) Flabel))
                 (let ((Tlabel *exit-label*))
                   (dlet ((*exit-label* (next-label)))
                         (CJT (car fs) Tlabel)
                         (wt-label *exit-label*)))))
    ((FMLA-NOT) (CJT (cadr fmla) Flabel))
    (else (c2get-expr* fmla)
          (wt `(jmp-if-false ,(touch-label Flabel))))))

;; c1expr, (c1and args)
(define (c1and args)
  (cond ((end? args) c1true)
        ((end? (cdr args)) (c1expr (car args)))
        (else `(and ,@(c1args args))
              #;(list c2and (c1args args))))) ;; 引数を解析

;; c1expr, (c1or args)
(define (c1or args)
  (cond ((end? args) c1false)
        ((end? (cdr args)) (c1expr (car args)))
        (else `(or ,@(c1args args))
              #;(list c2or (c1args args))))) ;; 引数を解析

(define (c2and forms)
  (do ((forms forms (cdr forms)))
      ((null? (cdr forms))
       (c2expr (car forms))) ;; 最後の式の評価結果を返す
    (c2get-expr* (car forms))
    (let ((label (next-label2)))
      (wt `(jmp-if-true ,(touch-label label))) ;; ,(cdr lable)じゃだめ???
      (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
      (exit-always)
      (wt-label label))))

(define (c2or forms)
  (dlet ((*sp* *sp*))
        (do ((forms forms (cdr forms)))
            ((null? (cdr forms))
             (c2expr (car forms))) ;; 最後の式の評価結果を返す
          (c2get-expr* (car forms))
          (let ((label (next-label2)))
            (wt `(jmp-if-false ,(touch-label label))) ;; ,(cdr lable)じゃだめ???
            (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
            (exit-always)
            (wt-label label)))))

;; 展開したcondをc1exprにわたす
;; c1expr, (c1cond args)
(define (c1cond args)
  (c1expr (c1expand-cond args)))

;; cond式を展開する. 基本的に入れ子のifとbeginにする
;; c1cond, (c1expand-cond args)
(define (c1expand-cond args)
  (if (null? args)
      #;(cadr c1cond-else-default)
      c1cond-else-default ;; (cadr (list c2constant '()))->null
      (let ((clause (car args)) ;; cond式の最初の節
            (rest (cdr args))) ;; 残りの節
        (cond ((end? clause)
               (scbad-args 'cond clause)) ;; 節がない
              ((eq? (car clause) 'else)
               `(begin ,@(cdr clause)))
              ((end? (cdr clause)) ;; 節のtest式しかないとき
               (if (end? rest) 
                   (car clause) ;; 最後の節のときはtest式が結果
                   `(or ,(car clause) ,(c1expand-cond rest)))) 
              ((and (eq? (cadr clause) '=>) ;; Gauche p.102参照, 省略記法
                    (not (end? (cddr clause)))
                    (end? (cdddr clause)))
               (let ((temp (gensym)))
                 `(let ((,temp ,(car clause))) ;; tempにtest式の結果を保持
                    (if ,temp
                        (,(caddr clause) ,temp) ;; test式が真ならこれを引数にthen式を呼び出す
                        ,(c1expand-cond rest)))))
              (else `(if ,(car clause) 
                         (begin ,@(cdr clause)) ;; test式が真ならその節の残りの式をすべて評価
                         ,(c1expand-cond rest)))))))

;; c1expr, (c1case args)
(define (c1case args) ;; (case key ((data ...) exp ...) ...)
  (cond ((end? args) (scbad-args 'case args))
        ((end? (cdr args)) ;; keyしかない
         (c1begin (list (car args) (cadr c1cond-else-default)))) ;; keyを評価してnullを返す
        (else
         (let ((temp (gensym)))
           (c1expr `(let ((,temp ,(car args)))
                      ,(c1expand-case temp (cdr args))))))))

;; case式をcaseを使わない形に展開する
;; c1case, (c1expand-case temp (cdr args))
(define (c1expand-case key clauses)
  (if (end? clauses) 
      (cadr c1cond-else-default) ;; else節なし
      (let ((clause (car clauses)) (rest (cdr clauses))) ;; 最初の節, 残りの節
        (cond ((end? clause) (scbad-args 'case clause))
              ((eq? (car clause) 'else)
               `(begin ,@(cdr clause))) ;; else節なら節の式をすべて評価
              ((null? (car clause)) ;; keyと比較するデータがないとき
               (c1expand-case key rest))
              (else `(if (or ,@(c1map (lambda (sym) `(eqv? ,key ',sym))
                                      (car clause))) ;; c1mapの評価結果はリストなので,@でorの引数に展開している
                         (begin ,@(cdr clause)) 
                         ,(c1expand-case key rest)))))))

;;; SCEVAL  The Expression Dispatcher.
;; c1begin, (c1expr (car forms))
;; compile-expr, (c1expr form), *env*->()
(define (c1expr form)
  (cond ((symbol? form) 
         (c1vref form)) ;; 変数参照
        ((pair? form)
         (let ((fun (car form))
               (args (cdr form))) ;; (fun args)
           (cond ((symbol? fun)
                  (case fun
                    ((if) (c1if args)) 
                    ((cond) (c1cond args)) ;; cond式をif式に展開して再度c1exprで評価する
                    ((case) (c1case args)) ;; case式をif式に展開
                    ((and) (c1and args))
                    ((or) (c1or args))
                    ((begin) (c1begin args))
                    ((lambda) (c1lambda args)) ;; (lambda args)
                    ((delay) (c1delay args)) ;; (delay args)
                    ((let) (c1let args)) 
                    ((let*) (c1let* args))
                    ((letrec) (c1letrec args))
                    ((do) (c1do args)) ;; 名前付きletへ
                    ((set!) (c1set! args)) ;;
                    ((quote) (c1quote args))
                    ((quasiquote) (c1quasiquote args))
                    ((define) (c1define args)) ;; エラー
                    ((macro) (c1macro args)) ;; エラー
                    (else (if (>= *re-compile-level* 2)
                              (set-fun-data fun)) ;;;;; By A243  Nov, 1992 ;;;;;
                          #;(if (macro-function fun)
                          (c1expr ((macro-function fun) form))
                          (c1symbol-fun fun args))
                          (c1symbol-fun fun args)))) ;; (f args)の呼び出し
                 ((and (pair? fun)
                       (eq? (car fun) 'lambda)) ;; ((lambda ...) args)
                  (c1lambda-fun (cdr fun) args)) ;; ((...) ...) ;; let式に書き換える
                 (else
                  (list #;c2funcall (c1expr fun) (c1args args)))))) ;; どういうパターンか
        (else
         (case form
           ((#f) c1false)
           ((#t) c1true)
           ((()) c1null)
           (else (c1constant form))))))

;; c1expr, (c1symbol-fun fun args)
(define (c1symbol-fun name args)
  (let parse ((env *env*)
              (ccb #f))
    (cond ((null? env) ;; グローバル関数を呼び出す
           #;`(,c2funcall (,c2gvref ,name) ,(c1args args))
           `(,name ,@(c1args args)))
          ((eq? (car env) 'CB)
           (parse (cdr env) #t)) ;; lambda式内部?
          ((eq? (var-name (car env)) name) ;; ローカル関数を呼び出す
           (if ccb (set-var-closed (car env) #t))
           #;`(,c2funcall (,c2vref ,(car env) ,ccb) ,(c1args args))
           `(,(car env) ,@(c1args args)))
          (else
           (parse (cdr env) ccb)))))

;; ((lambda params body) args)
;; 各paramsにargsを束縛するlet式に変形する
;; c1expr, (c1lambda-fun (cdr fun) args)
(define (c1lambda-fun lambda-expr args)
  ;; 仮引数と実引数のリストのリストを作成, ((param arg) ...)
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
      (c1let (cons (make-defs (car lambda-expr) args)
                   (cdr lambda-expr))))) ;; (...), lambda式の実行本体部分

;; args先頭から解析し、リストにする
;; c1symbol-fun, (c1args args)
;; c1and, (c1args args)
;; c1or, (c1args args)
(define (c1args forms)
  (if (end? forms)
      '()
      (cons (c1expr (car forms))
            (c1args (cdr forms)))))

(define (c2expr form) (apply (car form) (cdr form)))

(define (c2push-expr* form)
  (dlet ((*value-to-go* 'PUSH) (*exit* 'NEXT) (*exit-label* (next-label)))
        (c2expr form)
        (wt-label *exit-label*)
        (set! *sp* (- *sp* 1))))

(define (c2get-expr* form)
  (dlet ((*value-to-go* 'BX)
         (*exit* 'NEXT)
         (*exit-label* (next-label)))
        (c2expr form)
        (wt-label *exit-label*)))

(define (c2funcall fun args)
  (if (and (eq? (car fun) c2vref) (var-local-fun (cadr fun)))
      (if (let* ((actuals (length args))
                 (x (var-local-fun-args (cadr fun)))
                 (requireds (length (car x))))
            (or (< actuals requireds)
                (and (null? (cadr x)) (> actuals requireds))))
          (scerror "Wrong number of args to the local fun ~s."
                   (var-name (cadr fun)))
          (case *exit*
            ((XRETURN LRETURN)
             (cond ((< *level* (var-local-fun (cadr fun)))
                    (c2lcall *exit* fun args))
                   ((and (= *level* (var-local-fun (cadr fun)))
                         (not (null? args))
                         (null? (cdr args))
                         (= *sp* -1))
                    (c2get-expr* (car args))
                    (wt '(set-lval0 -1))
                    (wt `(lcall ,(touch-label (var-loc (cadr fun))))))
                   (else
                    (dlet ((*bp* *bp*) (*sp* *sp*))
                          (if (not (null? args))
                              (do ((x args (cdr x)))
                                  ((null? (cdr x))
                                   (c2get-expr* (car x)))
                                (c2push-expr* (car x))))
                          (wt `(trlcall ,(- *level* (var-local-fun (cadr fun)))
                                        ,(length args)
                                        ,(touch-label (var-loc (cadr fun)))))))))
            (else (cond ((and (not (eq? *value-to-go* 'PUSH))
                              (or (eq? *exit* 'NEXT) (eq? *exit* 'ESCAPE)))
                         (c2lcall (touch-label *exit-label*) fun args))
                        (else (let ((label (next-label2)))
                                (c2lcall (touch-label label) fun args)
                                (wt-label label))
                              (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
                              (cmp:exit))))))

      (or (and *inline-functions* (c2call-simple-function fun args))
          (case *exit*
            ((XRETURN LRETURN)
             (dlet ((*bp* *bp*) (*sp* *sp*))
                   (if (null? args)
                       (c2get-expr* fun)
                       (begin (c2push-expr* fun)
                              (do ((x args (cdr x)))
                                  ((null? (cdr x))
                                   (c2get-expr* (car x)))
                                (c2push-expr* (car x)))))
                   (wt `(,(if (eq? *exit* 'XRETURN) 'trcall 'ltrxcall)
                         ,(length args)))))
            (else (cond ((and (not (eq? *value-to-go* 'PUSH))
                              (or (eq? *exit* 'NEXT) (eq? *exit* 'ESCAPE)))
                         (c2xcall *exit-label* fun args))
                        (else (let ((label (next-label2)))
                                (c2xcall label fun args)
                                (wt-label label))
                              (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
                              (cmp:exit))))))))

(define (c2lcall return fun args)
  (dlet ((*bp* *bp*) (*sp* *sp*))
        (let ((level (- *level* (var-local-fun (cadr fun)))))
          (if (negative? level)
              (wt `(open-lframe-1 ,*bp* ,return))
              (wt `(open-lframe ,*bp* ,level ,return))))
        (for-each c2push-expr* args)
        (wt `(lcall ,(touch-label (var-loc (cadr fun))) ,return))))

(define (c2xcall return fun args)
  (dlet ((*bp* *bp*) (*sp* *sp*))
        (wt `(open-frame ,(touch-label return)))
        (c2push-expr* fun)
        (for-each c2push-expr* args)
        (wt `(xcall ,(cdr return)))))

(define (c2call-simple-function fun args)
  (and (eq? (car fun) c2gvref)
       (case (cadr fun)
         ((car cdr not null? caar cadr cdar cddr pair? symbol? zero? 1+ 1-
               vector? vector-length
               $$car $$cdr $$not $$null? $$caar $$cadr $$cdar $$cddr $$pair?
               $$symbol? $$zero? $$1+ $$1- $$vector? $$vector-length)
          (and (not (null? args))
               (null? (cdr args))
               (begin (c2get-expr* (car args))
                      (wt (case (cadr fun)
                            ((car $$car) '(get-car))
                            ((cdr $$cdr) '(get-cdr))
                            ((not $$not) '(not))
                            ((null? $$null?) '(null?))
                            ((caar $$caar) '(get-caar))
                            ((cadr $$cadr) '(get-cadr))
                            ((cdar $$cdar) '(get-cdar))
                            ((cddr $$cddr) '(get-cddr))
                            ((pair? $$pair?) '(pair?))
                            ((symbol? $$symbol?) '(symbol?))
                            ((zero? $$zero?) '(zero?))
                            ((1+ $$1+) '(1+))
                            ((1- $$1-) '(1-))
                            ((vector? $$vector?) '(vector?))
                            ((vector-length $$vector-length)
                             '(vector-length))))
                      (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
                      (cmp:exit) #t)))
         ((cons $$cons)
          (and (not (null? args))
               (not (null? (cdr args)))
               (null? (cddr args))
               (begin
                 (cond ((eq? (caar args) c2constant)
                        (c2get-expr* (cadr args))
                        (wt `(make-cons1 ,(cadar args))))
                       ((eq? (caadr args) c2constant)
                        (c2get-expr* (car args))
                        (wt (if (null? (cadadr args))
                                '(ncons)
                                `(make-cons2 ,(cadadr args)))))
                       (else
                        (c2push-expr* (car args))
                        (c2get-expr* (cadr args))
                        (wt '(make-cons))))
                 (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
                 (cmp:exit) #t)))
         ((list $$list)
          (cond ((null? args) (c2constant '()))
                ((<= (length args) 5)
                 (do ((x args (cdr x)))
                     ((null? (cdr x))
                      (c2get-expr* (car x))
                      (wt '(ncons)))
                   (c2push-expr* (car x)))
                 (dotimes (i (- (length args) 1)) (wt '(make-cons)))
                 (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
                 (cmp:exit) #t)
                (else #f)))
         ((eq? eqv? equal? $$eq? $$eqv? $$equal?)
          (and (not (null? args))
               (not (null? (cdr args)))
               (null? (cddr args))
               (begin
                 (cond ((eq? (caar args) c2constant)
                        (c2get-expr* (cadr args))
                        (wt (list (case (cadr fun)
                                    ((eq? $$eq?) 'eq-const?)
                                    ((eqv? $$eqv?) 'eqv-const?)
                                    ((equal? $$equal?) 'equal-const?))
                                  (cadar args))))
                       ((eq? (caadr args) c2constant)
                        (c2get-expr* (car args))
                        (wt (if (not (null? (cadadr args)))
                                (list (case (cadr fun)
                                        ((eq? $$eq?) 'eq-const?)
                                        ((eqv? $$eqv?) 'eqv-const?)
                                        ((equal? $$equal?) 'equal-const?))
                                      (cadadr args))
                                '(null?))))
                       (else
                        (c2push-expr* (car args))
                        (c2get-expr* (cadr args))
                        (wt (case (cadr fun)
                              ((eq? $$eq?) '(eq?))
                              ((eqv? $$eqv?) '(eqv?))
                              ((equal? $$equal?) '(equal?))))))
                 (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
                 (cmp:exit) #t)))
         ((+ * - / < > <= >= = set-car! set-cdr! vector-ref
             $$+ $$* $$- $$/ $$< $$> $$<= $$>= $$= $$set-car! $$set-cdr!
             $$vector-ref)
          (and (not (null? args))
               (or (and (not (null? (cdr args)))
                        (null? (cddr args))
                        (begin
                          (c2push-expr* (car args))
                          (c2get-expr* (cadr args))
                          (wt (case (cadr fun)
                                ((+ $$+) '(+))
                                ((* $$*) '(*))
                                ((- $$-) '(-))
                                ((/ $$/) '(/))
                                ((< $$<) '(<))
                                ((> $$>) '(>))
                                ((<= $$<=) '(<=))
                                ((>= $$>=) '(>=))
                                ((= $$=) '(=))
                                ((set-car! $$set-car!) '(set-car!))
                                ((set-cdr! $$set-cdr!) '(set-cdr!))
                                ((vector-ref $$vector-ref) '(vector-ref))))
                          (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
                          (cmp:exit) #t))
                   (case (cadr fun)
                     ((- / $$- $$/)
                      (and (null? (cdr args))
                           (begin
                             (c2get-expr* (car args))
                             (wt (case (cadr fun)
                                   ((- $$-) '(0-))
                                   ((/ $$/) '(1/))))
                             (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
                             (cmp:exit) #t)))
                     (else #f)))))
         ((vector-set! $$vector-set!)
          (and (not (null? args))
               (not (null? (cdr args)))
               (not (null? (cddr args)))
               (null? (cdddr args))
               (begin
                 (c2push-expr* (caddr args))
                 (c2push-expr* (car args))
                 (c2get-expr* (cadr args))
                 (wt '(vector-set!))
                 (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
                 (cmp:exit) #t)))
         (else #f))))

;; c1body, (c1begin body), bodyは式のリスト
;; c1expr, (c1begin args), (begin ...)
(define (c1begin forms)
  (cond ((end? forms)
         c1begin-empty-default) 
        ((end? (cdr forms))
         (c1expr (car forms))) ;; 式が一つだけ
        (else
         #;(list 'begin (c1map c1expr forms))
         `(begin ,@(c1map c1expr forms))
         #;(list c2begin (c1map c1expr forms))))) ;;複数の式があるとき

(define (c2begin forms)
  (do ((l forms (cdr l)))
      ((null? (cdr l))
       (c2expr (car l))) ;; 最後の式の評価結果を返す
    (dlet ((*value-to-go* 'TRASH)
           (*exit* 'NEXT)
           (*exit-label* (next-label))) ;; ((+ *last-label* 1) . #f)
          (c2expr (car l))
          (wt-label *exit-label*))))

;; lambda式の本体部分を解析する
;; ローカルdefineがあればdefsに追加し, c1bodyで再帰的に解析
;; ローカルdefineがあればc1letrecで処理
;; ローカルdefineがなければc1beginで処理
;; c1lam, 引数:bodyはlambda式の本体, defsはnull
;; c1letrec, (c1body (cdr args) '())
(define (c1body body defs)
  (if (end? body)
      (if (not (null? defs))
          (c1letrec (list (reverse defs))) ;; ローカルdefineしかない
          c1begin-empty-default) ;; 実行する本体がない
      (let ((form (car body))) 
        (cond ((and (pair? form) (eq? (car form) 'define)) ;; ローカルdefineのとき
               (if (or (end? (cdr form)) 
                       (end? (cddr form)) ) 
                                        ;                       (not (end? (cdddr form))))
                   (scbad-args 'define (cdr form)))
               (c1body (cdr body) ;; ローカルdefineをdefsに追加し, 残りの本体を再帰的に解析
                       (cons (if (pair? (cadr form)) ;; 省略記法を使ったローカル関数定義の場合
                                 `(,(caadr form) ;; 省略記法を展開した形に変更
                                   (lambda ,(cdadr form) ,@(cddr form))) ;; (define (f ...) ...) -> (f (lambda (...) ...))
                                 (cdr form)) ;; (f (lambda (...) ...)) ;; 要するにletrecと同じカタチにしている
                             defs))) ;; defsにdefineを取り除いた残りの部分をcons
              ((null? defs) ;; ローカルdefineがないとき
               (c1begin body)) ;;
              (else ;; ローカルdefineがあるとき
               (c1letrec (cons (reverse defs) body)))))))

;;; SCEXIT  Exit Manager.

;; c2gvref, c2constant
(define *value-to-go* 'BX)
(define *exit* 'XRETURN)
(define *exit-label* '())
(define *unwind-sp* 0) ;; 巻き戻すスタックポインタ?

;; c2gvref
;; c2delay
(define (cmp:exit)
  (case *exit*
    ((ESCAPE) (wt `(jmp ,(touch-label *exit-label*)))) ;; *bc-codep*を(`(jmp ,(cdr *exit-label*))に書き換え
    ((UNWIND-NEXT) (wt-reset-sp *unwind-sp*)) ;; *unwind-sp*!=*sp*なら*bc-codep*を`(reset-sp ,(- *unwind-sp* *sp*))に書き換え
    ((UNWIND-ESCAPE) (wt-reset-sp *unwind-sp*) ;; *unwind-sp*!=*sp*なら*bc-codep*を`(reset-sp ,(- *unwind-sp* *sp*))に書き換え
     (wt `(jmp ,(touch-label *exit-label*)))) ;; *bc-codep*を(`(jmp ,(cdr *exit-label*))に書き換え
    ((XRETURN) (wt '(xreturn))) ;; *exit*の初期値, *bc-codep*を('(xreturn))に書き換え
    ((LRETURN) (wt '(lreturn))))) ;; *bc-codep*を('(lreturn))に書き換え

;; c2and
;; c2or
(define (exit-always)
  (case *exit*
    ((NEXT ESCAPE) (wt `(jmp ,(touch-label *exit-label*))))
    ((UNWIND-NEXT UNWIND-ESCAPE) (wt-reset-sp *unwind-sp*)
     (wt `(jmp ,(touch-label *exit-label*))))
    ((XRETURN) (wt '(xreturn)))
    ((LRETURN) (wt '(lreturn)))))

;;; SCFUNC  Lambda Expression.

;; c1expr, (c1lambda args), ((...) ...)?
;; c1letrec, (c1lambda (cdr form)), ((...) ...)
(define (c1lambda args)
  (dlet ((*env* (cons 'CB *env*))) 
        #;(cons c2lambda (cons (cons 'lambda args) (c1lam args)))
        (cons 'lambda (c1lam args))))

;; c1expr, (c1delay args), (...)
(define (c1delay args)
  (dlet ((*env* (cons 'CB *env*)))
        (cons c2delay (cddr (c1lam (cons '() args)))))) ;; lambda式の無引数版

;; lambda式の引数部分と本体部分を解析したリストを返す, ((情報変数 ...) () ...), (() 情報変数 ...)
;; (lambda (...) ...) -> ((解析したlambda式の引数のリスト) () (本体の解析結果))
;; (lambda x ...) -> (() (解析したx) (本体の解析結果))
;; compile-function, (let ((x v) ...) ...)
;; c1lambda, (c1lam args), 引数はlambda式の引数((...) ...)
;; c1delay, (c1lam (cons '() args)), (() ...) lambda式の無引数版
;; c1named-let, (c1lam arg-body), ((vars) body)
;; c1letrec, (c1lambda (cdr form))
#;(define (c1lam lambda-expr)
(if (end? lambda-expr) (scbad-args 'lambda lambda-expr)) 
(let ((requireds '()) ;; 情報変数のリスト
(rest '())) ;; 任意引数の場合はその記号の情報変数となる
(dlet ((*env* *env*))
(do ((vl (car lambda-expr) (cdr vl))) ;; vlはlambdaの仮引数のリスト
((not (pair? vl)) ;; vlがnullまたは記号単体
(if (not (null? vl)) ;; 記号単体
(let ((var (make-var vl)))
(set! *env* (cons var *env*)) ;; *env*に作成したリストを追加する
(set! rest var))) ;; restはvlが記号のリストではなかった場合のみ記号vlの情報を格納したリストに書き換わる
(list (reverse requireds) 
rest ;; (not (null? vl))が偽ならnull, 真なら記号vlの情報を格納したリスト
(c1body (cdr lambda-expr) '()))) ;; lambda式の本体部分を解析
(let ((var (make-var (car vl))))
(set! requireds (cons var requireds)) ;; 作成した情報変数をrequiredsに追加する
(set! *env* (cons var *env*))))))) ;; 作成した情報変数を*env*に追加する

(define (c1lam lambda-expr)
  (if (end? lambda-expr) (scbad-args 'lambda lambda-expr)) 
  (let ((requireds '()) ;; 情報変数のリスト
        (rest '())) ;; 任意引数の場合はその記号の情報変数となる
    (dlet ((*env* *env*))
          (do ((vl (car lambda-expr) (cdr vl))) ;; vlはlambdaの仮引数のリスト
              ((not (pair? vl)) ;; vlがnullまたは記号単体
               (if (not (null? vl)) ;; 記号単体
                   (let ((var (make-var vl)))
                     (set! *env* (cons var *env*)) ;; *env*に作成したリストを追加する
                     (list var (c1body (cdr lambda-expr) '())))
                   (list (reverse requireds) 
                         (c1body (cdr lambda-expr) '())))) ;; lambda式の本体部分を解析
            (let ((var (make-var (car vl))))
              (set! requireds (cons var requireds)) ;; 作成した情報変数をrequiredsに追加する
              (set! *env* (cons var *env*)))))))

(define (make-arg-info requireds rest)
  (if (null? rest)
      (* (+ requireds 1) 4)
      (+ (* (+ requireds 1) 8) 3)))

;; たぶんc2exprで実行されるんだと思う
;; (c2lambda (lambda params body) (情報変数 ...) () body)
;; (c2lambda (lambda params body) () 情報変数 body)
(define (c2lambda lam requireds rest body)
  (if (zero? *re-compile-level*) (set! lam '())) ;;;;; By A243  Dec, 1992 ;;;;
  (if (not (eq? *value-to-go* 'TRASH))
      (let ((label (next-label2))
            (arg-info (make-arg-info (length requireds) rest)))
        (cond ((null? *clink*)
               ;; for DOS
                                        ;(set! *cclosures*
                                        ;      (cons (list label *ccb-vs* requireds rest body)
                                        ;            *cclosures*))
               ;; for UNIX
               (set! *cclosures*
                     (cons (list c2local-lam label requireds rest body)
                           *cclosures*))
               ;;
               (wt `(make-nil-closure ,arg-info ,(touch-label label) ,lam)))
              (else 
               ;; for DOS
                                        ;(set! *cclosures*
                                        ;      (cons (list label *ccb-vs* requireds rest body)
                                        ;            *cclosures*))
               ;; for UNIX
               (set! *cclosures*
                     (cons (list c2cclosure label *ccb-vs* requireds rest body)
                           *cclosures*))
               ;;
               (wt (if (= (car *clink*) *level*)
                       `(get-lval0 ,(- (cdr *clink*) *bp*))
                       `(get-lval ,*bp* ,(- *level* (car *clink*))
                                  ,(cdr *clink*))))
               (wt `(make-closure ,arg-info ,(touch-label label) ,lam))))
        (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))))
  (cmp:exit))

;; たぶんc2exprで実行される  
(define (c2delay body)
  (if (not (eq? *value-to-go* 'TRASH))
      (let ((label (next-label2)))
        (set! *cclosures*
              ;; for DOS
                                        ;(cons (list label *ccb-vs* '() '() body)
                                        ;      *cclosures*)
              ;; for UNIX
              (cons (list c2cclosure label *ccb-vs* '() '() body)
                    *cclosures*)
              ;;
              )
        (wt (cond ((null? *clink*) '(get-const ()))
                  ((= (car *clink*) *level*)
                   `(get-lval0 ,(- (cdr *clink*) *bp*)))
                  (else `(get-lval ,*bp* ,(- *level* (car *clink*))
                                   ,(cdr *clink*)))))
        (wt `(make-promise ,(touch-label label)))
        (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))))
  (cmp:exit))

;; compile-function
(define (c2lam label requireds rest body)
  (wt-label label)
  (dlet ((*cenv-length* 0)
         (*cenv-loc* '())
         (*clink* '())
         (*ccb-vs* 0)
         (*level* 0)
         (*bp* 0)
         (*sp* -1))
        (for-each c2lambda-bind requireds)
        (if (not (null? rest))
            (c2lambda-bind rest))
        (dlet ((*value-to-go* 'BX)
               (*exit* 'XRETURN))
              (c2expr body))))

(define (c2local-lam label requireds rest body)
  (let ((magic (begin-local-function)))
    (wt-label label)
    (dlet ((*cenv-length* 0) (*cenv-loc* '()) (*clink* '()) (*ccb-vs* 0)
           (*level* 0) (*bp* 0) (*sp* -1))
          (for-each c2lambda-bind requireds)
          (if (not (null? rest)) (c2lambda-bind rest))
          (dlet ((*value-to-go* 'BX) (*exit* 'XRETURN)) (c2expr body)))
    (end-local-function magic)))

(define (c2cclosure label cenv-length requireds rest body)
  (dlet ((*cenv-length* cenv-length))
        (let ((magic (begin-local-function)))
          (wt-label label)
          (dlet ((*cenv-loc* (- 0 (length requireds) (if (null? rest) 2 3))))
                (dlet ((*clink* (cons 0 *cenv-loc*)) (*ccb-vs* *cenv-length*)
                       (*level* 0) (*bp* 0) (*sp* -1))
                      (for-each c2lambda-bind requireds)
                      (if (not (null? rest)) (c2lambda-bind rest))
                      (set! *sp* (- *sp* 1))
                      (dlet ((*value-to-go* 'BX) (*exit* 'XRETURN)) (c2expr body))))
          (end-local-function magic))))     

(define (c2local-fun label cenv-length cenv-loc clink ccb-vs level
                     requireds rest body)
  (dlet ((*cenv-length* cenv-length) (*cenv-loc* cenv-loc) (*clink* clink)
         (*ccb-vs* ccb-vs) (*level* level))
        (let ((magic (begin-local-function)))
          (wt-label label)
          (dlet ((*bp* 0) (*sp* 0))
                (for-each c2lambda-bind requireds)
                (if (not (null? rest))
                    (begin (wt `(make-rest ,(- *sp* 1))) (c2lambda-bind rest)))
                (dlet ((*value-to-go* 'BX) (*exit* 'LRETURN)) (c2expr body)))
          (end-local-function magic))))

;;; SCLETS  Let, Let*, and Letrec.

;; nameが特殊形式の識別子ではない場合にリストを作成する
(define (make-var name)
  (if (symbol? name)
      (if (member name '(if cond case and or begin lambda delay let let* 
                            letrec do set! quote quasiquote define macro))
          (scerror "Cannot bind the keyword ~s." name) ;; 特殊形式と同名なら束縛不可能とする
          (list name #f #f #f #f '() '())) ;; nameの情報, 多分リストの残りに変数nameに関連する情報を保持する
      (scerror "~s is not a symbol." name))) ;; 記号ではなかった場合

;; 変数情報を格納したリストから必要な情報を取り出すインターフェース
;; (list name #f #f #f #f '() '()))
;; c1vref, (var-name var)
(define (var-name var) (car var)) ;; 名前
(define (var-funarg var) (cadr var)) ;; 関数ポジション以外で評価されるか
(define (var-assigned var) (caddr var)) ;; 代入されるか
(define (var-closed var) (cadddr var)) ;; lambdaに閉じ込められるか
(define (var-local-fun var) (car (cddddr var))) ;; ローカル関数かどうか
(define (var-local-fun-args var) (cadr (cddddr var))) ;; ローカル関数引数と本体
(define (var-loc var) (caddr (cddddr var))) ;; 変数の位置

(define (set-var-name var x) (set-car! var x))
;; (list name ここを変更 #f #f #f '() '()))
(define (set-var-funarg var x) (set-car! (cdr var) x))
(define (set-var-assigned var x) (set-car! (cddr var) x))
;; (list name #f #f ここを変更 #f '() '()))
(define (set-var-closed var x) (set-car! (cdddr var) x))
(define (set-var-local-fun var x) (set-car! (cddddr var) x))
;; (list name #f #f #f #f ここを変更 '()))
(define (set-var-local-fun-args var x) (set-car! (cdr (cddddr var)) x))
;; 局所変数の場所?
(define (set-var-loc var x) (set-car! (cddr (cddddr var)) x))

(define *env* '()) ;; make-varで作るリスト(list name #f #f #f #f '() '()))のリスト

;; c1expr, (c1let args)
;;         (let name ((x v) ...) ...), (name ((x v) ...) ...)
;;         (let ((x v) ...) ...), (((x v) ...) ...)
;; c1lambda-fun,
;;               (((param arg) ...) exps)
(define (c1let args)
  (if (end? args) (scbad-args 'let args))
  (if (symbol? (car args)) ;; 名前付きletか
      (if (end? (cdr args))
          (scbad-args 'let args)
          (c1named-let (car args) (cadr args) (cddr args))) ;; 名前, 束縛, 本体
      (let ((body '())
            (defs '()))
        ;; 束縛変数を情報変数に拡張, 初期値とともにdefsに追加, 情報変数を*env*に追加し, 本体を評価
        (dlet ((*env* *env*)) ;; *env*を退避
              (dolist (def (car args)) ;; 束縛部分
                      (if (or (end? def) (end? (cdr def)) (not (end? (cddr def))))
                          (scbad-binding def))
                      (let ((var (make-var (car def)))) ;; 情報変数を作成
                        (set! defs (cons (cons var (cadr def)) defs)) ;; (情報変数 . 初期値)をdefsに追加
                        #;(set! defs (cons (list var (cadr def)) defs)) ;; c.scm
                        (set! *env* (cons var *env*))))
              (set! body (c1body (cdr args) '())))
        ;; 束縛する初期値を初期値の解析結果に置き換える
        (dolist (def defs) ;; def -> (var . form)
                (let ((var (car def))
                      (form (cdr def)))
                  (if (and (pair? form) (eq? (car form) 'LAMBDA)) ;; 初期値がlambda式
                      (if (or (var-funarg var) (var-assigned var) (var-closed var))
                          (set-cdr! def (c1lambda (cdr form)))
                          (begin (set-cdr! def (c1lam (cdr form)))
                                 (set-var-local-fun var #t)
                                 (set-var-local-fun-args var (cdr def))))
                      (begin (set-cdr! def (c1expr form)) ;; 初期値がlambda式以外ならc1exprで解析した結果に置き換え
                             #;(print "c.scm:debug " (c1expr form)))))) ;; debug
        (list #;c2let 'let (reverse defs) body))))

;; let*式を解析する
;; c1expr, (c1let* args)
(define (c1let* args)
  (if (end? args) (scbad-args 'let* args))
  (let ((body '()) (defs '()))
    (dlet ((*env* *env*)) ;; *env*を退避
          ;; 束縛変数を情報変数に拡張, 情報変数と初期値の対をdefsに追加, 情報変数を*env*に追加, 本体を評価
          (dolist (def (car args)) ;; 束縛部分
                  (if (or (end? def) (end? (cdr def)) (not (end? (cddr def))))
                      (scbad-binding def))
                  (let ((var (make-var (car def)))) ;; 情報変数を作成
                    (set! defs (cons (cons var (cadr def)) defs)) ;; 作成した情報変数と値の対をdefsに追加
                    #;(set! defs (cons (list var (cadr def)) defs))
                    (set! *env* (cons var *env*)))) ;; *env*に情報変数を追加
          (set! body (c1body (cdr args) '())) ;; 本体を解析
          ;; defsには束縛が逆順に入っている
          ;; let*は入れ子のlet式と等価
          ;; 自分の変数スコープを除いた環境で初期値を評価する
          (dolist (def defs) ;; def -> (var . form)
                  (set! *env* (cdr *env*)) ;; 自分の変数スコープは取り除く
                  (let ((var (car def)) ;; 情報変数
                        (form (cdr def))) ;; 初期値
                    (if (and (pair? form) (eq? (car form) 'LAMBDA)) ;; 初期値がlambda式ならlambda式の解析結果をdefの初期値に
                        (if (or (var-funarg var) (var-assigned var) (var-closed var))
                            (set-cdr! def (c1lambda (cdr form)))
                            (begin (set-cdr! def (c1lam (cdr form)))
                                   (set-var-local-fun var #t)
                                   (set-var-local-fun-args var (cdr def))))
                        (set-cdr! def (c1expr form)))))) ;; 初期値がlambda式以外ならc1exprで解析した結果に置き換える
    (list #;c2let* 'let* (reverse defs) body))) 

;; c1body, (c1letrec (list (reverse defs))), ローカルdefine:(f (lambda (...) ...))のリスト
;;         (c1letrec (cons (reverse defs) body)), (ローカルdefineのリスト 本体)
;; c1expr, (c1letrec args), 
(define (c1letrec args)
  (if (end? args) (scbad-args 'letrec args))
  (let ((body '())
        (defs '()))
    (dlet ((*env* *env*)) ;; macrolib.lsp
          ;; letrecで束縛されるすべてのローカル変数を情報変数varにし, (var '() 初期値)をdefsに追加, 情報変数varを*env*に追加
          ;; 拡張した環境*env*の元でletrec式の本体を解析
          (dolist (def (car args)) ;; macrolib.lsp, def:ローカルdefineの１つ                  
                  (if (or (end? def)
                          (end? (cdr def))
                          (not (end? (cddr def))))
                      (scbad-binding def)) 
                  (let ((var (make-var (car def)))) ;; ローカルdefineで束縛される変数の情報変数を作成
                    (set! defs (cons (list var '() (cadr def)) defs)) ;; (cadr def) -> (lambda (...) ...) 
                    (set! *env* (cons var *env*)))) ;; 情報変数を*env*に追加
          (set! body (c1body (cdr args) '()))
          ;; letrec式で束縛されるローカル変数の各初期値を上で拡張済みの環境*env*で初期値を解析
          (dolist (def defs) ;; def -> (var '() (lambda (...) ...))
                  (let ((var (car def)) ;; var
                        (form (caddr def))) ;; (lambda (...) ...)
                    (if (and (pair? form) (eq? (car form) 'lambda #;'LAMBDA)) ;; 初期値がlambda式, r7rsでは不等価
                        (if (or (var-funarg var) (var-assigned var) (var-closed var)) 
                            (set-car! (cdr def) (c1lambda (cdr form))) ;; lambda式の解析結果 (var ここに格納 (lambda (...) ...))
                            (begin (set-car! (cdr def) `(lambda ,@(c1lam (cdr form)))) ;; c.scm
                                   #;(set-car! (cdr def) (c1lam (cdr form))) ;; lambda式の解析結果 (var ここに格納 (lambda (...) ...))
                                   (set-var-local-fun var #t) ;; 局所関数, letrecで定義されるから
                                   (set-var-local-fun-args var (cadr def)))) ;; lambda式の解析結果を変数情報に格納
                        (set-car! (cdr def) (c1expr form))))) ;; 初期値の解析結果を格納
          (c1letrec-aux defs))
    ;; c.scm: defsからlambda式を取り除く
    (let loop ((defs defs))
      (if (null? defs)
          '()
          (let ((var (caar defs))
                (form (cadar defs)))
            (begin (set-car! defs (list var form))
                   (loop (cdr defs))))))
    (list #;c2letrec 'letrec (reverse defs) body)))



;; 初期値を解析した結果、funarg | assigned | closedな使われ方があったため変更
;; c1letrec, (c1letrec-aux defs), defs:(var ... (lambda (...) ...))のリスト
(define (c1letrec-aux defs)
  (define (parse defs) 
    (if (null? defs)
        #f
        (let* ((def (car defs)) ;; (var 初期値の解析結果 初期値)
               (var (car def))) ;; 情報変数
          (if (and (var-local-fun var)
                   (or (var-funarg var) (var-assigned var) (var-closed var)))
              (begin (set-var-local-fun var #f) ;; 
                     (set-car! (cdr def) (c1lambda (cdaddr def))) ;; lambda式の引数と本体の解析結果
                     (parse (cdr defs))
                     #t)
              (parse (cdr defs))))))
  (if (parse defs) (c1letrec-aux defs)))

;; c1let, (c1named-let (car args) (cadr args) (cddr args)), 名前 束縛 本体
;; c1do
(define (c1named-let name bind body)
  (let ((var (make-var name)) ;; nameの情報変数を作成
        (inits (c1map (lambda (x) ;; 初期値を解析したリスト
                        (if (or (end? x) (end? (cdr x)) (not (end? (cddr x))))
                            (scbad-binding x))
                        (c1expr (cadr x))) 
                      bind)) 
        (vars (c1map car bind))) ;; 束縛変数のリスト
    (dlet ((*env* (cons var *env*))) ;; *env*を退避, 名前付きletの名前を追加した*env*を使う
          (let* ((arg-body (cons vars body)) ;; lambda式と同じ形式を作る  (params body)
                 (fun (c1lam arg-body))) 
            (cond ((or (var-funarg var) (var-assigned var) (var-closed var))
                   (list #;c2named-let 'let var inits (c1lambda arg-body)))
                  (else (set-var-local-fun var #t)
                        (set-var-local-fun-args var (list vars #f))
                        (list #;c2named-let 'let var inits fun)))))))

;; c1expr, (c1do args)
;;         (do ((var init) ...) (test tail-sequence) body ...)
(define (c1do args)
  (if (or (end? args) (end? (cdr args)) (end? (cadr args)))
      (scbad-args 'do args))
  (let ((name (gensym))) ;; doループを名前付きletとして処理
                                        ;    (c1named-let name (car args)
    ;; 名前, 束縛(car args), 本体
    (c1named-let name
                 (c1map (lambda (x) (list (car x) (cadr x))) (car args)) ;; ((変数 初期値) ...)
                 `((if ,(caadr args) ;; 終了条件
                       (begin ,@(cdadr args)) ;; tail-sequenceを実行
                       (begin ,@(cddr args) ;; body ...
                              ,(cons name ;; 再帰呼び出しを行う
                                     (c1map (lambda (ite)
                                              (cond ((or (end? ite) (end? (cdr ite)))
                                                     (scbad-binding ite))
                                                    ((end? (cddr ite)) ;; 更新式なし
                                                     (car ite))
                                                    ((end? (cdddr ite)) ;; 更新あり
                                                     (caddr ite))
                                                    (else (scbad-binding ite))))
                                            (car args)))))))))

(define (c2let defs body)
  (dlet ((*sp* *sp*))
        (dolist (def defs)
                (let ((var (car def))
                      (form (cdr def)))
                  (cond ((var-local-fun var)
                         (set-var-local-fun var (+ *level* 1))
                         (set-var-loc var (next-label2))
                         (set! *local-funs*
                               (cons (list* (var-loc var) *cenv-length* *cenv-loc*
                                            *clink* *ccb-vs* (+ *level* 1) form)
                                     *local-funs*)))
                        (else (c2push-expr* form))))))
  (dlet ((*sp* *sp*)
         (*clink* *clink*)
         (*ccb-vs* *ccb-vs*))
        (let ((old-sp *sp*)
              (unwindp #f))
          (dolist (def defs)
                  (if (not (var-local-fun (car def)))
                      (begin (c2lambda-bind (car def))
                             (set! unwindp #t))))
          (c2let-body unwindp old-sp body))))

(define (c2let* defs body)
  (dlet ((*sp* *sp*) (*clink* *clink*) (*ccb-vs* *ccb-vs*))
        (let ((old-sp *sp*) (unwindp #f))
          (dolist (def defs)
                  (let ((var (car def)) (form (cdr def)))
                    (cond ((var-local-fun var)
                           (set-var-local-fun var (+ *level* 1))
                           (set-var-loc var (next-label2))
                           (set! *local-funs*
                                 (cons (list* (var-loc var) *cenv-length* *cenv-loc*
                                              *clink* *ccb-vs* (+ *level* 1) form)
                                       *local-funs*)))
                          (else (set! unwindp #t)
                                (c2push-expr* form)
                                (c2bind var)))))
          (c2let-body unwindp old-sp body))))

;; defs:((ローカル変数 解析結果 初期値) ...)
;; body:letrec式本体の解析結果
(define (c2letrec defs body)
  (dlet ((*clink* *clink*)
         (*ccb-vs* *ccb-vs*))
        (dlet ((*sp* *sp*))
              (let ((old-sp *sp*)
                    (unwindp #f))
                (dolist (def defs)
                        (if (not (var-local-fun (car def)))
                            (begin (wt '(push-const ())) (set! *sp* (- *sp* 1))
                                   (c2bind (car def)))))
                (dolist (def defs)
                        (let ((var (car def))
                              (form (cadr def)))
                          (cond ((var-local-fun var)
                                 (set-var-local-fun var (+ *level* 1))
                                 (set-var-loc var (next-label2))
                                 (set! *local-funs*
                                       (cons (list* (var-loc var)
                                                    *cenv-length* *cenv-loc* *clink*
                                                    *ccb-vs* (+ *level* 1) form)
                                             *local-funs*)))
                                (else (set! unwindp #t)
                                      (c2get-expr* form)
                                      (wt `(,(if (or (var-assigned var) (var-closed var))
                                                 'set-hval0 'set-lval0)
                                            ,(- (cdr (var-loc var)) *bp*)))))))
                (c2let-body unwindp old-sp body)))))

(define (c2let-body unwindp old-sp body)
  (if unwindp
      (case *exit*
        ((NEXT)
         (cond ((eq? *value-to-go* 'PUSH)
                (c2get-expr* body)
                (wt-reset-sp old-sp)
                (wt '(push-bx)))
               (else (dlet ((*exit* 'UNWIND-NEXT) (*unwind-sp* old-sp))
                           (c2expr body)))))
        ((ESCAPE)
         (cond ((eq? *value-to-go* 'PUSH)
                (c2get-expr* body)
                (wt-reset-sp old-sp)
                (wt '(push-bx))
                (cmp:exit))
               (else (dlet ((*exit* 'UNWIND-ESCAPE) (*unwind-sp* old-sp))
                           (c2expr body)))))
        (else (c2expr body)))
      (c2expr body)))

(define (c2named-let var inits form)
  (let ((vref (list c2vref var #f)))
    (cond ((var-local-fun var)
           (set-var-local-fun var (+ *level* 1))
           (set-var-loc var (next-label2))
           (set! *local-funs*
                 (cons (list* (var-loc var) *cenv-length* *cenv-loc* *clink*
                              *ccb-vs* (+ *level* 1) form)
                       *local-funs*))
           (c2funcall vref inits))
          (else
           (dlet ((*sp* *sp*) (*clink* *clink*) (*ccb-vs* *ccb-vs*))
                 (let ((old-sp *sp*))
                   (wt '(push-const ())) (set! *sp* (- *sp* 1))
                   (c2bind var)
                   (c2get-expr* form)
                   (wt `(set-hval0 ,(- (cdr (var-loc var)) *bp*)))
                   (c2let-body #t old-sp (list c2funcall vref inits))))))))

;;; SCLOAD  The Loader

(define *open-frame-pushes* 5)
(define *open-lframe-pushes* 4)
(define *last-label* 0)

;; *last-label*をインクリメントし, (*last-label* . L*last-label*)を返す
;; compile-function
;; c2and
;; c2or
(define (next-label2)
  (set! *last-label* (+ *last-label* 1))
  (cons *last-label*
        (string->symbol
         (string-append "L" (number->string *last-label* 10))))) ;; 10は基数

(define (next-label)
  (set! *last-label* (+ *last-label* 1))
  (cons *last-label* #f))

;; 呼び出し元:cmp:exit, (touch-label *exit-label*)
;; c2and, (touch-label label), (cdr label)->L*last-label*
;; c2or, (touch-label label), (cdr label)->L*last-label*
(define (touch-label label)
  (or (cdr label)
      (begin (set-cdr! label 
                       (string->symbol
                        (string-append "L"
                                       (number->string (car label) 10))))
             (cdr label))))

(define (begin-local-function)
  *bc-codep*)

(define (end-local-function saved-codep)
  (set-cdr! saved-codep 
            (list (cons 'local-function (cdr saved-codep))))
  (set! *bc-codep* (cdr saved-codep)))

;; c2lam, (wt-labe label), (cdr label)は文字列
(define (wt-label label)
  (if (cdr label) (wt (cdr label))))

;; cmp:exit, (wt-reset-sp *unwind-sp*) *unwind-sp*は数値
(define (wt-reset-sp sp)
  (if (not (= sp *sp*))
      (wt `(reset-sp ,(- sp *sp*))))) ;; *bc-codep*を`(reset-sp ,(- sp *sp*))に書き換え

;; c2gvref, (wt `(get-gval ,name)), case式のどれも実行されない...
;; cmp:exit, (wt `(jmp ,(touch-label *exit-label*)), case式のどれも実行されない...
;;           (wt '(xreturn))
;;           (wt '(lreturn))
;; wt-reset-sp, (wt `(reset-sp ,(- sp *sp*)))
;; wt-label, (wt (cdr label))
(define (wt code)
  (set-cdr! *bc-codep* (list code)) ;; *bc-codep*は初期値はnullなのでset-cdr!はできない
  (set! *bc-codep* (cdr *bc-codep*)) ;; つまり((list code)), (set! *bc-codep* (list code))じゃだめ???
  (if (pair? code)
      (case (car code)
        ((open-frame)
         (set! *sp* (- *sp* *open-frame-pushes*))
         (set! *bp* *sp*))
        ((open-lframe open-lframe-1)
         (set! *sp* (- *sp* *open-lframe-pushes*))
         (set! *bp* *sp*))
        ((make-cons eq? eqv? equal? + * - / < > <= >= =
                    set-car! set-cdr! vector-ref)
         (set! *sp* (+ *sp* 1)))
        ((vector-set!)
         (set! *sp* (+ *sp* 2))))))

;; 
(define (wt-begin)
  (set! *bc-code* (list ()))
  (set! *bc-codep* *bc-code*))

(define (wt-end-function first func-name args)
  `(,first ,func-name 
           (COMPILED-FUNCTION ',func-name 
                              ,(do ((x (cdr args) (cdr x)) ;; (cdr args) -> 関数の本体
                                    (n 0 (+ n 1)))
                                   ((not (pair? x))
                                    (make-arg-info n x)))
                              ',(cdr *bc-code*))))

(define (wt-end-expr)
  `(LOAD-TIME-EVAL ',(cdr *bc-code*)))

;;; SCMISC  Miscellaneous Special Forms.

;; c1expr, (c1quote args)
(define (c1quote args)
  (if (or (end? args) (not (end? (cdr args)))) (scbad-args 'quote args))
  (case (car args)
    ((#f) c1false)
    ((#t) c1true)
    ((()) c1null)
    (else (c1constant (car args)))))

                                        ;(define (c1quasiquote args)
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
                                        ;    (c1expr (if (eq? xx (car args)) (qqquote xx) xx))))

;;; Quasiquote by T.Komiya

(let () (append))

;; c1expr, (c1quasiquote args)
;; `(1 ,a ,@b) -> (quasiquote (1 (unquote a) (unquote-splicing b))
(define (c1quasiquote args)
  (define (qq x level)
    (cond ((vector? x)
           `(list->vector ,(qq (vector->list x) level)))
          ((not (pair? x))
           (if (symbol? x)
               `',x 
               x)) ;; 文字列, 文字, 数値, 真偽値
          ((eq? (car x) 'quasiquote)
           `($$cons ,''quasiquote ,(qq (cdr x) (1+ level)))) ;; 入れ子の準クオート
          ((eq? (car x) 'unquote) ;; x -> (unquote ...)
           (let ((xcdr (cdr x))) ;; (...)
             (if (or (not (pair? xcdr)) 
                     (not (null? (cdr xcdr))))
                 (scbad-args 'unquote xcdr) 
                 (if (zero? level)
                     (car xcdr) ;; ...を評価
                     `($$cons ,''unquote ,(qq xcdr (1- level))))))) ;; 入れ子のレベルを一つ下げる
          ((and (pair? (car x))
                (eq? (caar x) 'unquote-splicing)) ;; x -> (unquote-splicing ...)
           (let ((xcar (car x)) ;; unquote-splicing
                 (xcdr (cdr x))) ;; (...)
             (if (or (not (pair? (cdr xcar)))
                     (not (null? (cddr xcar))))
                 (scbad-args 'unquote-splicing xcar))
             (if (zero? level)
                 `($$append ,(cadr xcar) ,(qq xcdr level)) 
                 `($$cons ($$cons ,''unquote-splicing ,(qq (cdr xcar) (1- level))) ;; 入れ子のレベルを一つ下げる
                          ,(qq xcdr level))))) ;; 
          (else
           `($$cons ,(qq (car x) level) ;; 
                    ,(qq (cdr x) level))))) 
  (if (or (end? args) (not (end? (cdr args)))) (scbad-args 'quasiquote args))
  (c1expr (qq (car args) 0)))

(define (c1define args)
  (scerror "Bad use of DEFINE."))

(define (c1macro args)
  (scerror "Bad use of MACRO."))

;;; SCVREF  Variable References.
;; c1expr, (c1vref form)
(define (c1vref name)
  (let lookup ((env *env*) 
               (ccb #f))
    (if (null? env)
        #;(list c2gvref name)
        name 
        (let ((var (car env)))
          (cond ((eq? var 'CB) 
                 (lookup (cdr env) #t)) ;; ccbはここで書き換え
                ((eq? (var-name var) name) 
                 (if ccb (set-var-closed var #t)) ;; (list name #f #f #t #f '() '()))
                 (set-var-funarg var #t) ;; (list name #t #f #f #f '() '()))
                 #;(list c2vref var ccb) var)
                (else (lookup (cdr env) ccb)))))))

(define (c1lookup name)
  (let lookup ((env *env*))
    (cond ((null? env) #f)
          ((and (not (eq? (car env) 'CB))
                (eq? (var-name (car env)) name))
           #t)
          (else (lookup (cdr env))))))

;; ローカル変数の参照, 変更
;; c1vref, (list c2vref var ccb)          
(define (c2vref var ccb)
  (if (not (eq? *value-to-go* 'TRASH)) ;; TRASHになるのはc2beginでだけ
      (cond (ccb 
             (let ((n (- *cenv-length* (var-closed var) 1)))
               (if (zero? *level*)
                   (case *value-to-go*
                     ((BX) (wt `(get-cval0 ,(- *cenv-loc* *bp*) ,n)))
                     ((PUSH) (wt `(push-cval0 ,(- *cenv-loc* *bp*) ,n))))
                   (case *value-to-go*
                     ((BX) (wt `(get-cval ,*bp* ,*level* ,*cenv-loc* ,n)))
                     ((PUSH)
                      (wt `(push-cval ,*bp* ,*level* ,*cenv-loc* ,n)))))))
            ((= (car (var-loc var)) *level*)
             (let ((offset (- (cdr (var-loc var)) *bp*)))
               (if (or (var-assigned var) (var-closed var))
                   (case *value-to-go*
                     ((BX) (wt `(get-hval0 ,offset)))
                     ((PUSH) (wt `(push-hval0 ,offset))))
                   (case *value-to-go*
                     ((BX) (wt `(get-lval0 ,offset)))
                     ((PUSH) (wt `(push-lval0 ,offset)))))))
            (else
             (let ((baseoffset *bp*)
                   (level (- *level* (car (var-loc var)))) 
                   (offset (cdr (var-loc var))))
               (if (or (var-assigned var) (var-closed var))
                   (case *value-to-go*
                     ((BX) (wt `(get-hval ,baseoffset ,level ,offset)))
                     ((PUSH) (wt `(push-hval ,baseoffset ,level ,offset))))
                   (case *value-to-go*
                     ((BX) (wt `(get-lval ,baseoffset ,level ,offset)))
                     ((PUSH) (wt `(push-lval ,baseoffset ,level ,offset)))))))))
  (cmp:exit))

;; グローバル変数を参照, 変更
;; c1vref, (list c2gvref name) 
(define (c2gvref name)
  (case *value-to-go* ;; 初期値は'BX
    ((BX) (wt `(get-gval ,name))) ;; *bc-codep*を(`(get-gval ,name))に書き換え
    ((PUSH) (wt `(push-gval ,name)))) ;; *bc-codep*を(`(push-gval ,name))に書き換え
  (cmp:exit))

;; c1expr, (c1set! args)
(define (c1set! args)
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
           (c1vref name))
          (else
           (let lookup ((env *env*) (ccb #f))
             (if (null? env)
                 #;(list c2gset! name (c1expr form))
                 (list 'set! name (c1expr form))
                 (let ((var (car env)))
                   (cond ((eq? var 'CB)
                          (lookup (cdr env) #t))
                         ((eq? (var-name var) name) 
                          (if ccb (set-var-closed var #t)) ;; lambda式の内部
                          (set-var-assigned var #t) ;; set!で代入される
                          #;(list c2set! var ccb (c1expr form)) (list 'set! var (c1expr form)))
                         (else
                          (lookup (cdr env) ccb))))))))))

(define (c2set! var ccb form)
  (c2get-expr* form)
  (cond (ccb
         (let ((offset (- *cenv-loc* *bp*))
               (n (- *cenv-length* (var-closed var) 1)))
           (if (zero? *level*)
               (wt `(set-cval0 ,offset ,n))
               (wt `(set-cval ,*bp* ,*level* ,offset ,n)))))
        ((= (car (var-loc var)) *level*)
         (wt `(set-hval0 ,(- (cdr (var-loc var)) *bp*))))
        (else (wt `(set-hval ,*bp* ,(- *level* (car (var-loc var)))
                             ,(cdr (var-loc var))))))
  (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
  (cmp:exit))

(define (c2gset! name form)
  (c2get-expr* form)
  (wt `(set-gval ,name))
  (if (eq? *value-to-go* 'PUSH) (wt '(push-bx)))
  (cmp:exit))

;; 通常のnull?に対またはnull以外の引数が与えられたときのエラー処理を追加したい?
(define (end? x)
  (cond ((pair? x) #f) 
        ((null? x) #t)
        (else (scerror "~s is not a pair." x))))

;; c1fmla, (c1map c1fmla (cdr fmla))
;; c1named-let
;; c1do
;; c1expand-case
(define (c1map fun l) 
  (if (end? l)
      '()
      (cons (fun (car l)) (c1map fun (cdr l)))))

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
  (format #t "~&Compile error : ")
  (apply format #t s args)
  (format #t "~%")
  ;; for DOS
                                        ;
  ;; for UNIX
  (error-break)
  )


;;;;;;;;;;;;;;;;;;;;;;;;
;;; c.scmプログラム ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define (c.scm:self-eval? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))

(define c.scm:*local-functions* '())

(define (c.scm:compile input)
  (cond ((string? input)
         (c.scm:compile-file input))
        (else
         (dlet ((c.scm:*local-functions* '()))
               (display (c.scm:compile-sexp input) (current-output-port))
               (newline (current-output-port))
               (c.scm:write-local-functions)))))

(define (c.scm:write-local-functions)
  (if (null? c.scm:*local-functions*)
      (newline (current-output-port))
      (let loop ((defs c.scm:*local-functions*))
        (cond ((null? defs)
               (newline (current-output-port)))
              (else
               (let ((def (car defs)))
                 (display `(define ,(var-name (car def)) ,(c.scm:c2expr (cadr def))) (current-output-port)))
               (loop (cdr defs)))))))

(define (c.scm:compile-file input))

(define (c.scm:compile-sexp sexp)
  (if (pair? sexp)
      (case (car sexp)
        ((define)
         (cond((or (null? (cdr sexp)) ;; (define)
                   (null? (cddr sexp))) ;; (define var)
               (error "C.SCM:ERROR, c.scm:compile-sexp, syntax-error:" sexp))
              ((symbol? (cadr sexp)) ;; (define var ...)
               (if (null? (cdddr sexp)) ;; (define var exp)
                   (if (and (pair? (caddr sexp))
                            (eq? (caaddr sexp) 'lambda))
                       `(define ,(cadr sexp) ,(c.scm:compile-function (cdaddr sexp))) ;; (define var (lambda ...))
                       `(define ,(cadr sexp) ,(c.scm:compile-expr (caddr sexp)))) ;; (define var exp)
                   (error "C.SCM:ERROR, c.scm:compile-sexp, syntax-error:" sexp))) ;; (define var exp ...)
              ((pair? (cadr sexp))
               `(define ,(caadr sexp) ,(c.scm:compile-function (cons (cdadr sexp) (cddr sexp))))) ;; (define (var ...) ...)
              (else
               (error "C.SCM:ERROR, c.scm:compile-sexp, syntax-error:" sexp))))
        (else
         (c.scm:compile-expr sexp))) ;; (fun ...)
      (c.scm:compile-expr sexp))) ;; atom

#;(define (c.scm:compile-sexp sexp)
  (match sexp
         (`(define (,x . ,params) . ,body)
          (c.scm:compile-function x params body))
         (`(define ,x (lambda ,params . ,body))          
          (c.scm:compile-function x params body))
         (`(define ,x ,exp)
          `(define ,x ,(c.scm:compile-expr exp)))
         (else
          (print "c.scm:compile, Unknown S expression: " sexp))))

(define (c.scm:compile-expr form)
  (dlet ((*env* '()))
        (c.scm:c2expr
         (c1expr form))))

(define c.scm:*debug-mode* #f)

(define (c.scm:debug . x)
  (cond ((null? x)
         (if c.scm:*debug-mode*
             (print "c.scm: debug mode")
             (print "c.scm: normal mode")))
        ((boolean? (car x))
         (if (car x)
             (begin (set! c.scm:*debug-mode* (car x))
                    (print "c.scm: debug mode"))
             (begin (set! c.scm:*debug-mode* (car x))
                    (print "c.scm: normal mode"))))
        (else
         (print "c.scm:mode: Invalid argument, on #t, off #f" (car x)))))

(define (c.scm:compile-function form)
  (if (or (symbol? (car form))
          (pair? (car form)))
      (let ((x (dlet ((*env* '()))
                     (c1lam form))))
        (if c.scm:*debug-mode*
            x
            `(lambda ,(car form) ,(c.scm:c2expr (c.scm:h (c.scm:c (cadr x)))))))))
            
#;(define (c.scm:compile-function name params body)
  (let ((x (dlet ((*env* '()))
                 (c1lam (cons params body)))))
    (if c.scm:*debug-mode*
        `(define ,name ,x)
        `(define ,name (lambda ,(map var-name (car x))
                         ,(c.scm:c2expr (cadr x)))))))

(define (c.scm:c2expr form)
  (cond ((c.scm:symbol? form)
         (c.scm:c2vref form))
        ((c.scm:pair? form)
         (let ((fun (car form))
               (args (cdr form)))
           (cond ((c.scm:symbol? fun)
                  (case fun
                    ((if) (c.scm:c2if args))
                    ((and) (c.scm:c2and args))
                    ((or) (c.scm:c2or args))
                    ((begin) (c.scm:c2begin args))
                    ((lambda) (c.scm:c2lambda args))
                    ((delay) (c.scm:c2delay args))
                    ((let) (c.scm:c2let args))
                    ((let*) (c.scm:c2let* args))
                    ((letrec) (c.scm:c2letrec args))
                    ((set!) (c.scm:c2set! args))
                    ((quote) (c.scm:c2quote args))
                    (else
                     (c.scm:c2symbol-fun fun args)))))))
        (else
         (case form
           ((#f) #f)
           ((#t) #t)
           ((()) '())
           (else
            form)))))

;; 変数参照
(define (c.scm:c2vref name)
  (cond ((c.scm:var? name)
         (var-name name))
        (else
         name)))

(define (c.scm:c2if args)
  `(if ,(c.scm:c2expr (car args))
       ,(c.scm:c2expr (cadr args))
       ,(c.scm:c2expr (caddr args))))

(define (c.scm:c2and args)
  `(and ,@(c.scm:c2args args)))

(define (c.scm:c2or args)
  `(or ,@(c.scm:c2args args)))

(define (c.scm:c2begin args)
  `(begin ,@(c1map c.scm:c2expr args)))

(define (c.scm:c2lambda form)
  (let ((params (if (c.scm:pair? (car form))
                    (map var-name (car form))
                    (var-name (car form))))
        (body (c.scm:c2expr (cadr form))))
    `(lambda ,params ,body)))

(define (c.scm:c2let form)
  (if (c.scm:var? (car form))
      (c.scm:c2named-let form)
      (let loop ((defs (car form))
                 (cdefs '()))
        (cond ((null? defs)
               `(let ,(reverse cdefs) ,(c.scm:c2expr (cadr form))))
              (else
               (loop (cdr defs)
                     (cons (list (var-name (caar defs)) (c.scm:c2expr (cdar defs))) cdefs)))))))

(define (c.scm:c2let* form)
  (let loop ((defs (car form))
             (cdefs '()))
    (cond ((null? defs)
           `(let* ,(reverse cdefs) ,(c.scm:c2expr (cadr form))))
          (else
           (loop (cdr defs)
                 (cons (list (var-name (caar defs)) (c.scm:c2expr (cdar defs))) cdefs))))))

(define (c.scm:c2letrec x)
  (let loop ((defs (car x))
             (cdefs '()))
    (cond ((null? defs)
           `(letrec ,(reverse cdefs) ,(c.scm:c2expr (cadr x))))
          (else
           (loop (cdr defs)
                 (cons (c.scm:c2def (car defs)) cdefs))))))             
                 
#;(define (c.scm:c2letrec x)
  (let loop ((defs (car x))
             (cdefs '()))
    (cond ((null? defs)
           (if (null? cdefs)
               (c.scm:c2expr (cadr x))
               `(letrec ,(reverse cdefs) ,(c.scm:c2expr (cadr x)))))
          (else
           (let ((var (caar defs)))
             (if (var-local-fun var)
                 (begin
                   (set! c.scm:*codes* (cons `(define ,@(c.scm:c2def (car defs))) c.scm:*codes*))
                   (loop (cdr defs)
                         cdefs))
                 (loop (cdr defs)
                       (cons (c.scm:c2def (car defs)) cdefs))))))))

(define (c.scm:c2def def)
  (list (var-name (car def)) (c.scm:c2expr (cadr def))))

(define (c.scm:c2quote args)
  `(quote ,@args))

(define (c.scm:c2symbol-fun name args)
  (cond ((c.scm:var? name)
         `(,(var-name name) ,@(c.scm:c2args args)))
        (else
         `(,name ,@(c.scm:c2args args)))))

(define (c.scm:c2args forms)
  (if (null? forms)
      '()
      (cons (c.scm:c2expr (car forms))
            (c.scm:c2args (cdr forms)))))

(define (c.scm:symbol? x)
  (or (c.scm:var? x)
      (symbol? x)))

(define (c.scm:pair? x)
  (and (not (c.scm:var? x))
       (pair? x)))

(define (c.scm:var? var)
  (match var
         (`(,name ,(or #f #t) ,(or #f #t) ,(or #f #t) ,(or #f #t) ,local-fun-args ,loc)
          #t)
         (else
          #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; closure conversion ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; f
;; free variables
(define (c.scm:f sexp)
  (cond ((c.scm:var? sexp)
         (list sexp))
        ((or (symbol? sexp)
             (c.scm:self-eval? sexp))
         '())
        ((c.scm:pair? sexp)
         (let ((fun (car sexp))
               (args (cdr sexp)))
           (case fun
                    ((if) (c.scm:f-if args))
                    ((and) (c.scm:f-and args))
                    ((or) (c.scm:f-or args))
                    ((begin) (c.scm:f-begin args))
                    ((lambda) (c.scm:f-lambda args))
                    ((delay) (c.scm:f-delay args))
                    ((let) (c.scm:f-let args))
                    ((let*) (c.scm:f-let args))
                    ((letrec) (c.scm:f-letrec args))
                    ((set!) (c.scm:f-set! args))
                    ((quote) (c.scm:f-quote args))
                    (else
                     (c.scm:f-symbol-fun fun args)))))))

(define (c.scm:f-if args)
  (c.scm:union (c.scm:union (c.scm:f (car args))
                            (c.scm:f (cadr args)))
               (c.scm:f (caddr args))))

(define (c.scm:f-and args)
  (c.scm:union (c.scm:f (car args))
               (c.scm:f-and (cdr args))))

(define (c.scm:f-or args)
  (c.scm:union (c.scm:f (car args))
               (c.scm:f-or (cdr args))))

(define (c.scm:f-begin args)
  (c.scm:union (c.scm:f (car args))
               (c.scm:f-begin (cdr args))))

(define (c.scm:f-lambda args)
  (c.scm:difference (c.scm:f (cadr args))
                    (car args)))

(define (c.scm:f-let args)
  (let ((vars (map car (car args))))
    (c.scm:difference
     (c.scm:union
      (let loop ((exps (map cdr (car args))))
        (cond ((null? exps)
               '())
              (else
               (c.scm:union (c.scm:f (car exps)) (loop (cdr exps))))))
      (c.scm:f (cadr args)))
     vars)))

(define (c.scm:f-letrec x)
  (let ((vars (map car (car x))))
    (c.scm:difference
     (c.scm:union
      (let loop ((exps (map cadr (car x))))
        (cond ((null? exps)
               '())
              (else
               (c.scm:union (c.scm:f (car exps)) (loop (cdr exps))))))
      (c.scm:f (cadr x)))
     vars)))

(define (c.scm:f-set! x)
  (c.scm:union (c.scm:f (car x))
               (c.scm:f (cadr x))))

(define (c.scm:f-quote x)
  '())

(define (c.scm:f-symbol-fun fun args)
  (if (null? args)
      (c.scm:f fun)
      (let loop ((args args))
        (cond ((null? args)
               '())
              (else
               (c.scm:union (c.scm:f (car args))
                            (loop (cdr args))))))))

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

;; c
;; closing
;; lambda式のパラメタに自由変数を追加
;; 関数呼び出しのパラメタに自由変数を追加
(define (c.scm:c sexp)
  (cond ((or (c.scm:var? sexp)
             (c.scm:self-eval? sexp))
         sexp)
        (else
         (let ((fun (car sexp))
               (args (cdr sexp)))
           (case fun
             ((if) (c.scm:c-if args))
             ((and) (c.scm:c-and args))
             ((or) (c.scm:c-or args))
             ((begin) (c.scm:c-begin args))
             ((lambda) (c.scm:c-lambda args))
             ((delay) (c.scm:c-delay args))
             ((let) (c.scm:c-let args))
             ((let*) (c.scm:c-let args))
             ((letrec) (c.scm:c-letrec args))
             ((set!) (c.scm:c-set! args))
             ((quote) (c.scm:c-quote args))
             (else
              (c.scm:c-symbol-fun fun args)))))))

(define (c.scm:c-if args)
  `(if ,(c.scm:c (car args))
       ,(c.scm:c (cadr args))
       ,(c.scm:c (caddr args))))

(define (c.scm:c-and args)
  `(and ,@(map c.scm:c args)))

(define (c.scm:c-or args)
  `(or ,@(map c.scm:c args)))

(define (c.scm:c-begin args)
  `(begin ,@(map c.scm:c args)))

(define (c.scm:c-lambda args)
  `(lambda (,@(c.scm:f-lambda args) ,@(car args)) ,(c.scm:c (cadr args))))

(define (c.scm:c-let args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(let ,(reverse cdefs) ,(c.scm:c (cadr args))))
          (else
           (let ((def (car defs)))
             (loop (cdr defs)
                   (cons (cons (car def)
                               (c.scm:c (cdr def)))
                         cdefs)))))))

(define (c.scm:c-letrec args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(letrec ,(reverse cdefs) ,(c.scm:c (cadr args))))
          (else
           (let ((def (car defs)))
             (loop (cdr defs)
                   (cons (list (car def)
                               (c.scm:c (cadr def)))
                         cdefs)))))))

(define (c.scm:c-set! args)
  `(set! ,(car args) ,(c.scm:c (cadr args))))

(define (c.scm:c-quote args)
  `(quote ,(car args)))

(define (c.scm:c-symbol-fun fun args)
  (if (c.scm:var? fun)
      `(,fun ,@(c.scm:f (var-local-fun-args fun)) ,@(map c.scm:c args))
      `(,fun ,@(map c.scm:c args))))

;; h
;; 入力:入れ子の関数を含む可能性がある項
;; 出力:入れ子の関数を含まない項、新しい定義のリスト
(define (c.scm:h sexp)
  (cond ((c.scm:var? sexp)
         sexp)
        ((or (symbol? sexp)
             (c.scm:self-eval? sexp))
         sexp)
        ((c.scm:pair? sexp)
         (let ((fun (car sexp))
               (args (cdr sexp)))
           (case fun
                    ((if) (c.scm:h-if args))
                    ((and) (c.scm:h-and args))
                    ((or) (c.scm:h-or args))
                    ((begin) (c.scm:h-begin args))
                    ((lambda) (c.scm:h-lambda args))
                    ((delay) (c.scm:h-delay args))
                    ((let) (c.scm:h-let args))
                    ((let*) (c.scm:h-let args))
                    ((letrec) (c.scm:h-letrec args))
                    ((set!) (c.scm:h-set! args))
                    ((quote) (c.scm:h-quote args))
                    (else
                     (c.scm:h-symbol-fun fun args)))))))

(define (c.scm:h-if args)
  `(if ,(c.scm:h (car args))
       ,(c.scm:h (cadr args))
       ,(c.scm:h (caddr args))))

(define (c.scm:h-and args)
  `(and ,@(map c.scm:h args)))

(define (c.scm:h-or args)
  `(or ,@(map c.scm:h args)))

(define (c.scm:h-begin args)
  `(begin ,@(map c.scm:h args)))

(define (c.scm:h-lambda args)
  `(lambda ,(car args) ,(c.scm:h (cadr args))))

(define (c.scm:h-let args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           `(let ,(reverse cdefs) ,(c.scm:h (cadr args))))
          (else
           (let ((def (car defs)))
             (loop (cdr defs)
                   (cons (cons (car def)
                               (c.scm:h (cdr def)))
                         cdefs)))))))

(define (c.scm:h-letrec args)
  (let loop ((defs (car args))
             (cdefs '()))
    (cond ((null? defs)
           (if (null? cdefs)
               (c.scm:h (cadr args))
               `(letrec ,(reverse cdefs) ,(c.scm:h (cadr args)))))
          (else
           (let ((def (car defs)))
             (if (var-local-fun (car def))
                 (begin (c.scm:h-local-fun def)
                        (loop (cdr defs)
                              cdefs))             
                 (loop (cdr defs)
                       (cons (list (car def)
                                   (c.scm:h (cadr def)))
                             cdefs))))))))
  
(define (c.scm:h-local-fun def)
  (set! c.scm:*local-functions* (cons def c.scm:*local-functions*)))

(define (c.scm:h-set! args)
  `(set! ,(car args) ,(c.scm:h (cadr args))))

(define (c.scm:h-quote args)
  `(quote ,(car args)))

(define (c.scm:h-symbol-fun fun args)
  `(,fun ,@(map c.scm:h args)))               

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *function-name* "cscm")
(define *newvar* 0)
(define (newvar . name)
  (set! *newvar* (+ *newvar* 1))
  (if (null? name)
      (string->symbol
       (string-append *function-name* (number->string *newvar*)))	 
      (string->symbol
       (string-append (car name) (number->string *newvar*)))))
