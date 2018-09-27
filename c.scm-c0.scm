;;; schemeの式を受け取り、変形を行う
;;; cond, case -> if
;;; ローカル関数 -> letrec
;;; do -> 名前付きlet
;;; 関数位置の生lambda -> let

;; (define var (lambda params body))
;; (define var expr)
;; (define (var params) body)
;; (begin ...)
;; (fun ...)
(define (c.scm:c0transform x)
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
                (error "C.SCM:ERROR, c.scm:compile-sexp, syntax-error:" sexp))))
        ((begin)
         `(begin ,@(map compile (cdr x))))
        (else
         (c.scm:c0compile-expr x))) ;; (expr ...)
      (c.scm:c0compile-expr x))) ;; atom

;; マクロ
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

;; first->define
;; args->(var params)|(var . param)
;; body->expr
(define (c.scm:c0compile-function first args body)
  (if (not (and (pair? args)
		        (symbol? (car args))))
      (scerror "~s is a bad arg to DEFINE." args))
  (let ((x (dlet ((*env* '()))
                 (c0lam (cons (cdr args) body)))))
    `(,first ,(car args) (lambda ,@x))))

;; form->expr
(define (c.scm:c0compile-expr form)
  (let ((x (dlet ((*env* '()))
                 (c0expr form))))
    x))

;; 自己評価的データなら#tを返す
;; x->expr
(define (c.scm:self-eval? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intrinsic constants
(define (c1constant x)
  #;(list c2constant x)
  (if (c.scm:self-eval? x)
      x
      `(quote ,x)))

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

;;; SCCOND  Conditionals.
(define (c1if args) 
  (if (or (end? args) ;; testがない
          (end? (cdr args)) ;; thenがない
          (and (not (end? (cddr args))) ;; else節があり
               (not (end? (cdddr args))))) ;; 更に節がある
      (scbad-args 'if args)) ;; 引数エラー
  (list 'if #;c2if
        (c1fmla (car args))
        (c1expr (cadr args))
        (if (null? (cddr args))
            c1if-else-default
            (c1expr (caddr args)))))

(define (c1fmla fmla)
  (if (pair? fmla)
      (case (car fmla)
        ((and #;AND) (cond
                 ((end? (cdr fmla)) c1true)
                 ((end? (cddr fmla)) (c1fmla (cadr fmla)))
                 (else (cons 'and #;'FMLA-AND (c1map c1fmla (cdr fmla))))))
        ((or #;OR) (cond
                ((end? (cdr fmla)) c1false)
                ((end? (cddr fmla)) (c1fmla (cadr fmla)))
                (else (cons 'or #;'FMLA-OR (c1map c1fmla (cdr fmla))))))
        ((not #;NOT) (cond ((c1lookup 'not) (c1expr fmla))
                     ((or (end? (cdr fmla)) (not (end? (cddr fmla))))
                      (scbad-args 'not (cdr fmla)))
                     (else (list 'not #;'FMLA-NOT (c1fmla (cadr fmla))))))
        (else (c1expr fmla)))
      (c1expr fmla)))

(define (c1and args)
  (cond ((end? args) c1true)
        ((end? (cdr args)) (c1expr (car args)))
        (else `(and ,@(c1args args))
              #;(list c2and (c1args args))))) ;; 引数を解析

(define (c1or args)
  (cond ((end? args) c1false)
        ((end? (cdr args)) (c1expr (car args)))
        (else `(or ,@(c1args args))
              #;(list c2or (c1args args))))) ;; 引数を解析

(define (c1cond args)
  (c1expr (c1expand-cond args)))

;; cond式を展開する. 基本的に入れ子のifとbeginにする
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

(define (c1case args) ;; (case key ((data ...) exp ...) ...)
  (cond ((end? args) (scbad-args 'case args))
        ((end? (cdr args)) ;; keyしかない
         (c1begin (list (car args) (cadr c1cond-else-default)))) ;; keyを評価してnullを返す
        (else
         (let ((temp (gensym)))
           (c1expr `(let ((,temp ,(car args)))
                      ,(c1expand-case temp (cdr args))))))))

;; case式をcaseを使わない形に展開する
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
                    (else
                     #;(if (>= *re-compile-level* 2)
                     (set-fun-data fun)) ;;;;; By A243  Nov, 1992 ;;;;;
                     #;(if (macro-function fun)
                     (c1expr ((macro-function fun) form))
                     (c1symbol-fun fun args))
                     (c1symbol-fun fun args)))) ;; (f args)の呼び出し
                 ((and (pair? fun)
                       (eq? (car fun) 'lambda)) ;; ((lambda ...) args)
                  (c1lambda-fun (cdr fun) args)) ;; ((...) ...) ;; let式に書き換える
                 (else
                  `(,(c1expr fun) ,@(c1args args)) ;; c.scm
                  #;(list c2funcall (c1expr fun) (c1args args))))))
        (else
         (case form
           ((#f) c1false)
           ((#t) c1true)
           ((()) c1null)
           (else (c1constant form))))))

(define (c1symbol-fun name args)
  `(,name ,@(c1args args)))

(define (c1lambda-fun lambda-expr args)
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

(define (c1args forms)
  (if (end? forms)
      '()
      (cons (c1expr (car forms))
            (c1args (cdr forms)))))

(define (c1begin forms)
  (cond ((end? forms)
         c1begin-empty-default) 
        ((end? (cdr forms))
         (c1expr (car forms))) ;; 式が一つだけ
        (else
         `(begin ,@(c1map c1expr forms)))))

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

(define (c1lambda args)
  (cons 'lambda (c1lam args)))

#;(define (c1lambda args)
  (dlet ((*env* (cons 'CB *env*))) 
        (cons c2lambda (cons (cons 'lambda args) (c1lam args)))))

(define (c1delay args)
  (cons 'delay (cdr (c1lam (cons '() args)))))

#;(define (c1delay args)
  (dlet ((*env* (cons 'CB *env*)))
        (cons c2delay (cddr (c1lam (cons '() args)))))) ;; lambda式の無引数版

(define (c1lam lambda-expr)
  (if (end? lambda-expr) (scbad-args 'lambda lambda-expr))
  (list (car lambda-expr) (c1body (cdr lambda-expr) '())))

#;(define (c1lam lambda-expr)
  (if (end? lambda-expr) (scbad-args 'lambda lambda-expr))
  (let ((requireds '()) (rest '()))
    (dlet ((*env* *env*))
      (do ((vl (car lambda-expr) (cdr vl)))
          ((not (pair? vl))
           (if (not (null? vl))
               (let ((var (make-var vl)))
                 (set! *env* (cons var *env*))
                 (set! rest var)))
           (list (reverse requireds)
                 rest
                 (c1body (cdr lambda-expr) '())))
        (let ((var (make-var (car vl))))
          (set! requireds (cons var requireds))
          (set! *env* (cons var *env*)))))))

(define (make-arg-info requireds rest)
  (if (null? rest)
      (* (+ requireds 1) 4)
      (+ (* (+ requireds 1) 8) 3)))

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
                        (set! defs (cons (list var (cadr def)) defs)) ;; c.scm:c0
                        #;(set! defs (cons (cons var (cadr def)) defs)) ;; (情報変数 . 初期値)をdefsに追加
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
                      (set-cdr! def (c1expr form))))) ;; 初期値がlambda式以外ならc1exprで解析した結果に置き換え
        (dolist (def defs) ;; c.scm:c0
                (set-car! def (var-name (car def))))
        (list 'let #;c2let (reverse defs) body))))
;;20180926ここまで

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
                        (form (caddr def)) ;; (lambda (...) ...)
                        #;(c.scm:free-vars (c.scm:difference *env* (map car defs)))) ;; c.scm この時点での自由変数
                    (if (and (pair? form) (eq? (car form) 'lambda #;'LAMBDA)) ;; 初期値がlambda式, r7rsでは不等価
                        (if (or (var-funarg var) (var-assigned var) (var-closed var)) 
                            (set-car! (cdr def) (c1lambda (cdr form))) ;; lambda式の解析結果 (var ここに格納 (lambda (...) ...))
                            (begin (set-car! (cdr def) `(lambda ,@(c1lam (cdr form)))) ;; c.scm
                                   #;(set-car! (cdr def) (c1lam (cdr form))) ;; lambda式の解析結果 (var ここに格納 (lambda (...) ...))
                                   #;(set-var-local-fun var (reverse (c.scm:difference c.scm:free-vars (cadadr def)))) ;; c.scm #tの代わりに自由変数のリストをおく
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
  name)

(define (c1lookup name)
  (let lookup ((env *env*))
    (cond ((null? env) #f)
          ((and (not (eq? (car env) 'CB))
                (eq? (var-name (car env)) name))
           #t)
          (else (lookup (cdr env))))))


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


(define (end? x)
  (cond ((pair? x) #f) 
        ((null? x) #t)
        (else (scerror "~s is not a pair." x))))

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
