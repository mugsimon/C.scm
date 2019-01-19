;;; schemeのdefine式を受け取り以下の変形を行う
;;; cond, case -> if
;;; ローカル関数 -> letrec
;;; do -> 名前付きlet
;;; 名前付きlet -> letrec
;;; 関数位置の生lambda -> let
;;; quasiquote, unquote, unquote-splicing -> cons, append, quote

;; (define var (lambda params body))
;; (define var expr)
;; (define (var params) body)
(define (c0transform x)
  (if (pair? x)
      (case (car x)
        ((define)
         (cond ((or (null? (cdr x)) ;; (define)
                    (null? (cddr x))) ;; (define var)
                (error "CSCM:ERROR, c0transform, syntax-error" x))
               ((symbol? (cadr x)) ;; (define var ...)
                (if (not (null? (cdddr x))) ;; (define var exp ...)
                    (error "CSCM:ERROR, c0transform, syntax-error" x)
                    (let ((form (caddr x)))
                      (if (and (pair? form)
                               (eq? (car form) 'lambda)) ;; (define var (lambda ...))
                          (let ((first (car x)) ;; define
                                (var (cadr x))) ;; var
                            (c0def-func first var form))
                          (let ((first (car x)) ;; define
                                (var (cadr x))) ;; var
                            `(,first ,var ,(c0def-expr form)))))))
               ((pair? (cadr x)) ;; (define (var params) body)
                (let ((first (car x)) ;; define
                      (var (caadr x)) ;; var
                      (params (cdadr x)) ;; params
                      (body (cddr x))) ;; body
                  (let ((lambda-expr `(lambda ,params ,@body))) ;; (lambda params body)
                    (c0def-func first var lambda-expr))))))
        (else
         (error "CSCM:ERROR, c0transform, not a definition" x)))
      (error "CSCM:ERROR, c0transform, not a definition" x)))
      
(define (c0def-func first var lambda-expr)
  (if (null? (cdr lambda-expr))
      (error "CSCM:ERROR, c0def-func, syntax-error" lambda-expr)
      (let ((params (cadr lambda-expr)))
        (if (not (or (null? params)
                     (pair? params)
                     (symbol? params)))
            (error "CSCM:ERROR, c0def-func, syntax-error" lambda-expr)
            (begin (set! *c0name* (symbol->string var)) ;; c0map1のため
                   (let ((x (c0lam (cdr lambda-expr))))
                     `(,first ,var (lambda ,@x))))))))
         
(define (c0def-expr form)
  (let ((x (c0expr form)))
    x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intrinsic constants
(define (c0constant x)
  (if (cscm:self-eval? x)
      x
      `(quote ,x)))

(define c0null ''())
(define c0false #f)
(define c0true #t)
(define c0if-else-default ''())
(define c0cond-else-default ''())
(define c0begin-empty-default ''())

;;; SCCOND  Conditionals.
(define (c0if args)
  (if (or (end? args) ;; test, then, elseがない
          (end? (cdr args)) ;; then, elseがない
          (and (not (end? (cddr args))) ;; elseがあり
               (not (end? (cdddr args))))) ;; 更に式がある
      (error "CSCM:ERROR, c0if, syntax-error" `(if ,@args)) ;; 不正なif式
      (list 'if
            (c0fmla (car args)) ;; test式
            (c0expr (cadr args)) ;; then式
            (if (null? (cddr args))
                c0if-else-default
                (c0expr (caddr args))))))

(define (c0fmla fmla)
  (if (pair? fmla)
      (case (car fmla)
        ((and) (cond
                ((end? (cdr fmla)) c0true)
                ((end? (cddr fmla)) (c0fmla (cadr fmla)))
                (else (cons 'and (c0map c0fmla (cdr fmla))))))
        ((or) (cond
               ((end? (cdr fmla)) c0false)
               ((end? (cddr fmla)) (c0fmla (cadr fmla)))
               (else (cons 'or (c0map c0fmla (cdr fmla))))))
        ((not) (cond ((or (end? (cdr fmla)) (not (end? (cddr fmla))))
                      (error "CSCM:ERROR, c0fmla, wrong number of argument" fmla))
                     (else (list 'not (c0fmla (cadr fmla))))))
        (else (c0expr fmla)))
      (c0expr fmla)))

(define (c0and args)
  (cond ((end? args) c0true)
        ((end? (cdr args)) (c0expr (car args)))
        (else `(and ,@(c0args args)))))

(define (c0or args)
  (cond ((end? args) c0false)
        ((end? (cdr args)) (c0expr (car args)))
        (else `(or ,@(c0args args)))))

(define (c0cond args)
  (c0expr (c0expand-cond args)))

;; cond式を展開する
(define (c0expand-cond args)
  (if (null? args)
      c0cond-else-default ;;
      (let ((clause (car args)) ;; cond式の最初の節
            (rest (cdr args))) ;; 残りの節
        (cond ((end? clause) ;; １つも節がない
               (error "CSCM:ERROR, c0expand-cond, wrong number of argument" `(cond ,args)))
              ((eq? (car clause) 'else) ;; else節がきた
               `(begin ,@(cdr clause)))
              ((end? (cdr clause)) ;; 節のtest式しかないとき
               (if (end? rest) 
                   (car clause) ;; 最後の節のときはtest式が結果
                   `(or ,(car clause) ,(c0expand-cond rest)))) 
              ((and (eq? (cadr clause) '=>) ;; Gauche p.102参照, 省略記法
                    (not (end? (cddr clause)))
                    (end? (cdddr clause)))
               (let ((temp (newvar "cscmcond")))
                 `(let ((,temp ,(car clause))) ;; tempにtest式の結果を保持
                    (if ,temp
                        (,(caddr clause) ,temp) ;; test式が真ならこれを引数にthen式を呼び出す
                        ,(c0expand-cond rest)))))
              (else `(if ,(car clause) 
                         (begin ,@(cdr clause)) ;; test式が真ならその節の残りの式をすべて評価
                         ,(c0expand-cond rest)))))))

(define (c0case args) ;; (case key ((data ...) exp ...) ...)
  (cond ((end? args)
         (error "CSCM:ERROR, c0case, wrong number of argument" `(case ,args)))
        ((end? (cdr args)) ;; keyしかない
         (c0begin (list (car args) (cadr c0cond-else-default)))) ;; keyを評価してnullを返す
        (else
         (let ((temp (newvar "case"))) ;; c.scm:c0
           (c0expr `(let ((,temp ,(car args)))
                      ,(c0expand-case temp (cdr args))))))))

;; case式を展開する
(define (c0expand-case key clauses)
  (if (end? clauses) 
      (cadr c0cond-else-default) ;; else節なし
      (let ((clause (car clauses)) ;; 最初の節
            (rest (cdr clauses))) ;; 残りの節
        (cond ((end? clause)
               (error "CSCM:ERROR, c0expand-case, wrong number of argument" `(case ,args)))
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
                    ;;
                    ((map) (c0map1 args))
                    ;;
                    (else
                     (c0symbol-fun fun args)))) ;; (f args)の呼び出し
                 ((and (pair? fun)
                       (eq? (car fun) 'lambda)) ;; ((lambda ...) args)
                  (c0lambda-fun (cdr fun) args)) ;; ((...) ...) ;; let式に書き換える
                 (else
                  `(,(c0expr fun) ,@(c0args args)))))) ;; c.scm
        (else
         (case form
           ((#f) c0false)
           ((#t) c0true)
           ((()) c0null)
           (else (c0constant form))))))

(define (c0symbol-fun name args)
  `(,name ,@(c0args args))) ;; c.scm:c0

(define (c0lambda-fun lambda-expr args)
  (define (make-defs vl as) ;; lambda式の引数のリスト, lambda式に渡す引数
    (cond ((null? vl)
           (if (not (null? as))
               (error "CSCM:ERROR, c0lambda-fun, wrong number of argument")
               '()))
          ((symbol? vl) ;; 任意引数の場合
           `((,vl (list ,@as)))) ;; ((vl (arg ...)))
          (else
           (if (null? as)
               (error "CSCM:ERROR, c0lambda-fun, wrong number of argument")
               (cons (list (car vl) (car as)) ;; 仮引数と実引数のリスト
                     (make-defs (cdr vl) (cdr as)))))))
  (if (end? lambda-expr)
      (error "CSCM:ERROR, c0lambda-fun, syntax-error")
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
         `(begin ,@(c0map c0expr forms)))))

(define (c0body body defs)
  (if (end? body)
      (if (not (null? defs))
          (c0letrec (list (reverse defs))) ;; ローカルdefineしかない
          c0begin-empty-default) ;; 実行する本体がない
      (let ((form (car body))) 
        (cond ((and (pair? form) (eq? (car form) 'define)) ;; ローカルdefineのとき
               (if (or (end? (cdr form)) 
                       (end? (cddr form)))
                   (error "CSCM:ERROR, c0body, syntax-error" form))
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
  (cons 'lambda (c0lam args)))

(define (c0delay args)
  (cons 'delay (cdr (c0lam (cons '() args)))))

(define (c0lam lambda-expr)
  (if (end? lambda-expr) (scbad-args 'lambda lambda-expr))
  (list (car lambda-expr) (c0body (cdr lambda-expr) '()))) 

;;; SCLETS  Let, Let*, and Letrec.
(define (c0let args)
  (if (end? args)
      (error "CSCM:ERROR, c0let, syntax-error" `(let ,args)))
  (if (symbol? (car args))
      (if (end? (cdr args))
          (error "CSCM:ERROR, c0let, syntax-error" `(let ,@args))
          (c0named-let (car args) (cadr args) (cddr args)))
      (let ((body '()) (defs '()))
        (let dolist ((lst (car args)))
          (if (null? lst)
              '()
              (let ((def (car lst)))
                (if (or (end? def) (end? (cdr def)) (not (end? (cddr def))))
                    (error "CSCM:ERROR, c0let, syntax-error" def))
                (set! defs (cons def defs))
                (dolist (cdr lst)))))
        (set! body (c0body (cdr args) '()))
        (let dolist ((lst defs))
          (if (null? lst)
              '()
              (let ((def (car lst)))
                (let ((var (car def))
                      (form (cadr def))) ;; c.scm:c0
                  (if (and (pair? form)
                           (eq? (car form) 'lambda))
                      (set-cdr! def (list (c0lambda (cdr form))))
                      (set-cdr! def (list (c0expr form)))))
                (dolist (cdr lst)))))
        (list 'let (reverse defs) body))))

(define (c0let* args)
  (if (end? args)
      (error "CSCM:ERROR, c0let*, syntax-error" `(let* ,args)))
  (let ((body '()) (defs '()))
    (let dolist ((lst (car args)))
      (if (null? lst)
          '()
          (let ((def (car lst)))
            (if (or (end? def) (end? (cdr def)) (not (end? (cddr def))))
                (error "CSCM:ERROR, c0let*, syntax-error" def))
            (set! defs (cons def defs))
            (dolist (cdr lst)))))
    (set! body (c0body (cdr args) '()))
    (let dolist ((lst defs))
      (if (null? lst)
          '()
          (let ((def (car lst)))
            (let ((var (car def)) (form (cadr def))) ;; c.scm:c0
              (if (and (pair? form) (eq? (car form) 'lambda))
                  (set-cdr! def (list (c0lambda (cdr form)))) ;; c.scm:c0
                  (set-cdr! def (list (c0expr form)))))
            (dolist (cdr lst)))))
    (list 'let* (reverse defs) body)))

(define (c0letrec args)
  (if (end? args)
      (error "CSCM:ERROR, c0letrec, syntax-error" `(letrec ,args)))
  (let ((body '()) (defs '()))
    (let dolist ((lst (car args)))
      (if (null? lst)
          '()
          (let ((def (car lst)))
            (if (or (end? def) (end? (cdr def)) (not (end? (cddr def))))
                (error "CSCM:ERROR, c0letrec, syntax-error" def))
            (set! defs (cons def defs))
            (dolist (cdr lst)))))
    (set! body (c0body (cdr args) '()))
    
    (let dolist ((lst defs))
      (if (null? lst)
          '()
          (let ((def (car lst)))
            (let ((var (car def))
                  (form (cadr def))) ;; c.scm:c0
              (if (and (pair? form) (eq? (car form) 'lambda))
                  (set-car! (cdr def) (c0lambda (cdr form))) ;; c.scm
                  (set-car! (cdr def) (c0expr form))))
            (dolist (cdr lst)))))

    (list 'letrec (reverse defs) body)))

(define (c0named-let name bind body)
  (let ((inits (c0map (lambda (x)
                        (if (or (end? x) (end? (cdr x))
                                (not (end? (cddr x))))
                            (error "CSCM:ERROR, c0named-let, syntax-error" x))
                        (c0expr (cadr x))) 
                      bind))
        (vars (c0map car bind)))
    `(letrec ((,name
               (lambda ,vars ,
                 (cadr (c0lam (cons vars body))))))
       (,name ,@inits))))

(define (c0do args)
  (if (or (end? args) (end? (cdr args)) (end? (cadr args)))
      (error "CSCM:ERROR, c0do, syntax-error" `(do ,@args)))
  (let ((name (newvar "do"))) ;; c.scm:c0
    (c0named-let name (c0map (lambda (x) (list (car x) (cadr x))) (car args))
                 `((if ,(caadr args)
                       (begin ,@(cdadr args))
                       (begin ,@(cddr args)
                              ,(cons name
                                     (c0map (lambda (ite)
                                              (cond ((or (end? ite) (end? (cdr ite)))
                                                     (error "CSCM:ERROR, c0do, syntax-error" ite))
                                                    ((end? (cddr ite))
                                                     (car ite))
                                                    ((end? (cdddr ite))
                                                     (caddr ite))
                                                    (else
                                                     (error "CSCM:ERROR, c0do, syntax-error" ite))))
                                            (car args)))))))))

;;; SCMISC  Miscellaneous Special Forms.

(define (c0quote args)
  (if (or (end? args) (not (end? (cdr args))))
      (error "CSCM:ERROR, c0quote, syntax-error" `(quote ,@args)))
  (case (car args)
    ((#f) c0false)
    ((#t) c0true)
    ((()) c0null)
    (else (c0constant (car args)))))

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
           `(cons ,''quasiquote ,(qq (cdr x) (set! level (+ level 1))))) ;; 入れ子の準クオート ;; c.scm:c0
          ((eq? (car x) 'unquote) ;; x -> (unquote ...)
           (let ((xcdr (cdr x))) ;; (...)
             (if (or (not (pair? xcdr)) 
                     (not (null? (cdr xcdr))))
                 (error "CSCM:ERROR, c0quasiquote, syntax-error" xcdr)
                 (if (zero? level)
                     (car xcdr) ;; ...を評価
                     `(cons ,''unquote ,(qq xcdr (set! level (- level 1)))))))) ;; 入れ子のレベルを一つ下げる
          ((and (pair? (car x))
                (eq? (caar x) 'unquote-splicing)) ;; x -> (unquote-splicing ...)
           (let ((xcar (car x)) ;; unquote-splicing
                 (xcdr (cdr x))) ;; (...)
             (if (or (not (pair? (cdr xcar)))
                     (not (null? (cddr xcar))))
                 (error "CSCM:ERROR, c0quasiquote, syntax-error" xcar))
             (if (zero? level)
                 `(append  ,(cadr xcar) ,(qq xcdr level)) 
                 `(cons (cons ,''unquote-splicing ,(qq (cdr xcar) (set! level (- level 1)))) ;; 入れ子のレベルを一つ下げる
                        ,(qq xcdr level))))) ;; 
          (else
           `(cons ,(qq (car x) level) ;; 
                  ,(qq (cdr x) level))))) 
  (if (or (end? args) (not (end? (cdr args))))
      (error "CSCM:ERROR, c0quasiquote, syntax-error" args))
  (c0expr (qq (car args) 0)))

(define (c0define args)
  (error "CSCM:ERROR, c0define, syntax-error" `(define ,@args)))

(define (c0macro args)
  (error "CSCM:ERROR, c0macro, syntax-error" `(macro ,@args)))

;;; SCVREF  Variable References.
(define (c0vref name)
  ;;(print "c.scm:debug, c0vref, name -> " name) ;; debug
  name)

(define (c0set! args)
  (if (or (end? args) (end? (cdr args)) (not (end? (cddr args))))
      (error "CSCM:ERROR, c0set!, syntax-error" `(set! ,@args)))
  (let ((name (car args))
        (form (cadr args)))
    (cond ((not (symbol? name))
           (error "CSCM:ERROR, c0set!, syntax-error" `(set! ,@args)))
          ((member name '(if cond case and or begin lambda delay let let* 
                             letrec do set! quote quasiquote define macro))
           (error "CSCM:ERROR, c0set!, Cannot set to the keyword" name))
          ((eq? name form)
           (c0vref name))
          (else
           (list 'set! name (c0expr form))))))

;; ライブラリ関数
(define (end? x)
  (cond ((pair? x) #f) 
        ((null? x) #t)
        (else (error "CSCM:ERROR, end?, not a pair" x))))

(define (c0map fun l) 
  (if (end? l)
      '()
      (cons (fun (car l)) (c0map fun (cdr l)))))

(define (c0map1 args)
  (let ((f (car args)))    
    (if (= (length (cdr args)) 1)
        (if (and (pair? f)
                 (eq? (car f) 'lambda))
            (let ((cscm1 (symbol->string (newvar "mapfun")))
                  (cscm2 (symbol->string (newvar "map"))))
              (let ((new-fun-name (string->symbol (string-append cscm1 "_" *c0name*)))
                    (new-map-name (string->symbol (string-append cscm2 "_" *c0name*))))       
                `(letrec ((,new-fun-name ,f)
                          (,new-map-name (lambda (x)
                                           (if (null? x)
                                               '()
                                               (cons (,new-fun-name (car x))
                                                     (,new-map-name (cdr x)))))))
                   (,new-map-name ,(cadr args)))))
            (let ((cscm1 (symbol->string (newvar "map"))))
              (let ((new-map-name (string->symbol (string-append cscm1 "_" *c0name*))))
                `(letrec ((,new-map-name (lambda (x)
                                           (if (null? x)
                                               '()
                                               (cons (,f (car x))
                                                     (,new-map-name (cdr x)))))))
                   (,new-map-name ,(cadr args))))))
        (begin (print "c0map1: 関数に渡すリストが1つではありません")
               `(map ,@args)))))
                                                     
(define *c0name* #f)
