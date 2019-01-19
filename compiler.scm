;; リストを作成する
;; ただし最後の要素はcdr部に格納される
(define (list* . x)
  (cond ((null? x)
         '())
        ((null? (cdr x))
         (car x))
        (else
         (cons (car x)
               (apply list* (cdr x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自己評価的データなら#tを返す
(define (cscm:self-eval? x)
  (or (number? x)
      (string? x)
      (char? x)
      (vector? x)
      (boolean? x)))

;; リストxとリストyの和集合のリストを返す
(define (cscm:union x y)
  (cond ((null? x)
         y)
        ((cscm:member (car x) y)
         (cscm:union (cdr x) y))
        (else
         (cons (car x) (cscm:union (cdr x) y)))))

;; リストxからリストyの要素を取り除いたリストを返す
(define (cscm:difference x y)
  (cond ((null? x)
         '())
        ((cscm:member (car x) y)
         (cscm:difference (cdr x) y))
        (else
         (cons (car x) (cscm:difference (cdr x) y)))))

;; make-varに対応したmemq
;; var-nameが同じなら同じ要素とみなす
(define (cscm:memq elt lst)
  (let ((elt (if (cscm:var? elt)
                 (var-name elt)
                 elt)))
    (let loop ((lst lst))
      (if (null? lst)
          #f
          (let ((top (if (cscm:var? (car lst))
                         (var-name (car lst))
                         (car lst))))
            (if (eq? elt top)
                lst
                (loop (cdr lst))))))))

;; make-varに対応したmember
;; var-nameが同じなら同じ要素とみなす
(define (cscm:member elt lst)
  (let ((elt (if (cscm:var? elt)
                 (var-name elt)
                 elt)))
    (let loop ((lst lst))
      (if (null? lst)
          #f
          (let ((top (if (cscm:var? (car lst))
                         (var-name (car lst))
                         (car lst))))
            (if (equal? elt top)
                lst
                (loop (cdr lst))))))))
    
;; リスト内の同じ要素を排除する
;; make-varの場合はより新しい要素に置き換える
(define (cscm:reduction lst)
  (let loop ((lst lst)
             (rlst '()))
    (cond ((null? lst)
           (reverse rlst))
          (else
           (let ((elt (car lst)))
             (let ((tmp (cscm:member elt rlst)))
               (if tmp
                   (begin (set-car! tmp elt)
                          (loop (cdr lst) rlst))
                   (loop (cdr lst) (cons elt rlst)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-varを持つSchemeコードで使用する
;; make-varまたはsymbolなら#t
(define (cscm:symbol? x)
  (or (cscm:var? x)
      (symbol? x)))

;; make-varではなくペアなら#t
(define (cscm:pair? x)
  (and (not (cscm:var? x))
       (pair? x)))

(define (cscm:list? x)
  (if (cscm:symbol? x)
      #f
      (cscm:list? (cdr x))))

;; make-varなら#t
(define (cscm:var? x)
  (and (list? x)
       (= (length x) 8)
       (symbol? (var-name x))
       (boolean? (var-funarg x))
       (boolean? (var-assigned x))
       (boolean? (var-closed x))
       (or (boolean? (var-local-fun x))
           (list? (var-local-fun x)))
       (or (boolean? (var-local-fun-args x))
           (list? (var-local-fun-args x)))
       (list? (var-loc x))
       (boolean? (var-liftable x))))

;; 20190115追加
;; make-varならvar-nameを返す
;; そうでなければ引数を返す
(define (cscm:var-name x)
  (if (cscm:var? x)
      (var-name x)
      x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 新しい変数を用意する
;; デフォルトでは「cscm番号」
(define *newvar-name* "cscm")
(define *newvar* 0)
(define (newvar . name)
  (set! *newvar* (+ *newvar* 1))
  (if (null? name)
      (string->symbol
       (string-append *newvar-name* (number->string *newvar*)))
      (string->symbol
       (string-append (car name) (number->string *newvar*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitiveとlibrary手続き
(define *primitive* (list 'eqv? 'eq? 

                            'number? 'complex? 'real? 'rational? 'integer?
                            'exact? 'inexact?
                            '= '< '> '<= '>=
                            '+ '* '- '/ 
                            'quotient 'remainder 'modulo
                            'numerator 'denominator
                            'floor 'ceiling 'truncate 'round
                            'exp 'log 'sin 'cos 'tan 'asin 'acos 'atan
                            'sqrt 'expt
                            'make-rectangular 'make-polar 'real-part 'imag-part 'magnitude 'angle
                            'exact->inexact 'inexact->exact
                            'number->string 'string->number
                            
                            'pair? 'cons 'car 'cdr 'set-car! 'set-cdr!
                            'caar 'cadr 'cddr

                            'symbol?
                            'symbol->string 'string->symbol

                            'char? 'char=? 'char<? 'char>? 'char<=? 'char>=?
                            'char->integer 'integer->char
                            
                            'string?
                            'make-string
                            'string-length 'string-ref 'string-set!


                            'vector?
                            'make-vector 'vector-length 'vector-ref 'vector-set!

                            'procedure?
                            'apply))
(define *library* (list 'equal?

                          'zero? 'positive? 'negative? 'odd? 'even?
                          'max 'min
                          'abs
                          'gcd 'lcm
                          'rationalize

                          'not
                          'boolean?

                          'caaar 'caadr 'cadar 'caddr 'cdaar 'cdadr 'cddar 'cdddr
                          'caaaar 'caaadr 'caadar 'caaddr 'cadaar 'cadadr 'caddar 'cadddr
                          'cdaaar 'cdaadr 'cdadar 'cdaddr 'cddaar 'cddadr 'cdddar 'cddddr
                          'null? list?
                          'list 'length 'append 'reverse 'list-tail 'list-ref
                          'memq 'memv 'member
                          'assq 'assv 'assoc

                          'char-ci=? 'char-ci<? 'char-ci>? 'char-ci<=? 'char-ci>=?
                          'char-alphabetic? 'char-numeric? 'char-whitespace? 'char-upper-case? 'char-lower-case?
                          'char-upcase 'char-downcase

                          'string
                          'string=? 'string-ci=?
                          'string<? 'string>? 'string<=? 'string>=?
                          'string-ci<? 'string-ci>? 'string-ci<=? 'string-ci>=?
                          'substring
                          'string-append
                          'string->list 'list->string
                          'string-copy
                          'string-fill!
                          
                          'vector
                          'vector->list 'list->vector
                          'vector-fill!

                          'map 'for-each 'force

                          'write 'display 'newline))

(define (cscm:primitive? x)
  (if (memq x *primitive*)
      #t
      #f))

(define (cscm:library? x)
  (if (memq x *library*)
      #t
      #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 各パスをロード
(load "~/Dropbox/scheme/c.scm/c0transform.scm") ;;
(load "~/Dropbox/scheme/c.scm/c1analysis.scm")
(load "~/Dropbox/scheme/c.scm/c4close.scm")
(load "~/Dropbox/scheme/c.scm/c5hoist.scm")
(load "~/Dropbox/scheme/c.scm/c6contain-lambda.scm")
(load "~/Dropbox/scheme/c.scm/c7scheme.scm")
(load "~/Dropbox/scheme/c.scm/c8a-normalize.scm")
(load "~/Dropbox/scheme/c.scm/c9generate.scm")
(load "~/Dropbox/scheme/c.scm/c10or-and.scm")
(load "~/Dropbox/scheme/c.scm/c12contain-set.scm")
(load "~/Dropbox/scheme/c.scm/c14rename.scm")
(load "~/Dropbox/scheme/c.scm/c16call-code.scm")
(load "~/Dropbox/scheme/c.scm/c17replace-cname.scm")
(load "~/Dropbox/scheme/c.scm/c3liftable.scm")
(load "~/Dropbox/scheme/c.scm/c18constant.scm")
(load "~/Dropbox/scheme/c.scm/c19remove-args.scm")


;;;;;;;;;;;;;;;;;;;;;;;;
;; xにfunsを順番に適用した結果を返す
(define (apply-funs x . funs)
  (let loop ((x x)
             (funs funs))
    (if (null? funs)
        x
        (loop ((car funs) x) (cdr funs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイルのs式をリストにして返す
(define (expr-lst input)
  (let ((iport (open-input-file input)))
    (let loop ((expr (read iport))
               (lst '()))
      (cond ((eof-object? expr)
             (close-input-port iport)
             lst)
            (else
             (loop (read iport)
                   (cons expr lst)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *scheme-port* (current-output-port))
(define *c-port* (current-output-port))

;; Schemeにするコードのリスト
(define *scheme* '())
;; Cにするコードのリスト
(define *cscm* '())
;; Schemeの大域変数に束縛する定数のリスト
(define *cscm-constant* '())
(define *rename-alist* '())




(define (compile input)
  (let ((tmp0 *scheme*)
        (tmp1 *cscm*)
        (tmp2 *cscm-constant*)
        (ret #f))
    (set! *scheme* '())
    (set! *cscm* '())
    (set! *cscm-constant* '())
    ;;
    (set! ret (if (string? input)
                  (compile-file input)
                  (compile-sexp input)))
    ;;
    (set! *cscm* tmp1)
    (set! *scheme* tmp0)
    (set! *cscm-constant* tmp2)
    ret))

(define (compile-sexp input)
  (compile-def input)
  (replace-cname)
  ;;((lambda (s x) (display s) (write x) (newline))  "cscm:debug, compile-sexp, *cscm* -> " *cscm*) ;; debug
  (let loop ((cscm *cscm*))
    (if (null? cscm)
        #t
        (let ((sexp (car cscm)))
          (set-car! cscm (c16call-code sexp))
          (loop (cdr cscm)))))
  ;;((lambda (s x) (display s) (write x) (newline))  "cscm:debug, compile-sexp, *cscm* -> " *cscm*) ;; debug
  (gen-c)
  (gen-scheme)
  )

(define (compile-file input)
  (let ((tmp0 *scheme-port*) ;; ポートを退避
        (tmp1 *c-port*)) ;; ポートを退避
    (set! *scheme-port* (open-output-file (string-append input ".scm"))) ;; input.scm
    (set! *c-port* (open-output-file (string-append input ".c"))) ;; input.c
    ;;
    (let ((explst (expr-lst input))) ;; 関数定義のリスト
      (for-each compile-def explst)) ;; 各関数定義をcompile-def
    ;;(print "cscm:debug, compile-file, *rename-alist* -> " *rename-alist*) ;; debug
    (replace-cname)
    (let loop ((cscm *cscm*))
      (if (null? cscm)
          #t
          (let ((sexp (car cscm)))
            (set-car! cscm (c16call-code sexp)) ;;
            (loop (cdr cscm)))))
    (gen-c)
    (gen-scheme)
    ;;((lambda (s x) (display s) (write x) (newline))  "cscm:debug, compile-file, *cscm* -> " *cscm*) ;; debug
    ;;
    (close-output-port *scheme-port*)
    (close-output-port *c-port*)
    (set! *scheme-port* tmp0)
    (set! *c-port* tmp1)))


(define (replace-cname)
  (letrec ((loop1 (lambda (scheme)
                    (if (null? scheme)
                        (loop2 *cscm*)
                        (let ((expr (car scheme)))
                          (set-car! scheme (c17replace-cname expr))
                          (loop1 (cdr scheme))))))
           (loop2 (lambda (cscm)
                    (if (null? cscm)
                        #t
                        (let ((expr (car cscm)))
                          (set-car! cscm (c17replace-cname expr))
                          (loop2 (cdr cscm)))))))
    (loop1 *scheme*)))

;; トップレベルの定義を受け取り、ホイストまで行う
;; CとSchemeを判断し、*scheme*と*cscm*に格納する
(define (compile-def input)
  (let ((cexps (apply-funs input c0transform c1analysis c3liftable c4close c5hoist))) ;; 先頭にトップレベル定義, 残りにホイストされたローカル関数
    (let ((topexp (car cexps))) ;; もともとトップレベルの関数
      (if (cscm? topexp) ;; Cにできるかチェック
          (let ((name (cadr topexp)))
            (let ((cname (make-c-name name))) ;; 接頭辞として c_ をつけた新しい名前
              (set-car! (cdr topexp) cname) ;; トップレベルの関数名を書き換える
              (set! *rename-alist* (cons (cons name cname) *rename-alist*)) ;; リネームリストにトップレベルの関数を加える　(名前 . 新しい名前)
              (set! *cscm* (cons (c18constant topexp) *cscm*)))) ;; quoteを使ったリストをグローバル変数に置き換え, *cscm-constant*に定数が入る
          (set! *scheme* (cons topexp *scheme*)))) ;; Cにできないときはこっち
    (let loop ((cexps (cdr cexps))) ;; トップレベルの次はホイストされてトップレベルになった関数の検査
      (if (null? cexps)
          (remove-c-args) ;; すべての検査が終了したらc_cscmってなってる引数を削除する
          (let ((cexp (car cexps)))
            (if (cscm? cexp) ;; Cにできるかチェック
                (let ((var (cadr cexp)))
                  (let ((name (var-name var)))
                    (let ((cname (make-c-name name))) ;; 接頭辞として c_ をつけた新しい名前
                      (set-var-name var cname) ;; 新しい名前に書き換えた
                      (set! *cscm* (cons (c18constant cexp) *cscm*))))) ;; quoteを使ったリストをグローバル変数に置き換え, *cscm-constant*に定数が入る
                (set! *scheme* (cons cexp *scheme*))) ;; Cにできないときはこっち
            (loop (cdr cexps)))))))

;; c_cscm...って名前の引数を削除する
(define (remove-c-args)
  (letrec ((sloop (lambda (s-lst) ;; Schemeにする関数定義から始める
                    (if (null? s-lst)
                        (cloop *cscm*)                        
                        (begin (set-car! s-lst (c19remove-args (car s-lst)))
                               (sloop (cdr s-lst))))))
           (cloop (lambda (c-lst)
                    (if (null? c-lst)
                        #t
                        (begin (set-car! c-lst (c19remove-args (car c-lst)))
                               (cloop (cdr c-lst)))))))
    (sloop *scheme*)))

(define (cscm? expr)
  (let ((form (caddr expr)))
    (if (and (pair? form)
             (eq? (car form) 'lambda))
        (not (or (c6contain-lambda? expr)
                 (c12contain-set!? expr)))
        #f)))

(define (make-c-name name)
  (string->symbol
   (string-append "c_" (symbol->string name))))    

(define (gen-c)
  (let loop ((cscm *cscm*))
    (if (null? cscm)
        #t
        (let ((expr (car cscm)))
          (set-car! cscm (apply-funs expr #;c7scheme c10or-and c8a-normalize c14rename-def))
          (loop (cdr cscm)))))
  (init-func) ;; 初期化コードを出力
  (dec-func) ;; 宣言を出力
  (let loop ((cscm *cscm*))
    (if (null? cscm)
        #t
        (let ((expr (car cscm)))
          (c9generate expr)
          (newline *c-port*)
          (loop (cdr cscm))))))

(define (gen-scheme)
  (let loop ((scheme *scheme*))
    (if (null? scheme)
        #t
        (let ((expr (car scheme)))
          (set-car! scheme (apply-funs expr c7scheme c14rename-def))
          (loop (cdr scheme)))))
  (let loop ((scheme *scheme*))
    (if (null? scheme)
        (gen-constant)
        (let ((expr (car scheme)))
          (write expr *scheme-port*)
          (newline *scheme-port*)
          (loop (cdr scheme))))))

(define (gen-constant)
  (let loop ((constant *cscm-constant*))
    (if (null? constant)
        #t
        (let ((expr (car constant)))
          (write expr *scheme-port*)
          (newline *scheme-port*)
          (loop (cdr constant))))))

(define (apply-c0 input)
  (if (string? input)
      (let ((exps (expr-lst input)))
        (for-each (lambda (x) (write x) (newline))
                  (reverse (map c0transform exps))))))

        
