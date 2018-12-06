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
(define (cscm:symbol? x)
  (or (cscm:var? x)
      (symbol? x)))

(define (cscm:pair? x)
  (and (not (cscm:var? x))
       (pair? x)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 新しい変数を用意する
;; c0transform, c8anfで使用
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
;; anfとrenameで使用する
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

                          'map 'for-each 'force))

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
#|
(load "c.scm-c0.scm")
(load "c.scm-c1.scm")
(load "c3-normalize.scm")
(load "c4-close.scm")
(load "c5-hoist.scm")
(load "c6-lambda.scm")
(load "c7-scheme.scm")
(load "c8-anf.scm")
(load "c9-generate.scm")
(load "c10-expand-or-and.scm")
(load "c11-expand-namedlet.scm")
(load "c12-assign.scm")
(load "c13-gc.scm")

(load "c0transform.scm") ;;
(load "c1analysis.scm")
(load "c4close.scm")
(load "c5hoist.scm")
(load "c6contain-lambda.scm")
(load "c7scheme.scm")
(load "c8a-normalize.scm")
(load "c9generate.scm")
(load "c10or-and.scm")
(load "c12contain-set.scm")
;;(load "c13-gc.scm")
(load "c14rename.scm")
(load "c16call-code.scm")
(load "c17replace-cname.scm")
|#

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
;;(load "~/Dropbox/scheme/c.scm/c13-gc.scm")
(load "~/Dropbox/scheme/c.scm/c14rename.scm")
(load "~/Dropbox/scheme/c.scm/c16call-code.scm")
(load "~/Dropbox/scheme/c.scm/c17replace-cname.scm")
(load "~/Dropbox/scheme/c.scm/c3liftable.scm")
(load "~/Dropbox/scheme/c.scm/c18constant.scm")



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

(define *scheme* '())
(define *cscm* '())
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
  (let ((tmp0 *scheme-port*)
        (tmp1 *c-port*))
    (set! *scheme-port* (open-output-file (string-append input ".scm")))
    (set! *c-port* (open-output-file (string-append input ".c")))
    ;;
    (let  ((explst (expr-lst input)))
      (for-each compile-def explst))
    ;;(print "cscm:debug, compile-file, *rename-alist* -> " *rename-alist*) ;; debug
    (replace-cname)
    (let loop ((cscm *cscm*))
      (if (null? cscm)
          #t
          (let ((sexp (car cscm)))
            (set-car! cscm (c16call-code sexp))
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
    (let ((topexp (car cexps)))
      (if (cscm? topexp) ;; Cにできるかチェック
          (let ((name (cadr topexp)))
            (let ((cname (make-c-name name)))
              (set-car! (cdr topexp) cname)
              (set! *rename-alist* (cons (cons name cname) *rename-alist*))
              (set! *cscm* (cons (c18constant topexp) *cscm*))))
          (set! *scheme* (cons topexp *scheme*))))
    (let loop ((cexps (cdr cexps)))
      (if (null? cexps)
          #t
          (let ((cexp (car cexps)))
            (if (cscm? cexp) ;; Cにできるかチェック
                (let ((var (cadr cexp)))
                  (let ((name (var-name var)))
                    (let ((cname (make-c-name name)))
                      (set-var-name var cname)
                      (set! *cscm* (cons (c18constant cexp) *cscm*)))))
                (set! *scheme* (cons cexp *scheme*)))
            (loop (cdr cexps)))))))

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
          (set-car! cscm (apply-funs expr c7scheme c10or-and c8a-normalize c14rename-def))
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

        
