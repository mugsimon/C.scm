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
;;; 共通関数
;; 自己評価的データなら#tを返す
(define (c.scm:self-eval? x)
  (or (number? x)
      (string? x)
      (char? x)
      (vector? x)
      (boolean? x)))

(define (cscm:self-eval? x)
  (or (number? x)
      (string? x)
      (char? x)
      (vector? x)
      (boolean? x)))


;; リストxとリストyの和集合のリストを返す
(define (c.scm:union x y)
  (cond ((null? x)
         y)
        ((c.scm:member (car x) y)
         (c.scm:union (cdr x) y))
        (else
         (cons (car x) (c.scm:union (cdr x) y)))))

;; リストxからリストyの要素を取り除いたリストを返す
(define (c.scm:difference x y)
  (cond ((null? x)
         '())
        ((c.scm:member (car x) y)
         (c.scm:difference (cdr x) y))
        (else
         (cons (car x) (c.scm:difference (cdr x) y)))))

(define (c.scm:var=? var1 var2)
  (cond ((and (c.scm:var? var1) (c.scm:var? var2))
         (eq? (var-name var1) (var-name var2)))
        ((c.scm:var? var1)
         (error "not a var" var2))
        ((c.scm:var? var2)
         (error "not a var" var1))
        (else
         (error "not a var" var1 var2))))

(define (c.scm:member elt lst)
  (if (c.scm:var? elt)
      (let loop ((lst lst))
        (if (null? lst)
            #f
            (let ((top (car lst)))
              (if (and (c.scm:var? top)
                       (c.scm:var=? elt top))
                  lst
                  (loop (cdr lst))))))
      (member elt lst)))
  
;; リスト内の同じ要素を排除する
;; make-varの場合はより新しい要素に置き換える
(define (c.scm:reduction lst)
  (let loop ((lst lst)
             (rlst '()))
    (cond ((null? lst)
           (reverse rlst))
          (else
           (let ((elt (car lst)))
             (let ((tmp (c.scm:member elt rlst)))
               (if tmp
                   (begin (set-car! tmp elt)
                          (loop (cdr lst) rlst))
                   (loop (cdr lst) (cons elt rlst)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-varを持つSchemeコードで使用する
(define (c.scm:symbol? x)
  (or (c.scm:var? x)
      (symbol? x)))

(define (cscm:symbol? x)
  (or (cscm:var? x)
      (symbol? x)))

(define (c.scm:pair? x)
  (and (not (c.scm:var? x))
       (pair? x)))

(define (cscm:pair? x)
  (and (not (cscm:var? x))
       (pair? x)))

(define (c.scm:var? x)
  (and (list? x)
       (= (length x) 7)
       (symbol? (var-name x))
       (boolean? (var-funarg x))
       (boolean? (var-assigned x))
       (boolean? (var-closed x))
       (or (boolean? (var-local-fun x))
           (list? (var-local-fun x)))
       (or (boolean? (var-local-fun-args x))
           (list? (var-local-fun-args x)))
       (list? (var-loc x))))

(define (cscm:var? x)
  (and (list? x)
       (= (length x) 7)
       (symbol? (var-name x))
       (boolean? (var-funarg x))
       (boolean? (var-assigned x))
       (boolean? (var-closed x))
       (or (boolean? (var-local-fun x))
           (list? (var-local-fun x)))
       (or (boolean? (var-local-fun-args x))
           (list? (var-local-fun-args x)))
       (list? (var-loc x))))

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
(define c.scm:*primitive* (list 'eqv? 'eq? 

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
(define c.scm:*library* (list 'equal?

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

(define (c.scm:primitive? x)
  (if (memq x c.scm:*primitive*)
      #t
      #f))

(define (c.scm:library? x)
  (if (memq x c.scm:*library*)
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
|#
;(load "~/c.scm/c.scm-c0.scm")
;(load "~/c.scm/c.scm-c1.scm")
;(load "~/c.scm/c3-normalize.scm")
;(load "~/c.scm/c4-close.scm")
;(load "~/c.scm/c5-hoist.scm")
;(load "~/c.scm/c6-lambda.scm")
;(load "~/c.scm/c7-scheme.scm")
;(load "~/c.scm/c8-a-normalize.scm")
;(load "~/c.scm/c9-generate.scm")
;(load "~/c.scm/c10-expand-or-and.scm")
#;(load "~/c.scm/c11-expand-namedlet.scm")
;(load "~/c.scm/c12-assign.scm")
;(load "~/c.scm/c13-gc.scm")
;(load "~/c.scm/c14-rename.scm")
;(load "~/c.scm/c15.scm")
;(load "~/c.scm/c16-call")
;;
;;(load "~/c.scm/list-to-cons.scm")
;;
(load "~/Dropbox/scheme/c.scm/c0transform.scm") ;;
(load "~/Dropbox/scheme/c.scm/c.scm-c1.scm")
(load "~/Dropbox/scheme/c.scm/c3-normalize.scm")
(load "~/Dropbox/scheme/c.scm/c4-close.scm")
(load "~/Dropbox/scheme/c.scm/c5-hoist.scm")
(load "~/Dropbox/scheme/c.scm/c6-lambda.scm")
(load "~/Dropbox/scheme/c.scm/c7-scheme.scm")
(load "~/Dropbox/scheme/c.scm/c8-a-normalize.scm")
(load "~/Dropbox/scheme/c.scm/c9-generate.scm")
(load "~/Dropbox/scheme/c.scm/c10-expand-or-and.scm")
(load "~/Dropbox/scheme/c.scm/c12-assign.scm")
(load "~/Dropbox/scheme/c.scm/c13-gc.scm")
(load "~/Dropbox/scheme/c.scm/c14-rename.scm")
(load "~/Dropbox/scheme/c.scm/c15.scm")
(load "~/Dropbox/scheme/c.scm/c16-call")



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
(define (c.scm:expr-lst input)
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
(define c.scm:*scheme-port* (current-output-port))
(define c.scm:*c-port* (current-output-port))
(set! c9*output-port* c.scm:*c-port*)

(define c.scm:*compiled* '())
(define *toplevel* '()) ;; トップレベル環境

(define (c.scm:compile input)
  (dlet ((c.scm:*compiled* '())
         (*toplevel* '()))
        (if (string? input)
            (c.scm:compile-file input)
            (c.scm:compile-sexp input))
        (if (string? input)
            (begin (dlet ((c.scm:*scheme-port* (open-output-file (string-append input ".scm")))
                          (c.scm:*c-port* (open-output-file (string-append input ".c"))))
                         (set! c9*output-port* c.scm:*c-port*)
                         (c.scm:generate-code) ;; コード出力
                         (close-output-port c.scm:*scheme-port*)
                         (close-output-port c.scm:*c-port*))
                   (set! c9*output-port* c.scm:*c-port*))
            (let loop ((compiled c.scm:*compiled*))
              (if (null? compiled)
                  (c.scm:generate-code)
                  (let ((x (c.scm:c16 (car compiled))))
                    (set-car! compiled x)
                    (loop (cdr compiled))))))))
      
(define (c.scm:compile-sexp input)
  (let ((x (apply-funs input c0transform c.scm:c1 c.scm:c3normalize c.scm:c4close)))
    (dlet ((c.scm:*c5local-functions* '()))
          (set! x (c.scm:c5hoist x))
          (set! c.scm:*c5local-functions* (map c.scm:c15 (cons x c.scm:*c5local-functions*)))
          (set! c.scm:*compiled* (append c.scm:*c5local-functions* c.scm:*compiled*)))))


#;(define (c.scm:compile-sexp input)
  (let ((x (apply-funs input c.scm:c0transform c.scm:c1 c.scm:c3normalize c.scm:c4close)))
    (dlet ((c.scm:*c5local-functions* '()))
          (set! x (c.scm:c5hoist x))
          (c.scm:generate-code x)
          (let loop ((local-funs c.scm:*c5local-functions*))
            (if (null? local-funs)
                #t
                (begin (c.scm:generate-code (car local-funs))
                       (loop (cdr local-funs))))))))

(define (c.scm:generate-code)
  (letrec ((generate (lambda (x)
                       (let ((name (cadr x)))
                         (if (var-cscm name)
                             (begin (c.scm:c9generate (apply-funs x c.scm:c7scheme c.scm:c10expand-or-and c.scm:c8a-normalize c.scm:c14rename))
                                    (newline c.scm:*c-port*))
                             (begin (display (c.scm:c14rename (c.scm:c7scheme x)) c.scm:*scheme-port*)
                                    (newline c.scm:*scheme-port*))))))
           (loop (lambda (funs)
                   (if (null? funs)
                       #t
                       (begin (generate (car funs))
                              (loop (cdr funs)))))))
    (loop c.scm:*compiled*)))

#;(define (c.scm:generate-code x)
  (if (or (c.scm:c6raw-lambda x)
          (c.scm:c12assign x)
          (c.scm:c13gc x))
              (begin (display (c.scm:c7scheme x) c.scm:*scheme-port*)
                     (newline c.scm:*scheme-port*))
              (begin (c.scm:c9generate (apply-funs x c.scm:c7scheme c.scm:c10expand-or-and c.scm:c8a-normalize c.scm:s1list-to-cons c.scm:c14rename))
                     (newline c.scm:*c-port*))))

;;;;;;;;;;;;;;;;;;;;;;
(define (c.scm:expr-lst input)
  (let ((iport (open-input-file input)))
    (let loop ((expr (read iport))
               (lst '()))
      (cond ((eof-object? expr)
             (close-input-port iport)
             lst)
            (else
             (loop (read iport)
                   (cons expr lst)))))))

(define (c.scm:compile-file input)
  (let ((exp-list (c.scm:expr-lst input)))
    (for-each c.scm:compile-sexp exp-list)
    (let loop ((compiled c.scm:*compiled*))
      (if (null? compiled)
          #t
          (let ((x (c.scm:c16 (car compiled))))
            (set-car! compiled x)
            (loop (cdr compiled)))))))
         

#;(define (c.scm:compile-file input)
  (let ((exp-list (c.scm:expr-lst input)))
    (dlet ((c.scm:*scheme-port* (open-output-file (string-append input ".scm")))
           (c.scm:*c-port* (open-output-file (string-append input ".c"))))
          (set! c9*output-port* c.scm:*c-port*)
          (for-each c.scm:compile-sexp exp-list)
          (close-output-port c.scm:*scheme-port*)
          (close-output-port c.scm:*c-port*))
    (set! c9*output-port* c.scm:*c-port*)))

