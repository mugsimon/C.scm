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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 共通関数
;; 自己評価的データなら#tを返す
(define (c.scm:self-eval? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))

;; リストxとリストyの和集合のリストを返す
(define (c.scm:union x y)
  (cond ((null? x)
         y)
        ((member (car x) y)
         (c.scm:union (cdr x) y))
        (else
         (cons (car x) (c.scm:union (cdr x) y)))))

;; リストxからリストyの要素を取り除いたリストを返す
(define (c.scm:difference x y)
  (cond ((null? x)
         '())
        ((member (car x) y)
         (c.scm:difference (cdr x) y))
        (else
         (cons (car x) (c.scm:difference (cdr x) y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-varを持つSchemeコードで使用する
(define (c.scm:symbol? x)
  (or (c.scm:var? x)
      (symbol? x)))

(define (c.scm:pair? x)
  (and (not (c.scm:var? x))
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
       (list? (var-local-fun-args x))
       (list? (var-loc x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 新しい変数を用意する
;; c0transform, c8anfで使用
(define *newvar-name* "c.scm")
(define *newvar* 0)
(define (newvar . name)
  (set! *newvar* (+ *newvar* 1))
  (if (null? name)
      (string->symbol
       (string-append *newvar-name* (number->string *newvar*)))
      (string->symbol
       (string-append (car name) (number->string *newvar*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 各パスをロード
(load "c.scm-c0.scm")
(load "c.scm-c1.scm")
(load "c3-noramlize.scm")
(load "c4-close.scm")
(load "c5-hoist.scm")
(load "c6-lambda.scm")
(load "c7-scheme.scm")
(load "c8-anf.scm")
(load "c9-generate.scm")

;;;;;;;;;;;;;;;;;;;;;;;;
;; 最初の入力に続く入力の関数を順番に適用した結果を返す
(define (apply-funs x . funs)
  (if (null? funs)
      x
      (apply-funs ((car funs) x) (cdr funs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
