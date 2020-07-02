;;; Simple C Compiler written in Scheme
;;; Copyright (c) 2020 yangrq

(import (match match))

(define-enumeration token-kind 
                    (reserved identifier string-literal integer-literal eof)
                    token-kind-constructor)
(define-enumeration type-kind
                    (void bool char short int long enum pointer array struct function)
                    type-kind-constructor)

(define-record-type token (fields kind content) (nongenerative))
(define-record-type type-tag (fields kind size align) (nongenerative))
(define-record-type type (fields tag is-incomplete? extra) (nongenerative))
(define-record-type type-array-extra (fields base len) (nongenerative))
(define-record-type struct-member (fields type name offset) (nongenerative))

(define void-tag (make-type-tag (type-kind void) 1 1))
(define bool-tag (make-type-tag (type-kind bool) 1 1))
(define char-tag (make-type-tag (type-kind char) 1 1))
(define short-tag (make-type-tag (type-kind short) 2 2))
(define int-tag (make-type-tag (type-kind int) 4 4))
(define long-tag (make-type-tag (type-kind long) 8 8))

(define keyword 
  '(
    return  if     else     while    for    int    char
    sizeof  struct typedef  short    long   void   _Bool
    enum    static break    continue goto   switch case
    default extern _Alignof do       signed
    ))

(define multi-char-operator
  '(
    <<= >>= ... == != <= >= -> ++ -- <<
    >>  +=  -=  *= /= && \|\| &= \|= ^=
    ))

(define integer-prefix '(0x 0b))

(define is-hex-digit?
  (lambda (ch)
    (or
      (and (char>=? ch #\0) (char<=? ch #\9))
      (and (char-ci>=? ch #\a) (char-ci<=? ch #\f)))))

(define is-oct-digit?
  (lambda (ch)
    (and (char>=? ch #\0) (char<=? ch #\7))))

(define is-bin-digit? 
  (lambda (ch)
    (or (char=? ch #\0) (char=? ch #\1))))

(define unget-string
  (lambda (port str start end) 
    (let unget-matched ([idx end])
      (when (>= idx start)
        (begin (unget-char port (string-ref str idx))
               (unget-matched (- idx 1)))))))

(define unget-symbol
  (lambda (port symbol)
    (let* ([symbol-string (symbol->string symbol)]
           [n (string-length symbol-string)])
      (unget-string port symbol-string 0 (- n 1)))))

(define match-symbol 
  (lambda (port symbols*)
    (find
      (lambda (symbol)
        (let* ([symbol-string (symbol->string symbol)]
               [n (string-length symbol-string)])
          (and (let match ([idx 0])
                 (if (= idx n)
                   #t
                   (let ([ch (peek-char port)]) 
                     (if (and (not (eof-object? ch))
                              (char=? ch (string-ref symbol-string idx)))
                       ;;; if match succeeded, eat a char from port and match next
                       (begin (read-char port)
                              (match (+ idx 1)))
                       ;;; if match failed, recover the port to initial state and return #f
                       (begin (unget-string port symbol-string 0 (- idx 1))
                              #f)))))
               symbol)))
      symbols*)))

(define read-keyword
  (lambda (port)
    (match-symbol port keyword)))

(define read-multi-char-operator
  (lambda (port)
    (match-symbol port multi-char-operator)))

(define read-integer-value
  (lambda (port)
    (let ([ch (peek-char port)])
      (when (eof-object? ch)
        (assertion-violation 'read-integer-literal
                             "given port has reached eof"
                             port))
      (and (char-numeric? ch) 
           (let ([sym (match-symbol port integer-prefix)])
             ;;; if '0x' or '0b' is followed by an eof mark, raise a exception 
             (if (eof-object? (peek-char port))
               (error 'read-integer-literal
                      "unexpected eof after integer literal prefix" 
                      (symbol->string sym)) 
               (let* ([integer-string             
                        (list->string 
                          (reverse (let loop ([ls '()]
                                              [ch (peek-char port)])
                                     (if (eof-object? ch)
                                       ls
                                       (when (or (char-numeric? ch) 
                                                 (char-alphabetic? ch))
                                         (read-char port)
                                         (loop (cons ch ls) (peek-char port))))))) ]
                      [base (cond 
                              [(and (eq? sym '0x) (is-hex-digit? ch)) 16]
                              [(and (eq? sym '0b) (is-bin-digit? ch)) 2]
                              [(and (char=? ch #\0) (is-oct-digit? ch)) 8]
                              [else 10])]
                      [integer (string->number integer-string base)])
                 ;;; there are 2 cases when integer is #f
                 ;;; 1) the token is '0'
                 ;;; 2) the token format is wrong with given prefix '0x' '0b' or '0'
                 ;;; raise a exception when case 2 happened
                 (when (and (not (string=? integer-string "")) (not integer))
                   (error 'read-integer-literal 
                          "not a valid literal with given prefix"
                          (case sym 
                            [#f "0"]
                            [else (symbol->string sym)])
                          (case sym
                            ;;; when the integer prefix is '0', integer-string will includes the prefix
                            ;;; so here need to get its substring
                            [#f (substring integer-string 
                                           1 
                                           (string-length integer-string))]
                            [else integer-string])))
                 ;;; case 1 happened
                 (if integer
                   integer
                   0))))))))


(define tokenize
  (lambda (infn)
    (let* ([ip (oepn-file-input-port infn (file-options)
                                     (buffer-mode block) (native-transcoder))])
      (close-port ip))))

