#lang plai
; uncomment the following line to hide all passing tests
(print-only-errors)

;; ==========================================================
;;                     EBNF & DEFINE-TYPES
;; ==========================================================

; Assignment 2 : With names and renames. 
;
;   <ReWAE> ::= <number>
;          | { + <ReWAE> <ReWAE> }
;          | { - <ReWAE> <ReWAE> }
;          | { * <ReWAE> <ReWAE> }
;          | { begin <ReWAE> <ReWAES> }
;          | { if0 <ReWAE> then <ReWAE> else <ReWAE> }
;          | { with { <id> <ReWAE> } <ReWAE> }
;          | { lazy-with { <id> <ReWAE> } <ReWAE> }
;          | { rename { <id> as <id> } in <ReWAE> }
;          | { with* { <WithMappings> } <ReWAE> }
;          | <id>
;
;    <WithMappings> ::=
;                     | { <id> <ReWAE> } <WithMappings>
;
;    <ReWAES> ::=
;               |  <ReWAE> <ReWAES>


; This will be the abstract syntax of our language.
; REMINDER: YOU CANNOT MAKE ANY CHANGES TO THE DEFINE-TYPE
; We will take marks for changes in this definition.

(define-type ReWAE     
  [num (n number?)]
  [binop (op procedure?) (lhs ReWAE?) (rhs ReWAE?)]
  [if0 (scrutinee ReWAE?) (then-expr ReWAE?) (else-expr ReWAE?)] 
  [with (id symbol?) (named-expr ReWAE?) (body ReWAE?)]
  [lazy-with (id symbol?) (named-expr ReWAE?) (body ReWAE?)] ; TODO: probably different in the interp
  [rename-as (source-id symbol?) (dest-id symbol?) (body ReWAE?)]
  [begin-exp (exprs (non-empty-listof ReWAE?))]
  [id (name symbol?)]
  )



;; ==========================================================
;;                           PARSE
;; ==========================================================
; TODO You must complete this list of reserved symbols.
(define *reserved-symbols* '(+ - with begin num * if0 then else lazy-with rename-as as in with*))

;(define *reserved-symbols* '(+ - with num * if0 then else lazy-with rename-as as in begin-exp id with*))

;; We invite you to reuse the valid-identifier predicate that we have
;; seen in lectures before:

;; valid-identifier? : any -> boolean
;; Determines whether the parameter is valid as an identifier name, i.e.,
;; a symbol that is not reserved.
(define (valid-identifier? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

;; Reserved symbols.
(test (valid-identifier? 'lazy-with) false)
(test (valid-identifier? 'else) false)


;; TODO: Implement this function.

;; parse : any -> ReWAE
;; Consumes an s-expression (in ReWAE's concrete syntax)
;; and generates the corresponding ReWAE program.

(define (parse sexp)
  (match sexp
    [(list 'with* '() body) (parse body)]
    [(list 'with* (list (cons (? valid-identifier? id) (list named-exprs)) exprs ...) body) ; TODO test this with rename and nested withs
     (with id (parse named-exprs) (parse (list 'with* exprs body)))]
    [(? valid-identifier?) (id sexp)]
    [(? number?) (num sexp)]
    [(list '+ lhs rhs) (binop + (parse lhs) (parse rhs))]
    [(list '* lhs rhs) (binop * (parse lhs) (parse rhs))]
    [(list '- lhs rhs) (binop - (parse lhs) (parse rhs))]
    [(list 'with (list (? valid-identifier? id) named-expr) body)
     (with id (parse named-expr) (parse body))]
    [(list 'lazy-with (list (? valid-identifier? id) named-expr) body)
     (lazy-with id (parse named-expr) (parse body))]
    [(list 'begin first-expr second-expr)
     (begin-exp (list (parse first-expr) (parse second-expr)))]
    [(list 'rename
           (list (? valid-identifier? source-id) 'as (? valid-identifier? dest-id)) 'in  body)
     (rename-as source-id  dest-id  (parse body))]
    [(list 'if0 scrutinee 'then then-expr 'else else-expr)
     (if0 (parse scrutinee) (parse then-expr) (parse else-expr))]
    [_ (error 'parse "unable to parse ~a" sexp)]))

; with*
(test (parse '{with* {{x {+ 1 2}}}
                     x})
      (with 'x (binop + (num 1) (num 2))
            (id 'x)))

(test (parse '{with* {{x {+ 1 2}}
                      {y {+ x 1}}}
                     {+ x y}})
      (with 'x (binop + (num 1) (num 2))
            (with 'y (binop + (id 'x) (num 1)) (binop + (id 'x) (id'y)))))

(test (parse '{begin
                {+ 4 3}
                {- 3 2}})
      (begin-exp
        (list (binop + (num 4) (num 3))
              (binop - (num 3) (num 2)))))

; We STRONGLY recommend you to add more tests to this function!
; TODO: Test that it works recursively & think of edge cases (like symbols that shouldn't be there)
(test (parse 'x) (id 'x))
(test/exn (parse '+) "")
(test/exn (parse '{-}) "")
(test/exn (parse '{* 1}) "")
(test/exn (parse '{+ 1 2 3}) "")

; ill-structured lazy-with
(test/exn (parse '{lazy-with}) "")
(test/exn (parse '{lazy-with x}) "")
(test/exn (parse '{lazy-with x 2 3}) "")
(test/exn (parse '{lazy-with {x 1}}) "")
(test/exn (parse '{lazy-with {x 1} 2 3}) "")
(test/exn (parse '{lazy-with {x 1 2} 3}) "")
(test/exn (parse '{lazy-with {+ 1} 2}) "")

; + (and -/with) with non-AEs as arguments
(test/exn (parse '{lazy-with {x "foo"} x}) "")
(test/exn (parse '{lazy-with {x 1} "foo"}) "")

(test (parse '{if0 {+ 3 1}
                   then -1
                   else 5})
      (if0 (binop + (num 3) (num 1))
           (num -1)
           (num 5)))

(test (parse '{if0 {+ 3 1}
                   then -1
                   else {if0 {+ -7 7}
                             then 8
                             else 1}})
      (if0 (binop + (num 3) (num 1))
           (num -1)
           (if0 (binop + (num -7) (num 7))
                (num 8)
                (num 1))))

(test (parse '{if0 7 then 1 else 4})
      (if0 (num 7) (num 1) (num 4)))

(test/exn (parse '{if {+ -5 'then 1}
                      0}) "")
(test/exn (parse '{if0 0}) "")
(test/exn (parse '{if0}) "")
(test/exn (parse '{if0 9 'then 1 'else 4 3}) "")

                  
(test (parse '{with {x 1} x})
      (with 'x (num 1) (id 'x)))
(test (parse '{with {x 1} {with {x 2} x}})
      (with 'x (num 1)
            (with 'x (num 2) (id 'x))))
(test (parse '{lazy-with {x 1} x})
      (lazy-with 'x (num 1) (id 'x)))
(test (parse '{lazy-with {x 1} {lazy-with {x 2} x}})
      (lazy-with 'x (num 1)
                 (lazy-with 'x (num 2) (id 'x))))

(test (parse '{rename {x as y} in
                      x})
      (rename-as 'x 'y (id 'x)))

(test (parse '{with {x 10}
                    {rename {x as y} in
                            x}})
      (with 'x (num 10)
            (rename-as 'x 'y (id 'x))))

(test (parse '{* {- 1 2} {+ 3 4}})
      (binop *
             (binop - (num 1) (num 2))
             (binop + (num 3) (num 4))))

; These tests check for errors generated explicitly by you via (error ...)
; TODO: write more tests for checking that parse generates errors when needed
(test/exn (parse "I am a string, not a symbol") "")
(test/exn (parse '{with {} 1}) "")
(test/exn (parse '{with {{1 x}} x}) "")
(test/exn (parse '{with* {x 1} x}) "") ; TODO: Test not passing
(test/exn (parse 'with) "")
(test/exn (parse '{a b c}) "")


;; ==========================================================
;;                           INTERP
;; ==========================================================

;; subst : ReWAE symbol ReWAE -> ReWAE
;; substitute out target-id for val everywhere it
;; is refenced "free" in target-exp.
(define (subst target-exp target-id val)
  (type-case ReWAE target-exp
    [num (n) target-exp]
    [id (n) (if (symbol=? n target-id) val target-exp)]
    [binop (op lhs rhs) (binop op (subst lhs target-id val)
                               (subst rhs target-id val))]
    [if0 (scrutinee then-expr else-expr) (if0 (subst scrutinee target-id val)
                                              (subst then-expr target-id val)
                                              (subst else-expr target-id val))]
    [with (id named-expr body)
          (if (symbol=? id target-id)
              (with id
                    (subst named-expr target-id val)
                    body)
              (with id
                    (subst named-expr target-id val)
                    (subst body target-id val)))]
    [lazy-with (id named-expr body)
               (if (symbol=? id target-id)
                   (lazy-with id
                              (subst named-expr target-id val)
                              body)
                   (lazy-with id
                              (subst named-expr target-id val)
                              (subst body target-id val)))]
    [rename-as (source-id dest-id body)
               (rename-as (if (equal? target-id source-id)
                              val
                              source-id)
                          ; subst the body with dest and the val
                          
                          dest-id
                          (subst body target-id val))]
    [begin-exp (exprs)
               (begin-exp (map (λ (exp) (subst exp target-id val)) exprs))]
    ;[else target-exp]
    ))

; rename-as
#;#;#;#;
(test (subst (with 'x (num 10)
                   (rename-as 'x 'y (id 'y)))
             'x
             (id 'z))
      (with 'x (num 10)
            (rename-as 'x 'y (id 'y))))
(test (subst (with 'x (num 10)
                   (rename-as 'x 'y (id 'y)))
             'y
             (id 'z))
      (with 'x (num 10)
            (rename-as 'x 'z (id 'z))))

#;
(test (subst (with 'x (num 10)
                   (rename-as 'x 'y (id 'x)))
             'z
             (num 11))
      (with 'x (num 10)
            (rename-as 'x 'y (id 'x)))) ; TODOshould be illigal - test parse for htis
(test (subst (with 'x (num 10)
                   (rename-as 'x 'y (id 'x)))
             'y
             (num 11))
      (with 'x (num 10)
            (rename-as 'x 'y (id 'x)))) ; nothing should happen
(test (subst (with 'z (num 10)
                   (rename-as 'x 'y (id 'x))) ; (id 'x) should be replaced
             'x
             (num 11))
      (with 'z (num 10)
            (rename-as 'x 'y (num 11))))
(test (subst (with 'z (num 10)
                   (rename-as 'x 'y (id 'x))) ; (id 'x) should be replaced
             'x
             (num 11))
      (with 'z (num 10)
            (rename-as 'x 'y (id 'x))))

; begin-exp
(test (subst (begin-exp
               (list (binop + (num 4) (num 3))
                     (binop - (num 3) (num 2))))
             'x
             (num 3))
      (begin-exp (list (binop + (num 4) (num 3))
                       (binop - (num 3) (num 2)))))
(test (subst (begin-exp
               (list (binop * (id 'x) (num 2))
                     (binop - (num 4) (id 'x))))
             'x
             (num 7))
      (begin-exp
        (list (binop * (num 7) (num 2))
              (binop - (num 4) (num 7)))))

; if0
(test (subst (if0 (id 'x)
                  (id 'x)
                  (id 'x))
             'x
             (num 3))
      (if0 (num 3)
           (num 3)
           (num 3)))

(test (subst (if0 (binop + (num 3) (num 1))
                  (id 'x)
                  (id 'x))
             'x
             (num 4))
      (if0 (binop + (num 3) (num 1))
           (num 4)
           (num 4)))

(test (subst (if0 (binop * (id 'x) (num 2))
                  (binop - (id 'x) (num 1))
                  (binop + (id 'x) (num 2)))
             'x
             (num 6000))
      (if0 (binop * (num 6000) (num 2))
           (binop - (num 6000) (num 1))
           (binop + (num 6000) (num 2))))

; with
(test (subst (with 'x (num 3) (num 3))
             'y
             (num 3))
      (with 'x (num 3) (num 3)))
(test (subst (with 'x (num 3) (id 'y))
             'y
             (num 3))
      (with 'x (num 3) (num 3)))
(test (subst (with 'x (num 3) (id 'x))
             'x
             (num 3)) (with 'x (num 3) (id 'x)))
(test (subst (with 'x (id 'x) (id 'x))
             'x
             (num 1))
      (with 'x (num 1) (id 'x)))
(test (subst (with 'y (id 'x) (id 'x))
             'x
             (num 1))
      (with 'y (num 1) (num 1)))
(test (subst (with 'x (num 1)
                   (with 'y (num 2)
                         (binop + (id 'x) (id 'y))))
             'z
             (num 3))
      (with 'x (num 1)
            (with 'y (num 2)
                  (binop + (id 'x) (id 'y)))))
(test (subst (with 'x (num 1)
                   (with 'y (num 2) (binop + (id 'x) (id 'y)))) 'x (num 3))
      (with 'x (num 1)
            (with 'y (num 2) (binop + (id 'x) (id 'y)))))

(test (subst (lazy-with 'x (num 3) (id 'x)) 'x (num 3)) (lazy-with 'x (num 3) (id 'x)))
(test (subst (lazy-with 'x (id 'x) (id 'x)) 'x (num 1)) (lazy-with 'x (num 1) (id 'x)))
(test (subst (lazy-with 'y (id 'x) (id 'x)) 'x (num 1)) (lazy-with 'y (num 1) (num 1)))

(test (subst (num 10) 'x (num 1)) (num 10))
(test (subst (id 'x) 'x (num 2)) (num 2))
(test (subst (id 'y) 'x (num 3)) (id 'y))

; + / - / *
(test (subst (binop + (id 'x) (id 'x)) 'x (num 2)) (binop + (num 2) (num 2)))

(test (subst (binop + (id 'x) (id 'y)) 'x (num 5)) (binop + (num 5) (id 'y)))
(test (subst (binop + (num 4) (id 'x)) 'x (num 5)) (binop + (num 4) (num 5)))
(test (subst (binop + (id 'x) (num 4)) 'x (num 5)) (binop + (num 5) (num 4)))
(test (subst (binop - (id 'x) (id 'x)) 'x (num 2)) (binop - (num 2) (num 2)))
(test (subst (binop - (id 'x) (id 'y)) 'x (num 5)) (binop - (num 5) (id 'y)))
(test (subst (binop * (id 'x) (id 'x)) 'x (num 2)) (binop * (num 2) (num 2)))
(test (subst (binop * (id 'x) (id 'y)) 'x (num 5)) (binop * (num 5) (id 'y)))


;; TODO The test provided is just an example.
;;      You must provide more tests for subst!
; consider {rename {x as x} in ...}


;; TODO you must also write the interp function!

;; interp : ReWAE -> number
;; consumes a ReWAE and computes the corresponding number
(define (interp an-ae)
  (type-case ReWAE an-ae
    [num (n) n]
    [binop (op lhs rhs) (op (interp lhs) (interp rhs))]
    [if0 (scrutinee then-expr else-expr) (if (equal? (interp scrutinee) 0)
                                             (interp then-expr)
                                             (interp else-expr))]
    [with (id named-expr body)
          (interp (subst body id (num (interp named-expr))))
          ]
    [lazy-with (id named-expr body) (interp (subst body id named-expr))]
    [rename-as (source-id dest-id body)
               (interp (subst body dest-id source-id))
               ]
    [begin-exp (exprs)
               (begin (map (λ (expr) (interp expr)) exprs)
                      (interp (last exprs)))]
    [id (n) (error 'parse "unable to evaluate ~a" n)]))


; TODO: write more tests for interp as needed

; rename-as
(test (interp (with 'x (num 4) (rename-as 'x 'y (binop + (id 'y) (num 1)))))
      5)

; begin-exp
(test (interp (begin-exp (list (num 8)))) 8)
(test (interp (begin-exp (list (num 9) (num 34)))) 34)
(test (interp (begin-exp (list (binop * (num 4) (num 4))
                               (binop + (num 4) (num 4))
                               (binop - (num 4) (num 4)))))
      0)

(test (interp (num 1)) 1)

; if0
(test (interp (if0 (num 9) (num 5) (num 3))) 3)
(test (interp (if0 (binop + (num -3) (num 3))
                   (binop - (num 6) (num 40))
                   (num 3)))
      (- 6 40))
(test (interp (if0 (binop + (num -6) (num 3))
                   (num 3)
                   (binop * (num 6) (num 40))))
      (* 6 40))

; given tests
(test (interp (with 'x (num 1) (id 'x))) 1)
(test (interp (binop - (num 8) (num 3))) 5)
(test (interp (binop -
                     (binop * (num 5) (num 6))
                     (binop + (num -10) (num 2))))
      38)
(test (interp (with 'x (num 1) (num 2))) 2)
(test (interp (with 'x (binop + (num 1) (num 2)) (id 'x))) 3)
(test (interp (with 'x (num 1) (id 'x))) 1)
(test (interp (with 'x (num 1)
                    (with 'x (num 2) (id 'x)))) 2)
(test (interp (with 'x (num 1)
                    (with 'x (id 'x) (id 'x))))
      1)

(test (interp (parse '{with* { {y 2} {x 1} } {rename {x as y} in {* 5 y}}}))
      5)
(test/exn (interp (parse '{rename {x as y} in 1}))
          "")
(test (interp (parse '{with {X 1} {rename {X as Y} in {+ Y 1}}}))
      2)
(test (interp (parse '{with* { {X 1} {Y 3}} {rename {X as Y} in {+ Y 1}}}))
      2)
(test/exn (interp (parse '{with {X 1} {rename {X as Y} in {+ X 1}}}))
          "")
(test (interp (parse '{lazy-with {a b} {with {b 10} a}}))
      10)

(test/exn (interp (id 'y)) "")