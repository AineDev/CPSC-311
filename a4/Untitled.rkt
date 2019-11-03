#lang plai
;; -----------------------------------------------------------------------------
;; First, some required additions for our racket code:

(require racket/sandbox
         ;Also, re-import some macros from plai:
         (only-in plai
                  ;Rename plai's testX macros as plai-testX
                  [test plai-test]
                  [test/pred plai-test/pred]
                  [test/exn plai-test/exn]))

;; As usual, if you comment the print-only-errors call in the following
;; expression, you will see all the tests that also pass:
;(print-only-errors)

;; -----------------------------------------------------------------------------

;; Some macro definitions we require
;; Please do not remove these macros or your tests may fail!

;; These macros wrap over the original test functionality from
;; plai, but they fail if actual running
;; takes more than 5 seconds or more than 256mb of memory.
;; We recommend you use this when working with infinite lists
;; and recursion in case things go wrong.

(define-syntax-rule (test actual expected)
  (plai-test (with-limits 5 256 actual) expected))

(define-syntax-rule (test/exn actual msg)
  (plai-test/exn (with-limits 5 256 actual) msg))

(define-syntax-rule (test/pred actual pred)
  (plai-test/pred (with-limits 5 256 actual) pred))

;; We also include a macro that checks  an expression times out.
;; In general we cannot check if a program "runs forever", but we
;; will use this macro as an approximation of that behaviour.
(define-syntax-rule (test/must-timeout actual)
  (plai-test/exn
   ; We are wrapping the call to with-limits with a racket exception handler.
   ; You do not need to know the details of this implementation,
   ; but its behaviour is to throw the error only when with-limits fails
   ; (which happens when it runs out of resources)
   (with-handlers ([exn:fail:resource?
                    (lambda (original-error)
                      (error "timed out : ~s" original-error))])
     (with-limits 5 256 actual))
   "timed out"))

;; A4L : The Assignment 4 Language

;; Syntax specification:
;;
;; <A4L> ::= <num>
;;         | {+ <A4L> <A4L>}
;;         | {- <A4L> <A4L>}
;;         | {* <A4L> <A4L>}
;;         | {if0 <A4l> <A4L> <A4L>}
;;         | empty
;;         | {cons <A4L> <A4L>}
;;         | {match <A4L> as
;;             {{cons <id> <id>} => <A4L>}
;;             {empty => <A4L>}}
;;         | {withrec <defn> <A4L>}
;;         | {withrecs {<defns>} <A4L>}
;;         | {fun {<IDs>} <A4L>}
;;         | {<A4L> <A4Ls>}
;;         | <id>
;;         | {strict <A4L>}

;; <IDs> ::=
;;       | <id> <IDs>

;; <A4Ls> ::=
;;       | <A4L> <A4Ls>

;; <defn> ::= {<id> <A4L>}

;; <defns> ::=
;;           | <defn> <defns>

;; In this assignment, desugaring is again an explicit step.

; NOTE : YOU MUST NOT MAKE ANY CHANGES TO THIS DEFINE-TYPE!
(define-type s-A4L
  ; Numeric expressions
  [s-num (n number?)]
  [s-add (lhs s-A4L?) (rhs s-A4L?)]
  [s-sub (lhs s-A4L?) (rhs s-A4L?)]
  [s-mult (lhs s-A4L?) (rhs s-A4L?)]
  [s-if0 (scrutinee s-A4L?) (thens s-A4L?) (elses s-A4L?)]
  ; List expressions
  [s-empty]
  [s-cons (head s-A4L?) (tail s-A4L?)]
  [s-match (expr s-A4L?)
           (hd-name symbol?) (tl-name symbol?) (bdy-if-cons s-A4L?)
           (bdy-if-empty s-A4L?)]
  ; Recursion expressions
  [s-withrec (name symbol?) (named-expr s-A4L?) (body s-A4L?)]
  [s-withrecs (decls (listof (list/c symbol? s-A4L?))) (body s-A4L?)]
  ; Function application and definition expressions
  [s-fun (params (lambda (x) (andmap symbol? x))) (body s-A4L?)]
  [s-app (fun-exp s-A4L?) (arg-expr (lambda (x) (andmap s-A4L? x)))]
  [s-id (name symbol?)]
  ; Strictness point operator
  [s-strict (arg s-A4L?)])

;; Notice that fun and app look the same as last assignment after desugaring.
;; NOTE: YOU MUST NOT CHANGE THIS DEFINE-TYPE
;;       If you want to attempt the bonus question, do so in a separate file and
;;       in that file you may ONLY remove variants that become redundant.
(define-type A4L
  [num (n number?)]
  [binop (op procedure?) (l A4L?) (r A4L?)]
  [if0 (scrutinee A4L?) (then-expr A4L?) (else-expr A4L?)]
  [mt]
  [cns (hd A4L?) (tl A4L?)]
  [match-A4L (expr A4L?)
             (hd-id symbol?) (tl-id symbol?) (body-cons A4L?)
             (body-empty A4L?)]
  [withrec (name symbol?) (named-expr A4L?) (body A4L?)]
  [withrecs (defns (listof (list/c symbol? A4L?))) (body A4L?)]
  [fun (param symbol?) (body A4L?)] ;(s-fun (listof symbol) (A4L)) -> (fun (symbol) (A4L))
  [app (fun-expr A4L?) (arg-expr A4L?)] ; (s-app (listof A4L) (A4L)) -> (app (A4L) (A4L))
  [id (name symbol?)]
  [strict-A4L (arg A4L?)])

;; -----------------------------------------------------------------------------
;; PARSING 

(define *reserved-symbols*
  '(+ - * withrec withrecs fun empty cons match if0 strict))

;; valid-identifier? : any -> boolean
;; Determines whether the parameter is valid as an identifier name, i.e.,
;; a symbol that is not reserved.
(define (valid-identifier? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

;; parse : any -> s-A4L
(define (parse sexp)
  (match sexp
    [(? number?) (s-num sexp)]
    [(list '+ lhs rhs) (s-add (parse lhs) (parse rhs))]
    [(list '- lhs rhs) (s-sub (parse lhs) (parse rhs))]
    [(list '* lhs rhs) (s-mult (parse lhs) (parse rhs))]
    [(list 'if0 scrutinee then-s else-s)
     (s-if0 (parse scrutinee) (parse then-s) (parse else-s))]
    ['empty (s-empty)]
    [(list 'cons hs ts)
     (s-cons (parse hs) (parse ts))]
    [(list 'match expr 'as
           (list (list 'cons hd-id tl-id) '=> body1)
           (list 'empty '=> body2))
     (s-match (parse expr) hd-id tl-id (parse body1) (parse body2))]
    [(list 'strict arg) (s-strict (parse arg))]
    [(list 'withrec (list (? valid-identifier? name) named-exp) body)
     (s-withrec name (parse named-exp) (parse body))]
    [(list 'withrecs (list (list (? valid-identifier? names) named-exps) ...)
           body)
     (s-withrecs (map (λ (name named-exp)
                        (list name (parse named-exp))) names named-exps)
                 (parse body))]
    [(list 'fun (list (? valid-identifier? names) ...) body)
     (s-fun names (parse body))]
    [(? valid-identifier?) (s-id sexp)]
    ; For function calls to work they must be the last list expression matched.
    [(list fun-exp arg-exp ..1)
     (s-app (parse fun-exp) (map parse arg-exp))]
    [_ (error 'parse "unable to parse ~a" sexp)]))

;; -----------------------------------------------------------------------------
;; DESUGARING

(define (desugar exp)
  (type-case s-A4L exp
    [s-num (n) (num n)]
    [s-add (l r) (binop + (desugar l) (desugar r))]
    [s-sub (l r) (binop - (desugar l) (desugar r))]
    [s-mult (l r) (binop * (desugar l) (desugar r))]
    [s-if0 (scr thn els) (if0 (desugar scr) (desugar thn) (desugar els))]
    [s-empty () (mt)]
    [s-cons (hd tl) (cns (desugar hd) (desugar tl))]
    [s-match (e hd tl bc be)
             (match-A4L (desugar e) hd tl (desugar bc) (desugar be))]
    [s-withrec (name expr body) (withrec name (desugar expr) (desugar body))]
    [s-withrecs (defns body)
                (withrecs (map (λ (defn)
                                 (list (first defn)
                                       (desugar (second defn))))
                               defns)
                          (desugar body))]
    [s-fun (ids base)
           (foldr (lambda (x acc) (fun x acc))
                  (desugar base)
                  ids)]
    ;TODO: Fix the following incorrect desugaring for multiple arguments here.
    ;      It must be compatible with the desugaring that we have provided
    ;      For functions in the line before
    [s-app (fn definitions)
           (foldl (lambda (frst; value
                           rslt; rest of natural recursion
                           ) (app rslt
                              (desugar frst))) ; how to put them together
                  (desugar fn)
                  definitions)]
                                   
    ; (fun-exp s-A4L?) (arg-expr (lambda (x) (andmap s-A4L? x))) ; s-app format (s-app (s-A4L) (listof s-A4L))
    ; [app (fun-expr A4L?) (arg-expr A4L?)]                      ; app format (app (A4L) (app (A4L) (app (A4L) (A4L))))
    ; (app (desugar f) (desugar (last a)))                       ; original 
    [s-id (x) (id x)]
    [s-strict (a) (strict-A4L (desugar a))]))

;; -----------------------------------------------------------------------------
;; INTERPRETATION 

;; NOTE:  YOU MUST NOT CHANGE THIS DEFINE-TYPE
(define-type Value
  [numV (n number?)]
  ;; List-related values: First, an empty list:
  [mtV]
  ;; Cons type allows for infinite lists, as head and tail could be thunkVs.
  [cnsV (hd Value?) (tl Value?)]
  [closureV (param symbol?) (body A4L?) (env Env?)]
  ;; thunkVs are critical for the by need behaviour of this language.
  ;; They include a cache parameter to store computed values
  [thunkV (body A4L?) (env Env?) (cache (box/c (or/c false? Value?)))])

; Note: YOU MUST NOT CHANGE THIS DEFINE-TYPE
;;      If you want to attempt the bonus question, do so in a separate file and
;;      in that file you may ONLY remove variants that become redundant.
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value Value?) (env Env?)]
  ;; Recursive environment allowing self-reference
  [aRecEnv (name symbol?)
           (value (box/c (or/c false? Value?)))
           (env Env?)])

;; lookup : symbol Env -> Value
;; Find the value for a particular symbol.
(define (lookup name env)
  (type-case Env env
    [mtEnv () (error 'lookup (format "No binding for identifier ~s" name))]
    [anEnv (bound-name bound-value rest-env)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-env))]
    [aRecEnv (bound-name bound-value rest-env)
             (if (symbol=? bound-name name)
                 (local ([define box-contents (unbox bound-value)])
                   (if box-contents ; equivalent to (not (false? box-contents))
                       box-contents
                       (error (string-append "aRecEnv:  Attempted to access "
                                             "recursive value before it has "
                                             "been properly defined."))))
                 (lookup name rest-env))]))

;; interp : A4L -> Value
;; evaluate expr and produce the resulting Value
(define (interp expr)
  (local [
          ;; is-a? : apply pred to value and return value if true
          (define (is-a? pred value)
            (if (pred value)
                value
                (error 'interp (string-append "Received a value of the "
                                              "wrong type: ~a; expected ~a")
                       value pred)))

          ;; binop : check v1 and v2 are numVs, apply op, and return a numV
          (define (interp-binop op v1 v2)
            (numV (op (numV-n (is-a? numV? v1))
                      (numV-n (is-a? numV? v2)))))
          
          ;; strict : Value -> Value (except thunkV)
          (define (strict v)
            (type-case Value v
              ; Check if we had evaluated the argument before
              [thunkV (expr env cache)
                      (local ([define cache-contents (unbox cache)])
                        (if cache-contents
                            ; Cache is not #f, so we had evaluated before!
                            ; Return the contents:
                            cache-contents
                            ; Cache is #f, so we must first evaluate,
                            ; set the cache and then return the value
                            (begin
                              (set-box! cache (strict (helper expr env)))
                              ;Return the new cache value
                              (unbox cache))))]
              [else v]))

          ;; cyclically-bind-and-interp : symbol A4L Env -> Env
          ;; create the appropriate environment for evaluating a single
          ;; recursive function.
          ;; we have defined a special aRecEnv deferred substitution in Env
          ;; for recursion
          (define (cyclically-bind-and-interp name bound-expr env)
            (local ([define box-for-name (box #f)]
                    [define new-env (aRecEnv name box-for-name env)]
                    [define bound-val (helper bound-expr new-env)])
              (begin
                (set-box! box-for-name bound-val)
                new-env)))

          ;; mutually-cyclically-bind-and-interp
          ;;           : (listof (list/c symbol A4L)) Env -> Env
          ;; All definitions in decls may refer to any of the other recursive
          ;; definitions being simultaneously defined.
          (define (mutually-cyclically-bind-and-interp decls env)
            ; TODO : Implement this definition
            (local ([define env-2 (cyclically-bind-and-interp (first (first decls)) (first (second decls)) env)])
              ;base case?
              (cond [(empty? decls) env]
                    [else
                     (mutually-cyclically-bind-and-interp (rest decls) env-2)])))
              
          ; helper : A4L Env -> Value  
          (define (helper expr env)
            (type-case A4L expr
              [num (n) (numV n)]
              [binop (op l r) (interp-binop op
                                            (strict (helper l env))
                                            (strict (helper r env)))]
              ; TODO : We have gone too far on the laziness of if0:
              ;        This needs fixing.
              ;        Keep in mind: What is the minimum amount of strictness
              ;                      that would make this work?
              [if0 (scr thn els)
                   (if (= 0 (numV-n (is-a? numV? (strict (helper scr env)))))
                        (helper thn env)
                        (helper els env))]
              [mt () [mtV]]
              ; Remember this is a by-need cons cell: It should not evaluate
              ; its head or tail unless you use strict on them /later/.
              ; TODO : Make this cons cell to work in a by-need fashion for the
              ;        infinite lists to work!
              [cns (hd tl)
                   (cnsV (thunkV hd env (box false)) (thunkV tl env (box false)))]
              [match-A4L (matched-expr
                          cons-hd-id cons-tl-id body-cons
                          body-empty)
                         ; Evaluate matched expression in our language
                         (type-case Value (strict (helper matched-expr env))
                           ; We can now run the pattern matching behaviour:
                           [cnsV (hd-contents tl-contents)
                                 (helper body-cons
                                         (anEnv cons-hd-id hd-contents
                                                (anEnv cons-tl-id tl-contents
                                                       env)))]
                           ; TODO : Fix the missing case here!
                           [else (error 'interp
                                        "Attempted to match a non-list value")])]
              
              [withrec (name named-expr bound-body)
                       (helper bound-body
                               (cyclically-bind-and-interp name named-expr env))]
              [withrecs (decls bound-body)
                        ; Note: You should not need to make any changes here
                        ;       just properly implement the helper function.
                        (helper bound-body
                                (mutually-cyclically-bind-and-interp decls env))]
              [fun (x body) (closureV x body env)]
              [app (fun-expr arg-expr)
                   (local ([define fun-val (strict (helper fun-expr env))]
                           [define arg-val (thunkV arg-expr env (box false))])
                     (type-case Value fun-val
                       [closureV (arg body clo-env)
                                 (helper body (anEnv arg arg-val clo-env))]
                       [else (error 'interp
                                    "Not a function in a function call: got ~s"
                                    fun-val)]))]
              [id (v) (lookup v env)]
              ; With strict-A4L, we can create strictness points wherever
              ; we so desire in the language.  HERE is where we force to
              ; evaluate the head and the tail of a cons cell:
              [strict-A4L (arg)
                          (local ([define arg-val (strict (helper arg env))])
                            (type-case Value arg-val
                              ; If it is a cons cell, force strictness on
                              ; its head and tail components: 
                              [cnsV (hd tl)
                                    (cnsV (strict hd)
                                          (strict tl))]
                              [else arg-val]))
                          ]))]
    (helper expr (mtEnv))))

; TODO : Fill the implementation of a map function (you can use map as the name
;        of the function for recursive calls
(define A4L-map
  (parse '{fun {f l}
               TODO-fixme}))

; TODO : Fill the implementation of a take-n function (you can use take-n as the name
;        of the function for recursive calls
(define A4L-take-n
  (parse '{fun {n l}
               {if0 n
                    empty ; n == 0 (base case)
                    {match l as ; n > 0 (recurse on tail)
                      {{cons hd tl}
                       =>
                       {strict {cons hd {take-n {- n 1} tl}}}}
                      {empty => empty}}}}))

; TODO : Fill the implementation of a drop-n function (you can use drop-n as the name
;        of the function for recursive calls
(define A4L-drop-n
  (parse '{fun {n l}
               TODO-fixme}))

; TODO : Fill the implementation of a skip function (you can use skip as the name
;        of the function for recursive calls

(define A4L-skip
  (parse
   '{fun {l}
         TODO-fixme}))
                        
; Tests

;; A shortcut: parse, desugar, and run the program
;; from the given piece of concrete syntax
(define (run sexp)
  (interp (desugar (parse sexp))))

;; fun and app
(test (run '{{fun {x} {+ x 1}} 2}) (numV 3))
(test (run '{{fun {x y z} {+ x {- y z}}} 1 2 3}) (numV 0))
(test (run '{{fun {f x} {f x}} {fun {y} {+ y 1}} 2}) (numV 3))
(test (run '{strict {{fun {f x} x} z {+ 35 9}}}) (numV 44))
(test (run '{{fun {f x} x} z {+ 35 8}})
      (thunkV (binop + (num 35) (num 8)) (mtEnv) (box #f)))
(test/exn (run '{{fun {x} x} 1 2 3}) "")
(test (run '{{fun {x y} {+ x y}} 1})
      (closureV 'y (binop + (id 'x) (id 'y))
                (anEnv 'x (thunkV (num 1) (mtEnv) (box #f)) (mtEnv))))
(test/exn (parse '{f}) "")

(test (run '{{fun {x} {if0 x 5 7}} 0}) (numV 5))
(test (run '{{fun {y} {if0 y z 10}} 3}) (numV 10))

(test (run '{match {cons 5 6} as
              {{cons n1 n2} => {+ n1 n2}}
              {empty => {+ x {+ y z}}}})
      (numV 11))

(test (run '{match empty as
              {{cons n1 n2} => {+ hd tl}}
              {empty => {- 0 {* 5 8}}}})
      (numV -40))


(test/pred (run '{withrec {list-gen {fun {n} {cons n {list-gen n}}}}
                          {list-gen 10}})
           (λ (result)
             (and (cnsV? result)
                  (not (equal? (numV 10) (cnsV-hd result)))
                  (not (cnsV? (cnsV-tl result))))))

(test/pred (run '{withrec {list-gen {fun {n} {cons n {list-gen n}}}}
                          {strict {list-gen {+ 5 5}}}})
           (λ (result)
             (and (cnsV? result)
                  (equal? (numV 10) (cnsV-hd result))
                  (cnsV? (cnsV-tl result))
                  (not (eq? (numV 10) (cnsV-hd (cnsV-tl result))))
                  (not (cnsV? (cnsV-tl (cnsV-tl result)))))))
; with-rec
(test (interp (desugar (parse '{withrecs {{triple {fun {x} {+ x {double x}}}}
                                          {double {fun {x} {+ x x}}}}
                                         {triple 5}}))) (numV 15))

(test (interp (desugar (parse '{withrecs {{f {fun {x} {+ x 1}}}} {f 2}}))) (numV 3))

; This test must pass because f and y are both defined in a mutual scope.
(test (interp (desugar (parse '{withrecs {{f {fun {x} {+ x y}}}
                                          {y 1}}
                                         {f 2}}))) (numV 3))

(test (run '{withrecs {{x 1}} x}) (numV 1))
(test (run '{withrec {a 42}
                     {withrecs {} a}}) (numV 42))

;; factorial, in a complicated mutually-recursive way.
(test (run '{withrecs
             {{fact {fun {n} {if0 {- n 1} 1 {* n {fact0 {- n 1}}}}}}
              {fact1 {fun {n} {if0 {- n 1} 1 {* n {fact2 {- n 1}}}}}}
              {fact0 {fun {n} {if0 {- n 1} 1 {* n {fact1 {- n 1}}}}}}
              {fact2 {fun {n} {if0 {- n 1} 1 {* n {fact {- n 1}}}}}}}
             {fact 10}}) (numV 3628800))

;; match
(test (interp (desugar (parse '{match {cons 1 empty} as
                                 {{cons hd tl} => 3}
                                 {empty => 4}}))) (numV 3))

(test (interp (desugar (parse '{match empty as
                                 {{cons hd tl} => 3}
                                 {empty => 4}}))) (numV 4))

(test (interp (desugar (parse '{withrec {f {fun {x} {+ x 1}}}
                                        {match {cons 1 {cons 2 empty}} as
                                          {{cons hd tl} => {f hd}}
                                          {empty => 0}}}))) (numV 2))

(test/exn (interp (desugar (parse '{withrecs {{x y}
                                              {y x}}
                                             {+ x y}})))
          "")

;; determine whether an input value is even or odd
(test (interp (desugar (parse '{withrecs {{even {fun {n} {if0 n 1 {odd {- n 1}}}}}
                                          {odd {fun {n} {if0 n 0 {even {- n 1}}}}}}
                                         {even 5}}))) (numV 0))
(test (interp (desugar (parse '{withrecs {{even {fun {n} {if0 n 1 {odd {- n 1}}}}}
                                          {odd {fun {n} {if0 n 0 {even {- n 1}}}}}}
                                         {even 500}}))) (numV 1))

(test (interp (desugar (parse '{withrecs {{even {fun {n} {if0 n 1 {odd {- n 1}}}}}
                                          {odd {fun {n} {if0 n 0 {even {- n 1}}}}}}
                                         {odd 23}}))) (numV 1))

(test/must-timeout
 (interp
  (desugar (parse '{withrecs {{even {fun {n} {if0 n 1 {odd {- n 1}}}}}
                              {odd {fun {n} {if0 n 0 {even {- n 1}}}}}}
                             {even -1}}))))

;; define list [1], take 1 element, return it via general add all elements function
(test (interp (desugar (parse
                        '{withrecs
                          {{add-cons
                            {fun {l}
                                 {match l as
                                   {{cons hd tl} => {+ hd {add-cons tl}}}
                                   {empty => 0}}}}
                           {take-n {fun {n l}
                                        {if0 n
                                             empty ; n == 0 (base case)
                                             {match l as ; n > 0 (recurse on tail)
                                               {{cons hd tl}
                                                =>
                                                {cons hd {take-n {- n 1} tl}}}
                                               {empty => empty}}}}}}
                          {add-cons {take-n 1 {cons 1 empty}}}}))) (numV 1))

;; Define a list of ones and force the list.
(test (interp (desugar (parse
                        '{withrec {ones {cons 1 ones}}
                                  {withrec
                                   {take-n
                                    {fun {n l}
                                         {if0 n
                                              empty ; n == 0 (base case)
                                              {match l as ; n > 0 (recurse on tail)
                                                {{cons hd tl}
                                                 =>
                                                 {strict {cons hd {take-n {- n 1} tl}}}}
                                                {empty => empty}}}}}
                                   {strict {take-n 3 ones}}}})))
      (cnsV (numV 1)
            (cnsV (numV 1)
                  (cnsV (numV 1) (mtV)))))
;; define list [1,3], take 2 elements, add them
(test(interp (desugar (parse
                       '{withrec {add-cons
                                  {fun {l}
                                       {match l as
                                         {{cons hd tl} => {+ hd {add-cons tl}}}
                                         {empty => 0}}}}
                                 {withrec
                                  {take-n
                                   {fun {n l}
                                        {if0 n
                                             empty
                                             {match l as
                                               {{cons hd tl}
                                                =>
                                                {cons hd {take-n {- n 1} tl}}}
                                               {empty => empty}}}}}
                                  {add-cons
                                   {take-n 2 
                                           {cons 1 {cons 2 empty}}}}}})))
     (numV 3))

;; define infinite list of ones, take 5 elements, add them
(test
 (interp
  (desugar
   (s-withrec 'take-n A4L-take-n
              (parse
               '{withrec {ones {cons 1 ones}}
                         {withrec
                          {add-cons
                           {fun {l}
                                {match l as
                                  {{cons hd tl} => {+ hd {add-cons tl}}}
                                  {empty => 0}}}}
                          {add-cons {take-n 5 ones}}}})))) (numV 5))


(test/must-timeout
 (run '{withrecs {{ones {cons 1 ones}}
                  {strict-list {fun {x}
                                    {match x as
                                      {{cons hd tl} => {strict {cons hd
                                                                     {strict-list tl}}}}
                                      {empty => empty}}}}}
                 {strict-list ones}}))

; Test: If we do not do strict, in the body of strict-list, we should not
;       evaluate beyond the cons pair.

(test/pred
 (run '{withrecs {{ones {cons 1 ones}}
                  {strict-list {fun {x}
                                    {match x as
                                      {{cons hd tl} => {cons hd
                                                             {strict-list tl}}}
                                      {empty => empty}}}}}
                 {strict-list ones}})
 (lambda (result)
   (and (cnsV? result)
        (thunkV? (cnsV-hd result))
        (thunkV? (cnsV-tl result)))))

(test (interp
       (desugar
        (s-withrecs `((map ,A4L-map))
                    (parse
                     '{withrecs
                       {{add1 {fun {x} {+ x 1}}}
                        {naturals {cons 0 {map {fun {x} {+ x 1}} naturals}}}}
                       {match {map add1 {map add1 {map add1 naturals}}} as
                         {{cons hd tl} => {strict hd}}
                         {empty => 50}}}))))
      (numV 3))

(test (interp
       (desugar
        (s-withrecs `((map ,A4L-map))
                    (parse
                     '{withrecs
                       {{add1 {fun {x} {+ x 1}}}
                        {naturals {cons 0 {map {fun {x} {+ x 1}} naturals}}}}
                       {match {map add1 {map add1 {map add1 naturals}}} as
                         {{cons hd tl}
                          => {match tl as
                               {{cons hd tl} => {strict hd}}
                               {empty => 50}}}
                         {empty => empty}}}))))
      (numV 4))

(test (interp
       (desugar
        (s-withrecs `((map ,A4L-map)
                      (take-n ,A4L-take-n))
                    (parse
                     '{withrec {naturals {cons 0 {map {fun {x} {+ x 1}} naturals}}}
                               {strict {take-n 3 naturals}}}))))
      (cnsV (numV 0)
            (cnsV (numV 1)
                  (cnsV (numV 2) (mtV)))))
(test (interp
       (desugar
        (s-withrecs
         `((map ,A4L-map)
           (take-n ,A4L-take-n)
           (skip ,A4L-skip))
         (parse
          '{withrec {naturals {cons 0 {map {fun {x} {+ x 1}} naturals}}}      
                    {strict {take-n 3 {skip naturals}}}}))))
      (cnsV (numV 0)
            (cnsV (numV 2)
                  (cnsV (numV 4) (mtV)))))

(test (interp
       (desugar
        (s-withrecs
         `((map ,A4L-map)
           (take-n ,A4L-take-n)
           (drop-n ,A4L-drop-n)
           (skip ,A4L-skip))
         (parse
          '{withrec {naturals {cons 0 {map {fun {x} {+ x 1}} naturals}}}      
                    {strict {take-n 3 {drop-n 6 {skip naturals}}}}}))))
      (cnsV (numV 12)
            (cnsV (numV 14)
                  (cnsV (numV 16) (mtV)))))