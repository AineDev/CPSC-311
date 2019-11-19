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

;; A5L : The Assignment 5 Language

;; Syntax specification:
;;
;; <A5L> ::= <num>
;;         | <string>
;;         | true | false
;;         | {error <string>}
;;         | {+ <A5L> <A5L>}
;;         | {- <A5L> <A5L>}
;;         | {* <A5L> <A5L>}
;;         | {= <A5L> <A5L>}
;;         | {if <A5L> <A5L> <A5L>}
;;         | {withrec <defn> <A5L>}
;;         | {withrecs {<defns>} <A5L>}
;;         | {fun {<IDs>} <A5L>}
;;         | {<A5L> <A5Ls>}
;;         | <id>
;;         | {OBJECT {<Fields>} {<Methods>}}
;;         | {OBJECT-DEL <A5L> {<Fields>} {<Methods>}}
;;         | {-> <A5L> <id> <A5Ls>}

;; <IDs> ::=
;;       | <id> <IDs>

;; <A5Ls> ::=
;;       | <A5L> <A5Ls>

;; <defn> ::= {<id> <A5L>}

;; <defns> ::=
;;           | <defn> <defns>

;; <Fields> ::=
;;            | <Field> <Fields>
;; <Field> ::= {field <id> <A5L>}

;; <Methods> ::=
;;             | <Method> <Methods>
;; <Method> ::= {method <id> {<IDs>} <A5L>}

;; In this assignment, desugaring is again an explicit step.

; NOTE : YOU MUST NOT MAKE ANY CHANGES TO THIS DEFINE-TYPE!
(define-type s-A5L
  ; Numeric expressions
  [s-num (n number?)]
  [s-bool (b boolean?)]
  [s-str (s string?)]
  [s-add (lhs s-A5L?) (rhs s-A5L?)]
  [s-sub (lhs s-A5L?) (rhs s-A5L?)]
  [s-mult (lhs s-A5L?) (rhs s-A5L?)]
  [s-= (lhs s-A5L?) (rhs s-A5L?)]
  [s-if (scrutinee s-A5L?) (thens s-A5L?) (elses s-A5L?)]
  ; Recursion expressions
  [s-withrec (name symbol?) (named-expr s-A5L?) (body s-A5L?)]
  [s-withrecs (decls (listof (list/c symbol? s-A5L?))) (body s-A5L?)]
  ; Function application and definition expressions
  [s-fun (params (lambda (x) (andmap symbol? x))) (body s-A5L?)]
  [s-app (fun-exp s-A5L?) (arg-expr (lambda (x) (andmap s-A5L? x)))]
  [s-id (name symbol?)]
  ; Object declaration
  [s-object (fields (listof s-field?)) (methods (listof s-method?))]
  [s-object-del (parent s-A5L?) (fields (listof s-field?)) (methods (listof s-method?))]
  ; Method call
  [s-call (object s-A5L?) (mname symbol?) (arg-expr (listof s-A5L?))]

  [s-err (message string?) (args (listof s-A5L?))]
  )

(define-type s-A5L-field
  [s-field (name symbol?) (value s-A5L?)])

(define-type s-A5L-method
  [s-method (name symbol?) (params (listof symbol?)) (body s-A5L?)])

(define-type A5L
  [num (n number?)]
  [bool (b boolean?)]
  [str (s string?)]
  [binop (op procedure?) (l A5L?) (r A5L?)]
  [equal-e (l A5L?) (r A5L?)]
  [if-b (scrutinee A5L?) (then-expr A5L?) (else-expr A5L?)]
  [withrec (name symbol?) (named-expr A5L?) (body A5L?)]
  [withrecs (defns (listof (list/c symbol? A5L?))) (body A5L?)]
  [fun (param symbol?) (body A5L?)]
  [app (fun-expr A5L?) (arg-expr A5L?)]
  [id (name symbol?)]

  [err (message string?) (args (listof A5L?))])

;; -----------------------------------------------------------------------------
;; PARSING
;; DO NOT CHANGE THIS PART

(define *reserved-symbols*
  '(+ - * = withrec withrecs fun if OBJECT OBJECT-DEL -> error method field))

;; valid-identifier? : any -> boolean
;; Determines whether the parameter is valid as an identifier name, i.e.,
;; a symbol that is not reserved.
(define (valid-identifier? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

;; parse : any -> s-A5L
(define (parse sexp)
  (match sexp
    [(? number?) (s-num sexp)]
    [(? string?) (s-str sexp)]
    ['true (s-bool true)]
    ['false (s-bool false)]
    [(list* 'error (? string? message) args) (s-err message (map parse args))]
    [(list '+ lhs rhs) (s-add (parse lhs) (parse rhs))]
    [(list '- lhs rhs) (s-sub (parse lhs) (parse rhs))]
    [(list '* lhs rhs) (s-mult (parse lhs) (parse rhs))]
    [(list '= lhs rhs) (s-= (parse lhs) (parse rhs))]
    [(list 'if scrutinee then-s else-s)
     (s-if (parse scrutinee) (parse then-s) (parse else-s))]
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
    [(list 'OBJECT fields methods)
     (s-object (map parse-field fields) (map parse-method methods))]
    [(list 'OBJECT-DEL parent fields methods)
     (s-object-del (parse parent) (map parse-field fields) (map parse-method methods))]
    [(list '-> obj mname arg-exp ...)
     (s-call (parse obj) mname (map parse arg-exp))]
    ; For function calls to work they must be the last list expression matched.
    ; In this pattern `arg-exps' can be empty, which happens when we call a nullary function
    [(cons fun-exp arg-exps)
     (s-app (parse fun-exp) (map parse arg-exps))]
    [_ (error 'parse "unable to parse ~a" sexp)]))

(define (parse-field sexp)
  (match sexp
    [(list 'field (? valid-identifier? name) init-val)
     (s-field name (parse init-val))]
    [_ (error 'parse-field "unable to parse ~a" sexp)]))

(define (parse-method sexp)
  (match sexp
    [(list 'method
           (? valid-identifier? name)
           (list (? valid-identifier? arg-names) ...)
           body)
     (s-method name arg-names (parse body))]
    [_ (error 'parse-method "unable to parse ~a" sexp)]))


;; -----------------------------------------------------------------------------
;; DESUGARING

(define (desugar exp)
  (type-case s-A5L exp
    [s-num (n) (num n)]
    [s-str (s) (str s)]
    [s-bool (b) (bool b)]
    [s-add (l r) (binop + (desugar l) (desugar r))]
    [s-sub (l r) (binop - (desugar l) (desugar r))]
    [s-mult (l r) (binop * (desugar l) (desugar r))]
    [s-= (l r) (equal-e (desugar l) (desugar r))]
    [s-if (scr thn els) (if-b (desugar scr) (desugar thn) (desugar els))]
    [s-withrec (name expr body) (withrec name (desugar expr) (desugar body))]
    [s-withrecs (defns body)
                (withrecs (map (λ (defn)
                                 (list (first defn)
                                       (desugar (second defn))))
                               defns)
                          (desugar body))]
    [s-fun (ids b) (foldr (lambda (x acc) (fun x acc)) (desugar b) ids)]
    ;; If the list of arguments is empty, then we apply the function to `false'
    ;; Otherwise, perform normal unfolding.
    [s-app (f a) (if (empty? a)
                     (app (desugar f) false)
                     (foldl (lambda (x acc) (app acc (desugar x))) (desugar f) a))]
    [s-id (x) (id x)]
    [s-object (fields methods)
              (desugar-object fields methods)]
    [s-object-del (parent fields methods)
                  (desugar-object-del parent fields methods)]

    
    ;; TODO-1: fix desugaring of s-call so that self is properly handled.

    [s-call (obj mname arg-expr) (desugar (s-app obj (list* (s-str (symbol->string mname)) obj arg-expr)))]
    ;[s-call (obj mname arg-expr) (desugar (s-app obj (if (s-= (s-id 'self) obj)
    ;                                                     (list* (s-str (symbol->string mname)) obj arg-expr)
    ;                                                     (list* (s-str (symbol->string mname)) arg-expr))))] ; s-A5L -> A5L [app (fun-expr A5L?) (arg-expr A5L?)]

    ; (desugar (s-call (s-id 'self) 'x? '()))
    ;    do think about where it needs to go though--why do you try to apply it as an argument to the outside of the given s-app?

    ; Original [s-call (obj mname arg-expr) (desugar (s-app obj (list* (s-str (symbol->string mname)) arg-expr)))]
    ;[(list '-> obj mname arg-exp ...)
    
    ; (s-call (parse obj) mname (map parse arg-exp))]
    ; For function calls to work they must be the last list expression matched.
    ; In this pattern `arg-exps' can be empty, which happens when we call a nullary function

    ; Method call
    ;[s-call (object s-A5L?) (mname symbol?) (arg-expr (listof s-A5L?))]

    
    [s-err (m args) (err m (map desugar args))]
    ))

(define (desugar-object fields methods)
  (local ([define (desugar-method m)
            (s-fun (cons 'self (s-method-params m))
                   (s-method-body m))]
          [define dynamic-dispatcher
            (s-fun (list 'method)
                   (foldr (λ (method dispatcher-rest-acc)
                            (s-if (s-= (s-id 'method)
                                       (s-str (symbol->string (s-method-name method))))
                                  (desugar-method method)
                                  dispatcher-rest-acc))
                          (s-err "Message not understood" '())
                          methods))])
    (desugar
     (foldr (λ (field desugared-object-acc)
              (s-withrec (s-field-name field)
                         (s-field-value field)
                         desugared-object-acc))
            dynamic-dispatcher
            fields))))

;; TODO-2: Change desugar-object-del so that it properly implements the
;;         delegation of methods, discussed in lecture 23, and on
;;         https://users.dcc.uchile.cl/~etanter/ooplai/Forwarding_and_Delegation.html

; s-object-del (parent s-A5L) (fields (listof s-field)) (methods (listof s-method)) -> [s-fun (ids b) (foldr (lambda (x acc) (fun x acc)) (desugar b) ids)]
(define (desugar-object-del parent fields methods)
  ;(desugar-object fields methods)
  (local ([define (desugar-method m)
            (s-fun (cons 'self (s-method-params m))
                   (s-method-body m))]
          [define dynamic-dispatcher
            (s-fun (list 'method)
                   (foldr (λ (method dispatcher-rest-acc)
                            (s-if (s-= (s-id 'method)
                                       (s-str (symbol->string (s-method-name method))))
                                  (desugar-method method)
                                  dispatcher-rest-acc))
                          (s-app parent (list (s-id 'method)))
                          methods))])
    (desugar
     (foldr (λ (field desugared-object-acc)
              (s-withrec (s-field-name field)
                         (s-field-value field)
                         desugared-object-acc))
            dynamic-dispatcher
            fields))))

;; NOTE:  YOU MUST NOT CHANGE THIS DEFINE-TYPE
(define-type Value
  [numV (n number?)]
  [strV (s string?)]
  [boolV (b boolean?)]
  [closureV (param symbol?) (body A5L?) (env Env?)])

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

;; interp : A5L -> Value
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

          ;; cyclically-bind-and-interp : symbol A5L Env -> Env
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
          ;;           : (listof (list/c symbol A5L)) Env -> Env
          ;; All definitions in decls may refer to any of the other recursive
          ;; definitions being simultaneously defined.
          (define (mutually-cyclically-bind-and-interp decls env)
            ; DONE : Implement this definition
            (local (; First, create all of the boxes
                    [define env-boxes (map (λ (x) (box #f)) decls)]
                    ; Create the environment that holds all of the boxes.
                    [define new-env (foldr (λ (decl box-for-decl env)
                                             (aRecEnv (first decl) box-for-decl env))
                                           env
                                           decls
                                           env-boxes)]
                    ; Then evaluate all of the expressions with the right env:
                    [define env-values (map (λ (x) (helper (second x) new-env))
                                            decls)])
                    
              (begin
                ; Update the boxes so that they have the right contents:
                (map set-box! env-boxes env-values)
                ; Then return the new environment.
                new-env)))

          ; helper : A5L Env -> Value
          (define (helper expr env)
            (type-case A5L expr
              [num (n) (numV n)]
              [str (s) (strV s)]
              [bool (b) (boolV b)]
              [binop (op l r) (interp-binop op
                                            (helper l env)
                                            (helper r env))]
              [equal-e (l r) (boolV (equal? (helper l env) (helper r env)))]
              [if-b (scr thn els)
                    (local ([define scr-val (helper scr env)])
                      (type-case Value scr-val
                        [boolV (b) (if b
                                       (helper thn env)
                                       (helper els env))]
                        [else (error 'interp
                                     "Expected a boolean value, but got ~s."
                                     scr-val)]))]
              
              [withrec (name named-expr bound-body)
                       (helper bound-body
                               (cyclically-bind-and-interp name named-expr env))]
              [withrecs (decls bound-body)
                        (helper bound-body
                                (mutually-cyclically-bind-and-interp decls env))]
              [fun (x body) (closureV x body env)]
              [app (fun-expr arg-expr)
                   (local ([define fun-val (helper fun-expr env)]
                           [define arg-val (helper arg-expr env)])
                     (type-case Value fun-val
                       [closureV (arg body clo-env)
                                 (helper body (anEnv arg arg-val clo-env))]
                       [else (error 'interp
                                    "Not a function in a function call: got ~s"
                                    fun-val)]))]
              [id (v) (lookup v env)]

              ;; New in A5L: If we encounter an `error', we just raise an error in Racket
              ;; We will format the error message with the arguments supplied
              [err (m args) (apply error 'interp (format "The program raised an error: ~s" m)
                                   (map (λ (a) (type-case Value (helper a env)
                                                 [numV (n) n]
                                                 [strV (s) s]
                                                 [boolV (b) b]
                                                 [closureV (n c e) "<Closure>"]))
                                        args))]
              ))]
    (helper expr (mtEnv))))

(define (run sexp)
  (interp (desugar (parse sexp))))

;; -----------------------------------------------------------------------------
;; OBJECT ORIENTED PROGRAMMING

;; Calling the methods

;; TODO-3:
;; Fix method calls in the following object definition.
(define A5L-todo-3
  '{fun {f} {OBJECT
             {{field func f}}
             {{method twice {x} {func {func x}}}
              ;; Fix the following method:
              {method four-times {x} {-> self twice {-> self twice x}}}}}})

;; -----
;; Lists

;; Ok, we lied... In fact, there are lists in A5L!
;; We just have to write them by ourselves.

;; We define a list in A5L to be an object with the following methods:
;; - is-empty?, which takes no arguments
;;   and returns `true' if the list is empty, and `false' if it is not.
;; - head, which takes no arguments and returns the head.
;; - tail, which takes no arguments and returns the tail, which is another list.

;; If a list is empty, than calling `head' or `tail' should cause an error.

;; Here are the `cons' constructor and `empty', the empty list, defined as objects.

;; TODO-4:
;; Add the `is-empty?' method to both `cons' and `empty'
(define A5L-cons
  '{fun {h t} {OBJECT
               {{field hd h}
                {field tl t}}
               {{method head {} hd}
                {method tail {} tl}
                {method is-empty? {} false}
                }}})

(define A5L-empty
  '{OBJECT {} ;; No fields, because nothing in an empty list!
           ;; We show an error message if we try to get the head or tail of `empty'.
           {{method head {} {error "Trying to get the head of an empty list."}}
            {method tail {} {error "Trying to get the tail of an empty list."}}
            {method is-empty? {} true}
            }})

;; TODO-5:
;; Write the `map' function for our list
;; You can use `cons' and `empty' in your code
(define A5L-map
  '{fun {f l}
        {if {-> l is-empty?}
            empty
            {cons {f {-> l head}} {map f {-> l tail}}}}})

;; TODO-6:
;; Write the `take' function for our list
;; You can use `cons' and `empty' in your code
(define A5L-take
  '{fun {n l}
        {if {-> l is-empty?}
            empty
            {if {= n 0}
                empty
                {cons {-> l head} {take {- n 1} {-> l tail}}}}}})

;; With the `map' function, we can map over a list by calling
;; {map some-function some-list}

;; Alternatively, we can define another type of list,
;; with `map' as one of its methods,
;; so that we can call `map' in a more object-oriented way:
;; {-> some-list-with-the-map-method map some-function}

;; We will call a list with the `map' method an `m-list'.

;; TODO-7:
;; Finish the definitions of `m-cons' and `m-empty',
;; the constructor of lists with the `map' method,
;; and the empty list with the `map' method.

;; The lists with the `map' method must adhere to our definition of a list,
;; that is, it must have the three list methods.
;; You should be able to use normal `map' and `take' on a list constructed using
;; `m-cons'es and `m-empty'.

;; Make your definition as simple as possible.
;; Use delegation so you do not have to implement all list methods in the
;; objects constructed by the `s-cons' and `s-empty' constructors.

;; You can use `m-cons' and `m-empty',
;; as well as the normal `cons' and `empty' in your code.

(define A5L-m-cons
  '{fun {h t} {OBJECT-DEL
               {cons h t}
               {}
               {{method map {f}
                        {cons {f h} {-> t map f}}}}}})


(define A5L-m-empty
  '{OBJECT-DEL
    empty; needs to be an instance of an object
    {}
    {{method map {f} empty}}})

;; ----
;; Infinity (revisited)

;; Did we say that there are no infinity in A5L?
;; Well, we lied again...  In fact, infinite lists are quite possible in A5L,
;; but they are constructed in a way quite different from lists in A5L.

;; TODO-8:
;; Define an infinite list of ones.  Do not use `iterate' which is in the next question.

;; HINT: `cons' and `empty' are not used here, but `self' is.
(define A5L-ones
        '{OBJECT {{field hd 1}}
                {{method head {} hd}
                 {method tail {} self}
                 {method is-empty? {} false}}})

;; TODO-9:
;; Define the `iterate' function, that takes a function and an initial value as arguments,
;; and returns an infinite list where the first element is the initial value,
;; the second element is the function applied to the initial value once,
;; the third element is the function applied twice, and so on.

;; i.e. given a function `f', and an initial value `x', `{iterate f x}' should give a list like:
;; [x, f(x), f(f(x)), f(f(f(x))), ...]

;; The idea of an `iterate' function comes from Haskell.
;; However, our implementation will be different from Haskell as we do not have lazy evaluation.

;; HINT: If the initial value is f(x), then the head of its tail should be f(f(x)).

;; You can use `iterate' in your code.
(define A5L-iterate
  '{fun {f i}
        {OBJECT {{field hd i}}
                {{method head {} hd}
                 {method tail {} {iterate f {f hd}}}
                 {method is-empty? {} false}}}})

;; TODO-10:
;; The `map' we have defined previously is eager.
;; If we use `map' on an infinite list, we will run into an infinite loop
;; because it will try to evaluate until the end of the list.

;; In order to map over an infinite list, we need to implement map in another way.
;; Remember that lists are just objects that have a certain set of methods,
;; and that map returns a list...
;; which means, lazy-map can just return an object with the three method required, ; what does "the three method required" mean?
;; and it would be a list!

;; Here is a partial implementation of `lazy-map',
;; which can run on infinite lists.
;; Fill in the TODOs in the function body so it works properly.

;; You can use `lazy-map' in your code.
(define A5L-lazy-map
  '{fun {f l}
        {if {-> l is-empty?}
            ;; If the list is empty, we have reached the base case
            ;; so we just return the normal empty list
            empty
            ;; If the list is not empty, create the object that will be the
            ;; first element in the list
            {OBJECT {} ;; Feel free to add fields if you think they are needed
                    {{method is-empty? {} false}
                     {method head {} {f {-> l head}}}
                     {method tail {} {lazy-map f {-> l tail}}}}}}})

;; -----------------------------------------------------------------------------
;; TESTS


(test (desugar (parse '{OBJECT {{field x 10} {field y 20}}
                               {{method x? {} x}
                                {method +x {a} {+ {-> self x?} a}}}}))
      (withrec
       'x
       (num 10)
       (withrec
        'y
        (num 20)
        (fun
         'method
         (if-b
          (equal-e (id 'method) (str "x?"))
          (fun 'self (id 'x))
          (if-b
           (equal-e (id 'method) (str "+x"))
           (fun
            'self
            (fun
             'a
             (binop +
                    (app (app (id 'self) (str "x?")) (id 'self))
                    (id 'a))))
           (err "Message not understood" '())))))))

;; Just basic method calls
(test (run `{withrec {foo {OBJECT {{field x 10}
                                   {field y 20}}
                                  {{method f {a} {+ a x}}
                                   {method g {a} {+ a y}}}}}
                     {+ {-> foo f 10} {-> foo g 20}}})
      (numV 60))

(test (run `{withrec {foo {OBJECT {}
                                  {{method f {a} {if {= 0 a} 0 {-> self f {- a 1}}}}}}}
                     {-> foo f 100}})
      (numV 0))

(test/must-timeout (run `{withrec {foo {OBJECT {}
                                               {{method f {a} {-> self f a}}}}}
                                  {-> foo f 100}}))

(test (run `{withrec {foo {OBJECT {}
                                  {{method f {a} {if {= 0 a}
                                                     0
                                                     {if {= 1 a}
                                                         0
                                                         {-> self g {- a 2}}}}}
                                   {method g {a} {-> self f {+ 1 a}}}}}}
                     {-> foo f 100}})
      (numV 0))

(test/exn (run `{withrec {foo {OBJECT {}
                                      {{method f {} 100}}}}
                         {-> foo g 100}})
          "Message not understood")

;; One way forwarding
(test (run `{withrec {foo {OBJECT {{field x 100}}
                                  {{method g {a} {+ x a}}}}}
                     {withrec {bar {OBJECT-DEL foo
                                               {{field x 200}}
                                               {{method f {a} {+ x {+ x a}}}}}}
                              {-> bar g 10}}})
      (numV 110))

(test (run `{withrec {foo {OBJECT {{field x 100}}
                                  {{method g {a} {+ x a}}}}}
                     {withrec {bar {OBJECT-DEL foo
                                               {{field x 200}}
                                               {{method f {a} {+ x {+ x a}}}}}}
                              {-> bar f 10}}})
      (numV 410))

(test (run `{withrec {foo {OBJECT {{field x 100}}
                                  {{method g {a} {+ x a}}}}}
                     {withrec {bar {OBJECT-DEL foo
                                               {{field x 200}}
                                               {{method g {a} {+ x {+ x a}}}}}}
                              {-> bar g 10}}})
      (numV 410))

(test/exn (run `{withrec {foo {OBJECT {{field x 100}}
                                      {{method g {a} {+ x a}}}}}
                         {withrec {bar {OBJECT-DEL foo
                                                   {{field x 200}}
                                                   {{method f {a} {+ x {+ x a}}}}}}
                                  {-> bar h 10}}})
          "Message not understood")

;; Delegation
(test (run `{withrec {foo {OBJECT {}
                                  {{method f {a} {* a 2}}
                                   {method g {a} {+ a {-> self f a}}}}}}
                     {withrec {bar {OBJECT-DEL foo
                                               {}
                                               {{method f {a} {* a 3}}}}}
                              {-> bar g 10}}})
      (numV 40))

(test (run `{withrec {foo {OBJECT {}
                                  {{method f {a} 100}
                                   {method g {a} {-> self f {- a 1}}}}}}
                     {withrec {bar {OBJECT-DEL foo
                                               {}
                                               {{method f {a} {if {= a 0}
                                                                  true
                                                                  {-> self g a}}}}}}
                              {-> bar g 20}}})
      (boolV true))


;; Tests for four-times
(test (run `{withrec {foo {,A5L-todo-3 {fun {x} {+ 1 x}}}}
                     {-> foo four-times 100}})
      (numV 104))

(test (run `{withrec {id {fun {x} x}}
                     {withrec {foo {,A5L-todo-3 id}}
                              {-> foo four-times 123}}})
      (numV 123))

(test (run `{withrec {foo {,A5L-todo-3 {fun {x} {* 2 x}}}}
                     {-> foo four-times 10}})
      (numV 160))

;; Tests for list
(define (with-list expr)
  `{withrecs {{cons ,A5L-cons}
              {empty, A5L-empty}}
             ,expr})

(define (with-m-list expr)
  `{withrecs {{cons ,A5L-cons}
              {empty, A5L-empty}
              {m-cons ,A5L-m-cons}
              {m-empty ,A5L-m-empty}}
             ,expr})

(test (run (with-list `{withrec {l {cons 1 {cons 2 {cons 3 empty}}}}
                                {+ {* 1000000 {-> l head}}
                                   {+ {* 10000 {-> {-> l tail} head}}
                                      {+ {* 100 {-> {-> {-> l tail} tail} head}}
                                         {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}))
      (numV 1020301))

(test (run (with-list `{withrec {l {cons 1 {cons 2 {cons 3 empty}}}}
                                {+ {* 1000000 {if {-> l is-empty?} 1 0}}
                                   {+ {* 10000 {if {-> {-> l tail} is-empty?} 1 0}}
                                      {+ {* 100 {if {-> {-> {-> l tail} tail} is-empty?} 1 0}}
                                         {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}))
      (numV 1))

(test (run (with-list `{withrec {l {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                {+ {* 100000 {-> l head}}
                                   {+ {* 1000 {-> {-> l tail} head}}
                                      {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                         {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}))
      (numV 1234560))

(test (run (with-list `{withrecs {{l0 {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                  {map ,A5L-map}}
                                 {withrec {l {map {fun {x} {- x 1}} l0}}
                                          {+ {* 100000 {-> l head}}
                                             {+ {* 1000 {-> {-> l tail} head}}
                                                {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                   {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1133550))

(test (run (with-list `{withrecs {{l0 {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                  {map ,A5L-map}}
                                 {withrec {l {map {fun {x} {+ x 1}} l0}}
                                          {+ {* 100000 {-> l head}}
                                             {+ {* 1000 {-> {-> l tail} head}}
                                                {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                   {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1335570))

(test (run (with-list `{withrecs {{f {fun {x} {+ x {f x}}}}
                                  {map ,A5L-map}}
                                 {-> {map f empty} is-empty?}}))
      (boolV true))

;; Map
(test (run (with-list `{withrecs {{l0 {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                  {map ,A5L-map}}
                                 {withrec {l {map {fun {x} {- x 1}} l0}}
                                          {+ {* 100000 {-> l head}}
                                             {+ {* 1000 {-> {-> l tail} head}}
                                                {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                   {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1133550))

(test (run (with-list `{withrecs {{l0 {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                  {map ,A5L-map}}
                                 {withrec {l {map {fun {x} {+ x 1}} l0}}
                                          {+ {* 100000 {-> l head}}
                                             {+ {* 1000 {-> {-> l tail} head}}
                                                {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                   {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1335570))

(test (run (with-list `{withrecs {{f {fun {x} {+ x {f x}}}}
                                  {map ,A5L-map}}
                                 {-> {map f empty} is-empty?}}))
      (boolV true))


;; Take
(test (run (with-list `{withrecs {{l0 {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                  {take ,A5L-take}}
                                 {withrec {l {take 100 l0}}
                                          {+ {* 100000 {-> l head}}
                                             {+ {* 1000 {-> {-> l tail} head}}
                                                {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                   {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1234560))

(test (run (with-list `{withrecs {{l0 {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                  {take ,A5L-take}}
                                 {withrec {l {take 4 l0}}
                                          {+ {* 100000 {-> l head}}
                                             {+ {* 1000 {-> {-> l tail} head}}
                                                {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                   {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1234560))

(test (run (with-list `{withrecs {{l0 {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                  {take ,A5L-take}}
                                 {withrec {l {take 3 l0}}
                                          {+ {* 100000 {-> l head}}
                                             {+ {* 1000 {-> {-> l tail} head}}
                                                {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                   {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1234561))

(test (run (with-list `{withrecs {{l0 {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                  {take ,A5L-take}}
                                 {withrec {l {take 2 l0}}
                                          {+ {* 100000 {-> l head}}
                                             {+ {* 1000 {-> {-> l tail} head}}
                                                {if {-> {-> {-> l tail} tail} is-empty?} 1 0}}}}}))
      (numV 1234001))

(test (run (with-list `{withrecs {{l0 {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                  {take ,A5L-take}}
                                 {-> {take 0 l0} is-empty?}}))
      (boolV true))

;; Tests for m-list
(test (run (with-m-list `{withrec {l {m-cons 1 {m-cons 2 {m-cons 3 m-empty}}}}
                                  {+ {* 1000000 {-> l head}}
                                     {+ {* 10000 {-> {-> l tail} head}}
                                        {+ {* 100 {-> {-> {-> l tail} tail} head}}
                                           {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}))
      (numV 1020301))

(test (run (with-m-list `{withrec {l {m-cons 1 {m-cons 2 {m-cons 3 m-empty}}}}
                                  {+ {* 1000000 {if {-> l is-empty?} 1 0}}
                                     {+ {* 10000 {if {-> {-> l tail} is-empty?} 1 0}}
                                        {+ {* 100 {if {-> {-> {-> l tail} tail} is-empty?} 1 0}}
                                           {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}))
      (numV 1))

(test (run (with-m-list `{withrec {l {m-cons 12 {m-cons 34 {m-cons 56 {m-cons 78 m-empty}}}}}
                                  {+ {* 100000 {-> l head}}
                                     {+ {* 1000 {-> {-> l tail} head}}
                                        {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                           {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}))
      (numV 1234560))


;; Map method of m-list
(test (run (with-m-list `{withrec {l0 {m-cons 12 {m-cons 34 {m-cons 56 {m-cons 78 m-empty}}}}}
                                  {withrec {l {-> l0 map {fun {x} {- x 1}}}}
                                           {+ {* 100000 {-> l head}}
                                              {+ {* 1000 {-> {-> l tail} head}}
                                                 {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                    {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1133550))

(test (run (with-m-list `{withrec {l0 {m-cons 12 {m-cons 34 {m-cons 56 {m-cons 78 m-empty}}}}}
                                  {withrec {l {-> l0 map {fun {x} {+ x 1}}}}
                                           {+ {* 100000 {-> l head}}
                                              {+ {* 1000 {-> {-> l tail} head}}
                                                 {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                    {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1335570))

(test (run (with-m-list `{withrec {f {fun {x} {+ x {f x}}}}
                                  {-> {-> m-empty map f} is-empty?}}))
      (boolV true))

;; m-list compatibility

(test (run (with-m-list `{withrecs {{l0 {m-cons 12 {m-cons 34 {m-cons 56 {m-cons 78 m-empty}}}}}
                                    {map ,A5L-map}}
                                   {withrec {l {map {fun {x} {- x 1}} l0}}
                                            {+ {* 100000 {-> l head}}
                                               {+ {* 1000 {-> {-> l tail} head}}
                                                  {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                     {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1133550))

(test (run (with-m-list `{withrecs {{l0 {m-cons 12 {m-cons 34 {m-cons 56 {m-cons 78 m-empty}}}}}
                                    {map ,A5L-map}}
                                   {withrec {l {map {fun {x} {+ x 1}} l0}}
                                            {+ {* 100000 {-> l head}}
                                               {+ {* 1000 {-> {-> l tail} head}}
                                                  {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                     {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1335570))

(test (run (with-m-list `{withrecs {{f {fun {x} {+ x {f x}}}}
                                    {map ,A5L-map}}
                                   {-> {map f m-empty} is-empty?}}))
      (boolV true))

(test (run (with-m-list `{withrecs {{l0 {m-cons 12 {m-cons 34 {m-cons 56 {m-cons 78 m-empty}}}}}
                                    {take ,A5L-take}}
                                   {withrec {l {take 3 l0}}
                                            {+ {* 100000 {-> l head}}
                                               {+ {* 1000 {-> {-> l tail} head}}
                                                  {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                     {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1234561))

(test (run (with-m-list `{withrecs {{l0 {m-cons 12 {m-cons 34 {m-cons 56 {m-cons 78 m-empty}}}}}
                                    {take ,A5L-take}}
                                   {withrec {l {take 2 l0}}
                                            {+ {* 100000 {-> l head}}
                                               {+ {* 1000 {-> {-> l tail} head}}
                                                  {if {-> {-> {-> l tail} tail} is-empty?} 1 0}}}}}))
      (numV 1234001))

(test (run (with-m-list `{withrecs {{l0 {m-cons 12 {m-cons 34 {m-cons 56 {m-cons 78 m-empty}}}}}
                                    {take ,A5L-take}}
                                   {-> {take 0 l0} is-empty?}}))
      (boolV true))

;; Infinite lists

(test (run (with-list `{withrec {ones ,A5L-ones}
                                {+ {if {-> ones is-empty?} 1 0}
                                   {if {-> {-> {-> ones tail} tail} is-empty?} 1 0}}}))
      (numV 0))

(test (run (with-list `{withrec {ones ,A5L-ones}
                                {-> ones head}}))
      (numV 1))

(test (run (with-list `{withrec {ones ,A5L-ones}
                                {-> {-> ones tail} head}}))
      (numV 1))

(test (run (with-list `{withrec {ones ,A5L-ones}
                                {-> {-> {-> {-> {-> {-> {-> ones tail} tail} tail} tail} tail} tail} head}}))
      (numV 1))

(test (run (with-list `{withrec {iterate ,A5L-iterate}
                                {withrec {naturals {iterate {fun {x} {+ 1 x}} 0}}
                                         {+ {if {-> naturals is-empty?} 1 0}
                                            {if {-> {-> {-> naturals tail} tail} is-empty?} 1 0}}}}))
      (numV 0))

(test (run (with-list `{withrec {iterate ,A5L-iterate}
                                {withrec {naturals {iterate {fun {x} {+ 1 x}} 0}}
                                         {-> naturals head}}}))
      (numV 0))

(test (run (with-list `{withrec {iterate ,A5L-iterate}
                                {withrec {naturals {iterate {fun {x} {+ 1 x}} 0}}
                                         {-> {-> {-> {-> {-> naturals tail} tail} tail} tail} head}}}))
      (numV 4))

(test (run (with-list `{withrec {iterate ,A5L-iterate}
                                {withrec {naturals {iterate {fun {x} {+ 1 x}} 0}}
                                         {+ {* 1000000 {-> naturals head}}
                                            {+ {* 10000 {-> {-> naturals tail} head}}
                                               {+ {* 100 {-> {-> {-> naturals tail} tail} head}}
                                                  {-> {-> {-> {-> naturals tail} tail} tail} head}}}}}}))
      (numV 10203))

(test (run (with-list `{withrec {iterate ,A5L-iterate}
                                {withrec {doubles {iterate {fun {x} {* 2 x}} 1}}
                                         {+ {* 1000000 {-> doubles head}}
                                            {+ {* 10000 {-> {-> doubles tail} head}}
                                               {+ {* 100 {-> {-> {-> doubles tail} tail} head}}
                                                  {-> {-> {-> {-> doubles tail} tail} tail} head}}}}}}))
      (numV 1020408))

(test (run (with-list `{withrec {iterate ,A5L-iterate}
                                {withrec {ns {iterate {fun {x} {if {= x 0} x {- x 1}}} 2}}
                                         {+ {* 1000000 {-> ns head}}
                                            {+ {* 10000 {-> {-> ns tail} head}}
                                               {+ {* 100 {-> {-> {-> ns tail} tail} head}}
                                                  {-> {-> {-> {-> ns tail} tail} tail} head}}}}}}))
      (numV 2010000))

(test (run (with-list `{withrecs {{iterate ,A5L-iterate}
                                  {lazy-map ,A5L-lazy-map}}
                                 {withrec {twos {lazy-map {fun {x} {* 2 x}}
                                                          {iterate {fun {x} 1} 1}}}
                                          {-> twos head}}}))
      (numV 2))

(test (run (with-list `{withrecs {{iterate ,A5L-iterate}
                                  {lazy-map ,A5L-lazy-map}}
                                 {withrec {twos {lazy-map {fun {x} {* 2 x}}
                                                          {iterate {fun {x} 1} 1}}}
                                          {-> {-> {-> {-> twos tail} tail} tail} head}}}))
      (numV 2))

(test (run (with-list `{withrecs {{iterate ,A5L-iterate}
                                  {lazy-map ,A5L-lazy-map}}
                                 {withrec {odds {lazy-map {fun {x} {+ x 1}}
                                                          {iterate {fun {x} {+ 2 x}} 0}}}
                                          {-> {-> {-> {-> odds tail} tail} tail} head}}}))
      (numV 7))

;; Also check that lazy-map works with finite list

(test (run (with-list `{withrecs {{l0 {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                  {lazy-map ,A5L-lazy-map}}
                                 {withrec {l {lazy-map {fun {x} {- x 1}} l0}}
                                          {+ {* 100000 {-> l head}}
                                             {+ {* 1000 {-> {-> l tail} head}}
                                                {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                   {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1133550))

(test (run (with-list `{withrecs {{l0 {cons 12 {cons 34 {cons 56 {cons 78 empty}}}}}
                                  {lazy-map ,A5L-lazy-map}}
                                 {withrec {l {lazy-map {fun {x} {+ x 1}} l0}}
                                          {+ {* 100000 {-> l head}}
                                             {+ {* 1000 {-> {-> l tail} head}}
                                                {+ {* 10 {-> {-> {-> l tail} tail} head}}
                                                   {if {-> {-> {-> {-> l tail} tail} tail} is-empty?} 1 0}}}}}}))
      (numV 1335570))

(test (run (with-list `{withrecs {{f {fun {x} {+ x {f x}}}}
                                  {lazy-map ,A5L-lazy-map}}
                                 {-> {lazy-map f empty} is-empty?}}))
      (boolV true))