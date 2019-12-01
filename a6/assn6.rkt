#lang plai
;; NOTE: While we provide you with some tests (all the tests that will be marked, for example)
;;       we as usual STRONGLY RECOMMEND YOU to write your own tests as well.  That way you might
;;       be better able to separate the impact of each of the TODOs, as many tests will require multiple
;;       TODOs to be fixed for the tests to pass.
(print-only-errors)

(require racket/sandbox
         ;Also, re-import some macros from plai:
         (only-in plai
                  ;Rename plai's testX macros as plai-testX
                  [test plai-test]
                  [test/pred plai-test/pred]
                  [test/exn plai-test/exn]))

; These macros are the same as in assignment 4 (though with shorter timeouts)
(define-syntax-rule (test actual expected)
  (plai-test (with-limits 2 128 actual) expected))

(define-syntax-rule (test/exn actual msg)
  (plai-test/exn (with-limits 2 128 actual) msg))

(define-syntax-rule (test/pred actual pred)
  (plai-test/pred (with-limits 2 128 actual) pred))

;; Syntax specification:
;;
;; <PLE> ::= <num>
;;          | <id>
;;          | {+ <PLE> <PLE>}
;;          | {- <PLE> <PLE>}
;;          | {* <PLE> <PLE>}
;;          | {with {<id> <PLE>} <PLE>}
;;          | {fun {<id>} <PLE>}
;;          | {<PLE> <PLE>}
;;          | {if0 <PLE> <PLE> <PLE>}
;;          | {empty}
;;          | {cons <PLE> <PLE>}
;;          | {first <PLE>}
;;          | {rest <PLE>}
;;          | {box <PLE>}
;;          | {setbox! <PLE> <PLE>}
;;          | {unbox <PLE>}
;;          | {seqn <PLE> <PLE>}
;;          | {promise <PLE>}
;;          | {yield}
;;          | {after <PLE> <PLE>}
;;          | {all <PLE> <PLE>}
;;          | {race <PLE> <PLE>}
;;          | {resolved <PLE>}
;;          | {print-out <string>}



;; We're going to make desugaring an explicit step.  So, here's the
;; "surface" language that our end-user programmers program in:
(define-type PLE
  [s-num (n number?)]
  [s-id (name symbol?)]
  [s-add (lhs PLE?) (rhs PLE?)]
  [s-sub (lhs PLE?) (rhs PLE?)]
  [s-mult (lhs PLE?) (rhs PLE?)]
  [s-with (name symbol?) (named-exp PLE?) (body PLE?)]
  [s-fun (arg-name symbol?) (body PLE?)]
  [s-app (fun-exp PLE?) (arg-exp PLE?)]
  [s-if0 (test-exp PLE?) (then-exp PLE?) (else-exp PLE?)]
  [s-box (val-exp PLE?)]
  [s-setbox! (box-exp PLE?) (val-exp PLE?)]
  [s-un-box (box-exp PLE?)]
  [s-seqn (exp1 PLE?) (exp2 PLE?)]
  [s-cons-cell (hd PLE?) (tl PLE?)]
  [s-empty-cell]
  [s-promise (expr PLE?)]
  [s-yield]
  [s-after (promise PLE?) (then PLE?)]
  [s-all (first PLE?) (snd PLE?)]
  [s-race (first PLE?) (snd PLE?)]
  [s-print-out (string string?)]
  [s-resolved (promise PLE?)]
  )

;; Here's the language our interpreter will understand.  Note that it
;; lacks "with" AND "set!".  "D-PLE" is for "desugared CFAE":
(define-type D-PLE
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs D-PLE?) (rhs D-PLE?)]
  [mult (lhs D-PLE?) (rhs D-PLE?)]
  [fun (arg-name symbol?) (body D-PLE?)]
  [app (fun-exp D-PLE?) (arg-exp D-PLE?)]
  [if0 (test-exp D-PLE?) (then-exp D-PLE?) (else-exp D-PLE?)]
  [make-box (val-exp D-PLE?)]
  [setbox! (box-exp D-PLE?) (val-exp D-PLE?)]
  [un-box (box-exp D-PLE?)]
  [cons-cell (hd D-PLE?)(tl D-PLE?)]
  [empty-cell]
  [promise (e D-PLE?)]
  [yield]
  [after (p D-PLE?) (then D-PLE?)]
  [all (fst D-PLE?) (snd D-PLE?)]
  [race (fst D-PLE?) (snd D-PLE?)]
  [print-out (str string?)]
  [resolved (promise D-PLE?)]
  )


(define *reserved-symbols* '(+ - * with fun if0 box setbox! unbox seqn
                               promise all race after print-out yield
                               empty-cell cons-cell resolved))

;; valid-identifier? : any -> boolean
;; Determines whether the parameter is valid as an identifier name, i.e.,
;; a symbol that is not reserved.
(define (valid-identifier? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

;; Some reserved symbols.
(test (valid-identifier? '+) false)
(test (valid-identifier? '-) false)
(test (valid-identifier? 'with) false)
(test (valid-identifier? 'setbox!) false)

;; Not a symbol
(test (valid-identifier? '{+ 1 2}) false)
(test (valid-identifier? 3) false)
(test (valid-identifier? "id") false)

;; OK
(test (valid-identifier? 'id) true)
(test (valid-identifier? 'app) true)
(test (valid-identifier? 'x) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PARSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; parse : any -> PLE
;; Consumes an s-expression (in our concrete "surface" syntax) and
;; generates the corresponding PLE AST.
(define (parse sexp)
  (match sexp
    [(? valid-identifier?) (s-id sexp)]
    [(? number?) (s-num sexp)]
    [(list '+ lexp rexp) (s-add (parse lexp) (parse rexp))]
    [(list '- lexp rexp) (s-sub (parse lexp) (parse rexp))]
    [(list '* lexp rexp) (s-mult (parse lexp) (parse rexp))]
    [(list 'with (list (and (? valid-identifier?) id) binding-expr) body-expr)
     (s-with id (parse binding-expr) (parse body-expr))]
    [(list 'fun (list (and (? valid-identifier?) id)) body-expr)
     (s-fun id (parse body-expr))]
    [(list 'if0 c-expr t-expr e-expr)
     (s-if0 (parse c-expr) (parse t-expr) (parse e-expr))]
    ;; Don't parse (<reserved> <expr>) as a function application.
    [(list (and f-expr (? (lambda (s) (not (member s *reserved-symbols*))))) a-expr)
     (s-app (parse f-expr) (parse a-expr))]
    [(list 'box v-expr) (s-box (parse v-expr))]
    [(list 'unbox b-expr) (s-un-box (parse b-expr))]
    [(list 'setbox! b-expr v-expr) (s-setbox! (parse b-expr) (parse v-expr))]
    [(list 'seqn expr1 expr2) (s-seqn (parse expr1) (parse expr2))]
    [(list 'cons-cell hd tl) (s-cons-cell (parse hd) (parse tl))]
    ['empty-cell (s-empty-cell)]
    [(list 'yield) (s-yield)]
    [(list 'promise expr) (s-promise (parse expr))]
    [(list 'all e1 e2) (s-all (parse e1) (parse e2))]
    [(list 'after e1 e2) (s-after (parse e1) (parse e2))]
    [(list 'race e1 e2) (s-race (parse e1) (parse e2))]
    [(list 'print-out s) (s-print-out s)]
    [(list 'resolved s) (s-resolved (parse s))]
    [else (error 'parse "unable to parse the s-expression ~s" sexp)]))


(test (parse 'x) (s-id 'x))
(test (parse 'ys) (s-id 'ys))
(test/exn (parse '+) "")

;; Boxes
(test (parse '{box 0}) (s-box (s-num 0)))
(test (parse '{unbox 0}) (s-un-box (s-num 0)))  ;; runtime error, but not parsing error
(test (parse '{setbox! 0 1}) (s-setbox! (s-num 0) (s-num 1)))  ;; ditto non-parsing error

;; Seqn
(test (parse '{seqn 0 1}) (s-seqn (s-num 0) (s-num 1)))

;; Numbers
(test (parse '3) (s-num 3))
(test (parse '0) (s-num 0))

;; Plain arithmetic.
(test (parse '{+ 1 2}) (s-add (s-num 1) (s-num 2)))
(test (parse '{- 1 2}) (s-sub (s-num 1) (s-num 2)))
(test (parse '{+ 3 4}) (s-add (s-num 3) (s-num 4)))
(test (parse '{- 3 4}) (s-sub (s-num 3) (s-num 4)))
(test (parse '{* 3 4}) (s-mult (s-num 3) (s-num 4)))

(test (parse '{+ {- 1 {+ 2 3}} 4})
      (s-add (s-sub (s-num 1) (s-add (s-num 2) (s-num 3)))
             (s-num 4)))

;; With binding
(test (parse '{with {x 1} x}) (s-with 'x (s-num 1) (s-id 'x)))

(test (parse '{with {x {with {y 2} {+ x y}}} {with {z 3} {+ x z}}})
      (s-with 'x (s-with 'y (s-num 2) (s-add (s-id 'x) (s-id 'y)))
              (s-with 'z (s-num 3) (s-add (s-id 'x) (s-id 'z)))))

;; Funs/Apps
(test (parse '{fun {x} x}) (s-fun 'x (s-id 'x)))
(test/exn (parse '{fun {fun} x}) "")
(test (parse '{1 2}) (s-app (s-num 1) (s-num 2)))

;; Error checking

; non-lists, reserved symbols (e.g., + and -), strings
(test/exn (parse '"hello") "")
(test/exn (parse '+) "")
(test/exn (parse '-) "")
(test/exn (parse 'with) "")


; lists that start with things besides +, -, or with, esp. numbers
; that aren't valid apps:
(test/exn (parse '{hello 1 2}) "")
(test/exn (parse '{"abc"}) "")
(test/exn (parse '{1 2 3}) "")

; + with fewer or more than 2 arguments
(test/exn (parse '{+}) "")
(test/exn (parse '{+ 1}) "")
(test/exn (parse '{+ 1 2 3}) "")

; - with fewer or more than 2 arguments
(test/exn (parse '{-}) "")
(test/exn (parse '{- 1}) "")
(test/exn (parse '{- 1 2 3}) "")

; ill-structured with
(test/exn (parse '{with}) "")
(test/exn (parse '{with x}) "")
(test/exn (parse '{with x 2 3}) "")
(test/exn (parse '{with {x 1}}) "")
(test/exn (parse '{with {x 1} 2 3}) "")
(test/exn (parse '{with {x 1 2} 3}) "")
(test/exn (parse '{with {+ 1} 2}) "")

; + (and -/with) with non-AEs as arguments
(test/exn (parse '{+ "a" 3}) "")
(test/exn (parse '{- 1 "b"}) "")
(test/exn (parse '{+ {- 12 #\c} 8}) "")
(test/exn (parse '{with {x "foo"} x}) "")
(test/exn (parse '{with {x 1} "foo"}) "")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DESUGARING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (desugar ast)
  (type-case PLE ast
    [s-num (n) (num n)]
    [s-id (name) (id name)]
    [s-add (lhs rhs) (add (desugar lhs) (desugar rhs))]
    [s-sub (lhs rhs) (add (desugar lhs) (mult (num -1) (desugar rhs)))]
    [s-mult (lhs rhs) (mult (desugar lhs) (desugar rhs))]
    ;; A substantive desugaring:
    [s-with (name named-exp body) (desugar (s-app (s-fun name body) 
                                                  named-exp))]
    [s-fun (arg-name body) (fun arg-name (desugar body))]
    [s-app (fun-exp arg-exp) (app (desugar fun-exp) (desugar arg-exp))]
    [s-if0 (c t e) (if0 (desugar c) (desugar t) (desugar e))]
    [s-box (v) (make-box (desugar v))]
    [s-un-box (b) (un-box (desugar b))]
    [s-setbox! (b v) (setbox! (desugar b) (desugar v))]
    [s-seqn (e1 e2) (desugar (s-with 'seqn e1 e2))]

    [s-cons-cell (hd tl) (cons-cell (desugar hd) (desugar tl))]
    [s-empty-cell () (empty-cell)]
    [s-promise (e) (promise (desugar e))]
    [s-all (e1 e2) (all (desugar e1) (desugar e2))]
    [s-after (p t) (after (desugar p) (desugar t))]
    [s-yield () (yield)]
    [s-race (e1 e2) (race (desugar e1) (desugar e2))]
    [s-print-out (s) (print-out s)]
    [s-resolved (e) (resolved (desugar e))]
    ))

(test (desugar (parse 'x)) (id 'x))
(test (desugar (parse 'ys)) (id 'ys))

;; Boxes
(test (desugar (parse '{box 0})) (make-box (num 0)))
(test (desugar (parse '{unbox 0})) (un-box (num 0)))  ;; runtime error, but not parsing error
(test (desugar (parse '{setbox! 0 1})) (setbox! (num 0) (num 1)))  ;; ditto non-parsing error

;; Seqn
(test (desugar (parse '{seqn 0 1})) (desugar (s-with 'seqn (s-num 0) (s-num 1))))

;; Numbers
(test (desugar (parse '3)) (num 3))
(test (desugar (parse '0)) (num 0))

;; Plain arithmetic.
(test (desugar (parse '{+ 1 2})) (add (num 1) (num 2)))
(test (desugar (parse '{- 1 2})) (add (num 1) (mult (num -1) (num 2))))
(test (desugar (parse '{* 3 4})) (mult (num 3) (num 4)))

;; With binding
(test (desugar (parse '{with {x 1} x})) (app (fun 'x  (id 'x)) (num 1)))

;; Funs/Apps
(test (desugar (parse '{fun {x} x})) (fun 'x (id 'x)))
(test (desugar (parse '{1 2})) (app (num 1) (num 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INTERPRETATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Values in our internal "desugared" abstract syntax:
(define-type D-PLE-value 
  [numV (n number?)]
  [boxV (inside number?)]
  ; Note we are using racket's procedures!
  [closureV (proc procedure?)]
  ; Values for lists:
  [mtV]
  [consV (hd D-PLE-value?) (tl D-PLE-value?)]
  ; voidV value
  [voidV]
  ; PromiseV value:
  [promiseV (promise-id number?)])

;; The environment that stores our "deferred substitutions":
(define-type Env 
  [mtEnv]
  [anEnv (id symbol?) 
         (val D-PLE-value?) 
         (more-subs Env?)])

;; lookup : symbol Env -> D-PLE-Value
;; Finds the value of name in env or errors if name is undefined
(define (lookup name env)
  (local ([define (lookup-helper name env)
            (type-case Env env
              [mtEnv () (error 'lookup "free identifier ~a" name)]
              [anEnv (bound-name bound-value rest-env)
                     (if (symbol=? bound-name name)
                         bound-value
                         (lookup-helper name rest-env))])])
    (lookup-helper name env)))

;; +/-/* helpers to make interpretation easier (no error-checking yet)
(define (num+/k x y sto str thr pro k)  (k (numV (+ (numV-n x) (numV-n y))) sto str thr pro))
(define (num*/k x y sto str thr pro k)  (k (numV (* (numV-n x) (numV-n y))) sto str thr pro))
(define (num-/k x y sto str thr pro k)  (k (numV (- (numV-n x) (numV-n y))) sto str thr pro))

;; The store associates memory locations with their values.
(define-type Store 
  [mtStore]
  [aStore (locn integer?) 
          (val D-PLE-value?) 
          (more-store Store?)])

;; lookup-store : integer Store -> D-PLE-Value
;; Finds the value at location locn in store or errors if it is undefined
(define (lookup-store locn store)
  (local ([define (lookup-helper locn store)
            (type-case Store store
              [mtStore () (error 'lookup-store "unallocated memory location ~a" locn)]
              [aStore (alloc-locn alloc-value rest-store)
                      (if (= alloc-locn locn)
                          alloc-value
                          (lookup-helper locn rest-store))])])
    (lookup-helper locn store)))

;; get-next-memory-location : Store -> integer
;; produce a value one larger than the largest store address used so far
;; or if the store is empty, produce 1
(define (get-next-memory-location store)
  (type-case Store store
    [mtStore () 1]
    [aStore (alloc-locn alloc-value rest-store)
            (max (+ alloc-locn 1)
                 (get-next-memory-location rest-store))]))

;;;;; Scheduling functions for cooperative multitasking

;; in this context, a ContFun is a function (D-PLE-value Store String threads proms -> A) for some A.
;; (a Continuation Function)

;; schedule-computation-for-later : ContFun (list ContFun) -> (list ContFun)
;; Appends a continuation function to be evaluated later.
;; It ends up providing round-robin scheduling of tasks.

(define (schedule-computation-for-later k lok)
  (append lok (list k)))


;; context-switch : Value Store String (list ContFun) ContFun -> A
;; This function represents the scheduler behaviour.
;; It evaluates the first available pending continuation on the list.
;; If there is no pending computation, it produces an error.

(define (context-switch value store stdout pending-computations promises)
  (cond
    [(empty? pending-computations) (error "NOTHING ELSE TO DO!  This should never happen.")]
    [else (let ([next-k (first pending-computations)])
            (next-k value store stdout (rest pending-computations) promises))]))

;; The Resolved Promises dictionary associates promise IDs with their values once they are resolved.
(define-type ResolvedPromises 
  [mtRPs]
  [resolvedPromise (promise-id integer?) 
                   (val D-PLE-value?) 
                   (more-store ResolvedPromises?)])

;; lookup-promise-value : Number ResolvedPromises -> (or D-PLE-Value? false)
;; Returns the value if the promise has been resolved.
;; If not found, it returns false.
(define (lookup-promise-value id promises)
  (type-case ResolvedPromises promises
    [mtRPs () #f]
    [resolvedPromise (r-id r-val rest)
                     (if (= r-id id)
                         r-val
                         (lookup-promise-value id rest))]))



;; do-once-promise-is-resolved : Number ContFun -> ContFun
;; This function builds a pending computation (to be later scheduled-for-later)
;; such that checks if a promise has been resolved.
;; once it is resolved, it calls continuation k with its resolved value.
;; if it has not been resolved yet,  the computation context switches after rescheduling itself for later.

(define (do-once-promise-is-resolved promise-id k)
  (local ([define (computation-helper promise-id)
            (λ (_ store stdout threads resolved-promises)
              (let [(promise-value (lookup-promise-value promise-id resolved-promises))]
                (if promise-value
                    (type-case D-PLE-value promise-value
                      [promiseV (new-promise-id)
                                ;; Is the resolved promise value /ANOTHER/ promise? If so, we should wait for that one to be resolved instead!
                                (context-switch (voidV) store stdout
                                                (schedule-computation-for-later (computation-helper new-promise-id) threads)
                                                resolved-promises)]
                      ; If it is not a promise, we can continue with the original work we wanted to do after the promise was resolved!
                      [else (k promise-value store stdout threads resolved-promises)])
                    ; If we have not resolved the promise yet, we reschedule ourselves for later and yield the computation.
                    (context-switch (voidV) store stdout
                                    (schedule-computation-for-later (computation-helper promise-id) threads)
                                    resolved-promises))))])
    ;; We trampoline our helper function with the original promise-id.
    (computation-helper promise-id)))                                

;;  resolve-promise-and-continue : Number -> ContFun
;;  this continuation updates the dictionary of resolved promises and then just
;;  context switches ("continues") to do the rest of the computations pending.
;;  It is the action we perform when we have finished the computation thread of a promise:
;;  Just resolve and see what is left to be executed!

(define (resolve-promise-and-continue promise-id)
  ;; We are returning a lambda (a ContFun!)
  (λ (resolved-value store-rv stdout-rv threads-rv proms-rv)
    ; Once the promise value is computed, we actually just need to update the promise values:
    (let ([resolved-proms (resolvedPromise promise-id resolved-value proms-rv)])
      ; and we can context switch to do the next computation pending:
      (context-switch (voidV) store-rv stdout-rv threads-rv resolved-proms))))


;; interp/k : D-PLE Env Store (D-PLE-value store stdout threads proms -> A) -> A
;;  Our interpreter definition.
;;  next-promise-id is defined in the local so that every time we call interp we reset our promise id generator.
;;  otherwise, interp/k just trampolines to helper/k.

(define (interp/k exp env store stdout k)
  (local [ ;; next-promise-id : () -> Number
          ;; Provides an unused number that will act as a promise identifier.
          ;; Useful for creating new promiseV's!
          (define next-promise-id 
            (let ([last-id (box 0)])
              (lambda ()
                (set-box! last-id (add1 (unbox last-id)))
                (unbox last-id))))

          ;; helper : D-PLE Env Store (D-PLE-value Store -> A) -> A
          ;; evaluates the exp in env using the memory store,
          ;; in continuation-passing style.
          (define (helper/k exp ; The original program
                            env ; The environment
                            store ; The store
                            stdout ; The standard output string
                            threads ; The computations that have not been finished yet
                            proms ; The already resolved promises
                            k)
            (type-case D-PLE exp
              [num (n)
                   (k (numV n) store stdout threads proms)]
              [add (l r)
                   (helper/k l env store stdout threads proms
                             (lambda (lv store-l stdout-l threads-l proms-l)
                               (helper/k r env store-l stdout-l threads-l proms-l
                                         (lambda (rv store-r stdout-r threads-r proms-r)
                                           (num+/k lv rv store-r stdout-r threads-r proms-r k)))))]

              [id  (name) (k (lookup name env) 
                             store stdout threads proms)]
              [if0 (tst thn els) 
                   (helper/k tst env store stdout threads proms
                             (lambda (tst-val sto-tst stdout-tst threads-tst proms-tst) 
                               (type-case D-PLE-value tst-val 
                                 [numV (n)
                                       (if (zero? n) 
                                           (helper/k thn env sto-tst stdout-tst threads-tst proms-tst k)
                                           (helper/k els env sto-tst stdout-tst threads-tst proms-tst k))]
                                 [else (error "The test expression of an if0 must evaluate to a number.")])))]
              [make-box (be)
                        ; (1) evaluate be to a value bv
                        ; (2) get the next available memory location m
                        ; (3) put bv into location m in the store
                        ; (4) evaluate to (boxV m) with the NEW store
                        (helper/k be env store stdout threads proms
                                  (lambda (bv store-b stdout-b threads-b proms-b)
                                    (local [(define new-locn (get-next-memory-location store-b))]
                                      (k (boxV new-locn)
                                         (aStore new-locn bv store-b)
                                         stdout-b
                                         threads-b
                                         proms-b))))]

              ; TODO 1 : Fix the implementation of this case.
              [fun (arg-name body)
                   (k
                    (closureV (lambda (argv store-f stdout-f threads-f proms-f caller/k)
                                  (helper/k body (anEnv arg-name argv env) store-f stdout-f threads-f proms-f caller/k)))
                      store
                      stdout
                      threads
                      proms
                      )
                    ]

              [app (f a)
                   (helper/k f env store stdout threads proms
                             (lambda (the-fun store-f stdout-f threads-f proms-f) 
                               (helper/k a env store-f stdout-f threads-f proms-f
                                         (lambda (the-arg store-a stdout-a threads-a proms-a) 
                                           (type-case D-PLE-value the-fun
                                             [closureV (proc)
                                                       (proc the-arg store-a stdout-a threads-a proms-a k)]
                                             [else (error "You can only apply closures!")])))))]
	     
              [mult (l r)
                    (helper/k l env store stdout threads proms
                              (lambda (lv store-l stdout-l threads-l proms-l) 
                                (helper/k r env store-l stdout-l threads-l proms-l
                                          (lambda (rv store-r stdout-r threads-r proms-r)  
                                            (num*/k lv rv store-r stdout-r threads-r proms-r k)))))]
	     
              [setbox! (be ve)
                       (helper/k be env store stdout threads proms
                                 (lambda (bv store-b stdout-b threads-b proms-b)
                                   (helper/k ve env store-b stdout-b threads-b proms-b
                                             (lambda (vv store-v stdout-v threads-v proms-v)
                                               (type-case D-PLE-value bv
                                                 [boxV (locn) (k vv (aStore locn vv store-v) stdout-v threads-v proms-v)]
                                                 [else (error "the box expression of a setbox! must evaluate to a box value")])))))]
             
              [un-box (be)
                      (helper/k be env store stdout threads proms
                                (lambda (bv store-b stdout-b threads-b proms-b)
                                  (type-case D-PLE-value bv
                                    [boxV (locn) (k (lookup-store locn store-b) store-b stdout-b threads-b proms-b)]
                                    [else (error "the box expression of an unbox must evaluate to a box value")])))]
              
              [empty-cell () (k (mtV) store stdout threads proms)]

              [cons-cell (hd tl)
                         (helper/k hd env store stdout threads proms
                                   (lambda (hd-v store-hd stdout-hd threads-hd proms-hd)
                                     (helper/k tl env store-hd stdout-hd threads-hd proms-hd
                                               (lambda (tl-v store-tl stdout-tl threads-tl proms-tl)
                                                 (k (consV hd-v tl-v) store-tl stdout-tl threads-tl proms-tl)))))]
              
              ;; when we hit a promise, we want to schedule it for later
              ;; then continue where we were
              ;; once the computation is actually finished
              ;; we want to relinquish control of the processor
              ;; and return to the scheduler
              [promise (expr)
                       (let* ([promise-id (next-promise-id)]
                              [promise-contents
                               ;; The promise itself will compute expr later:
                               (λ (_ current-store current-stdout current-threads current-proms)
                                 (helper/k expr env current-store current-stdout current-threads current-proms
                                           (resolve-promise-and-continue promise-id)))])
                         (k (promiseV promise-id) store stdout (schedule-computation-for-later promise-contents threads) proms))]
              
              ;; yield gives up the processor for now
              ;; we'll schedule ourselves for later and choose something else to run
              [yield ()
                     (context-switch (voidV) store stdout
                                     (schedule-computation-for-later k threads)
                                     proms)]

              ;; after creates a promise whose value would be resolved only after another promise has finished.
              ;; TODO 3 : find the bug in this implementation and fix it.  For a hint, the comments specify what we
              ;;          intend to do here!
              [after
               (promise-before after-callback)
               (helper/k
                promise-before env store stdout threads proms
                (λ (promise-v store-p stdout-p threads-p proms-p)
                  ; At this point, we check that promise-v is an actual promise:
                  (type-case D-PLE-value promise-v
                    [promiseV
                     (promise-id)
                     (helper/k
                      after-callback env store-p stdout-p threads-p proms-p
                      (λ (callback-v store-thn stdout-thn threads-thn proms-thn)
                        (type-case D-PLE-value callback-v
                          [closureV
                           (proc)
                           ;; We can now create the promise for after
                           (local ([define after-promise-id (next-promise-id)]
                                   [define after-behaviour
                                     ;; This continuation value will be called once the previous promise is done:
                                     (λ (previous-value current-store current-stdout current-threads current-proms)
                                       (proc previous-value current-store current-stdout current-threads current-proms
                                             (resolve-promise-and-continue after-promise-id)))]
                                   )
                             ;; We now must do the first scheduling!
                             (k (promiseV after-promise-id) store-thn stdout-thn threads-thn
                                proms-thn))]
                          [else (error "Then should be a procedure")])))] ;))
                    [else (error "after only works with a promise on first arg")])))]
              ;; TODO 4 : Implement "all".  We only provide you with a very minimal stub that does not have the appropriate behaviour.
              [all (a b)
                   (helper/k
                    a env store stdout threads proms
                    (λ (promise-a store-a stdout-a threads-a proms-a)
                      (type-case D-PLE-value promise-a
                        [promiseV
                         (promise-a-id)
                         (helper/k
                          b env store-a stdout-a threads-a proms-a
                          (λ (promise-b store-b stdout-b threads-b proms-b)
                            (type-case D-PLE-value promise-b
                              [promiseV
                               (promise-b-id)
                               (k (consV promise-a-id (consV promise-b-id (mtV))) store-b stdout-b
                                    threads-b
                                    proms-b)]
                              [else (error "all must take only promises")])))]
                        [else (error "all must take only promises")])))]
              [race
               (a b)
               (helper/k
                a env store stdout threads proms
                (λ (promise-a store-a stdout-a threads-a proms-a)
                  (type-case D-PLE-value promise-a
                    [promiseV
                     (promise-a-id)
                     (helper/k
                      b env store-a stdout-a threads-a proms-a
                      (λ (promise-b store-b stdout-b threads-b proms-b)
                        (type-case D-PLE-value promise-b
                          [promiseV (promise-b-id)
                                    (let* ([race-promise-id (next-promise-id)]
                                           [scheduled-a
                                            (schedule-computation-for-later
                                             (do-once-promise-is-resolved promise-a-id
                                                                          (resolve-promise-and-continue race-promise-id))
                                             threads-b)]
                                           [scheduled-both
                                            (schedule-computation-for-later
                                             (do-once-promise-is-resolved promise-b-id
                                                                          (resolve-promise-and-continue race-promise-id))
                                             scheduled-a)])
                                      (k (promiseV race-promise-id) store-b stdout-b
                                         scheduled-both
                                         proms-b))]
                          [else (error "race must take only promises")])))]
                    [else (error "race must take only promises")])))]
              [print-out (to-print)
                         (k (voidV) store (string-append stdout to-print) threads proms)]
              [resolved (p)
                        (helper/k p env store stdout threads proms
                                  (λ (promise-v store-p stdout-p threads-p proms-p)
                                    (type-case D-PLE-value promise-v
                                      [promiseV (promise-id)
                                                (context-switch (voidV) store-p stdout-p
                                                                ; Implicit yield here:
                                                                (schedule-computation-for-later (do-once-promise-is-resolved promise-id k)
                                                                                                threads-p)
                                                                proms-p)]
                                      [else (error "resolved must take only promises")])))]
              ))]
    ; Here we trampoline passing the arguments we got from the call to interp/k
    (helper/k exp env store stdout empty (mtRPs) k)))

(define (run sexp)
  (interp/k (desugar (parse sexp)) (mtEnv) (mtStore) ""
            (lambda (val sto stdout threads proms)
              (begin
                (if (not (equal? stdout ""))
                    (displayln stdout)
                    (void))
                val))))


;; TODO 2 : Fix this implementation.
;;          You want to look at the output of the tests and fix this!
(define (run-for-print sexp)
  (interp/k (desugar (parse sexp)) (mtEnv) (mtStore) ""
            (lambda args
              ;(map (lambda (l) (string-append l r)) "" (rest args))) 
              (first args)
              ))


(test (let/cc k (interp/k (id 'x) (anEnv 'x (numV 10) (mtEnv)) (mtStore) "" (lambda (v s sr th pr) (k v))))
      (numV 10))

(test (closureV? (let/cc k (interp/k (fun 'x (num 10)) (anEnv 'y (numV 100) (mtEnv)) (mtStore) ""
                                     (lambda (v s sr th pr) (k v)))))
      true)

(test (let/cc k (interp/k (app (fun 'x (id 'y))
                               (num 10)) (anEnv 'y (numV 100) (mtEnv)) (mtStore) ""
                                         (lambda (v s sr th pr) (k v))))
      (numV 100))

(test (run '{unbox {box 5}}) (numV 5))

(test (run '{seqn 0 1}) (numV 1))

(test (run '{with {b {box 5}}
                  {seqn {setbox! b 6}
                        {+ 10 {unbox b}}}})
      (numV 16))

(test (run '{setbox! {box 5} 6}) (numV 6))

(test (run '{with {b {box 5}}
                  {+ {seqn {setbox! b 6} {unbox b}}
                     {unbox b}}})
      (numV 12))

(test (run '{with {b {box 5}}
                  {+ {unbox b}
                     {seqn {setbox! b 6} {unbox b}}}})
      (numV 11))

(test (run '{with {b {box 0}} {seqn {box {setbox! b 1}} {unbox b}}}) (numV 1))

(test (run '10)
      (numV 10))

(test (run '{+ 10 100})
      (numV 110))

(test (run '{- 10 100})
      (numV -90))

(test (run '{* 10 100})
      (numV 1000))

(test (run '{+ {- 1 0} {* 3 4}})
      (numV 13))

(test (run '{if0 0 10 100})
      (numV 10))

(test (run '{if0 1 10 100})
      (numV 100))

(test (run '{cons-cell 4 {cons-cell 5 empty-cell}})
      (consV (numV 4) (consV (numV 5) (mtV))))

(test (run '{cons-cell 1 {cons-cell {seqn 5 {+ 3 2}} empty-cell}})
      (consV (numV 1) (consV (numV 5) (mtV))))

(test (run '{{fun {x} 10} 100})
      (numV 10))

(test (run-for-print '{seqn {print-out "a"} {print-out "b"}}) "ab")
(test (run-for-print '{+ {seqn {print-out "a"} {seqn {print-out "b"} 3}} {seqn {print-out "c"} 5}}) "abc")

;; test promises
;; processes yielding to each other
;; Note: This test (and other later ones) likely requires to fix TODO 3 to work!
(test (run-for-print '{resolved {after {promise {print-out "a"}} {fun {v} {print-out "c"}}}})
      "ac")

; Note: This test (and other later ones) requires to fix TODO 2 and TODO 3 to work!
(test (run-for-print '{with {a {promise {print-out "a"}}}
                            {after a {fun {v} {print-out "c"}}}})
      "")

(test/pred (run '{with {a {promise {print-out "a"}}}
                       {after a {fun {v} {print-out "c"}}}})
           promiseV?)

(test (run-for-print '{with {a {promise {seqn {print-out "a"} {print-out "b"}}}}
                            {resolved {after a {fun {v} {print-out "c"}}}}})
      "abc")

(test (run-for-print '{with {a {promise {seqn {print-out "a"} {seqn {yield} {print-out "b"}}}}}
                            {seqn {print-out "A"}
                                  {resolved {after a {fun {v} {print-out "c"}}}}}})
      "Aabc")

(test (run-for-print '{with {a {promise {seqn {print-out "a"} {seqn {yield} {print-out "b"}}}}}
                            {with {b {promise {print-out "giraffe"}}}
                                  {resolved {all a b}}}})
      "agiraffeb")

; not very nice of process a! but b gets scheduled at the end
(test (run-for-print '{with {a {promise {seqn {print-out "a"} {print-out "b"}}}}
                            {with {b {promise {print-out "giraffe"}}}
                                  {resolved {all a b}}}})
      "abgiraffe")

(test (run-for-print '{with {a {promise {seqn {print-out "a"} {seqn {yield} {print-out "b"}}}}}
                            {with {b {promise {seqn {print-out "giraffe"} {seqn {yield} {print-out "hello"}}}}}
                                  {resolved {all a b}}}})
      "agiraffebhello")


;; race
(test (run-for-print '{with {a {promise {seqn {print-out "a"} {seqn {yield} {print-out "b"}}}}}
                            {with {b {promise {print-out "giraffe"}}}
                                  {resolved {race a b}}}})
      "agiraffe")

; not very nice of process a! but b gets scheduled at the end
; notice this is because the {race} primitive is scheduled after both
(test (run-for-print '{with {a {promise {seqn {print-out "a"} {print-out "b"}}}}
                            {with {b {promise {print-out "giraffe"}}}
                                  {resolved {race a b}}}})
      "abgiraffe")

(test (run-for-print '{with {a {promise {seqn {print-out "a"} {seqn {yield} {print-out "b"}}}}}
                            {with {b {promise {seqn {print-out "giraffe"} {seqn {yield} {print-out "hello"}}}}}
                                  {resolved {race a b}}}})
      "agiraffebhello")

;; complex
(test (run-for-print '{with {a {promise {seqn {print-out "a"} {seqn {yield} {print-out "b"}}}}}
                            {with {b {promise {seqn {yield} {seqn {print-out "giraffe"} {print-out "hello"}}}}}
                                  {resolved {race a b}}}})
      "abgiraffehello")

(test (run-for-print '{with {a {promise {seqn {print-out "a"} {seqn {yield} {print-out "b"}}}}}
                            {with {b {promise {seqn {yield} {seqn {yield} {seqn {print-out "giraffe"} {print-out "hello"}}}}}}
                                  {resolved {race a b}}}})
      "ab")

(test (run-for-print '{with {a {promise {seqn {print-out "a"} {seqn {yield} {print-out "b"}}}}}
                            {with {b {promise {seqn {yield} {seqn {print-out "giraffe"} {seqn {yield} {print-out "hello"}}}}}}
                                  {resolved {race a b}}}})
      "abgiraffe")

;; will actually terminate early
(test (run-for-print '{with {a {promise {seqn {print-out "a"} {print-out "b"}}}}
                            {with {b {promise {seqn {yield} {seqn {print-out "giraffe"} {print-out "hello"}}}}}
                                  {resolved {race a b}}}})
      "ab")

;;  we would expect promise c to return first before b
;; a doesn't do anything
;; between promise a and c, even though a is a step ahead of the nested promise
;; it yields one more time
(test (run-for-print '{with {a {promise {seqn {yield} {yield}}}}
                            {with {b {promise {seqn {yield} {seqn {print-out "giraffe"} {promise {seqn {print-out "promise-b"} 2}}}}}}
                                  {with {c {promise {seqn {print-out "heidi"} {promise {seqn {print-out "promise-c"} 1}}}}}
                                        {resolved {race a {race b c}}}}}})
      "heidigiraffepromise-c")


;; race never dequeues the promise that didn't finish, so continues executing after call to race completes
;; so even though the continuation doesn't actually get executed
;; we write to the stdout when we invoke it
;; which means if we have mutation, this might be really bad 
(test (run-for-print '{with {a {promise {seqn {yield} {seqn {yield} {print-out "promise-a"}}}}}
                            {with {b {promise {seqn {yield} {seqn {print-out "giraffe"} {promise {seqn {print-out "promise-b"} 2}}}}}}
                                  {with {c {promise {seqn {print-out "heidi"} {promise {seqn {print-out "promise-c"} 1}}}}}
                                        {resolved {all a {race b c}}}}}})
      "heidigiraffepromise-cpromise-apromise-b")

;; notice that we still only return the value that resolves first
(test (run '{with {a {promise {seqn {yield} {seqn {yield} {seqn {print-out "promise-a"} 1}}}}}
                  {with {b {promise {seqn {yield} {seqn {print-out "giraffe"} {promise {seqn {print-out "promise-b"} 2}}}}}}
                        {with {c {promise {seqn {print-out "heidi"} {promise {seqn {print-out "promise-c"} 3}}}}}
                              {resolved {race a {race b c}}}}}})
      (numV 3))

;; mutation issues
(test (run '{with {i {box 5}}
                  {with {b {promise {seqn {yield} {seqn {print-out "giraffe"} {promise {seqn {print-out "promise-b"} {setbox! i 15}}}}}}}
                        {with {c {promise {seqn {print-out "heidi"} {promise {seqn {print-out "promise-c"}  3}}}}}
                              {resolved {after {race b c} {fun {val} {+ val {unbox i}}}}}}}})
      
      (numV 8))

;; need to delay the checking behaviour of after just one more yield
;; to see the mutation effect possibly 
(test (run '{with {i {box 5}}
                  {with {b {promise {seqn {yield} {seqn {print-out "giraffe"} {promise {seqn {print-out "promise-b"} {setbox! i 15}}}}}}}
                        {with {c {promise {seqn {print-out "heidi"} {promise {seqn {print-out "promise-c"} 3}}}}}
                              {seqn {yield} {resolved {after {race b c} {fun {val} {+ val {unbox i}}}}}}}}}) ; delay so that the check is delayed past eval of b
      
      (numV 18))

(test (run '{with {a {promise 5}}
                  {resolved {all {after a {fun {val} {+ 1 val}}}
                                 a}}})
      (consV (numV 6) (consV (numV 5) (mtV))))

(test (run-for-print '{with {a {promise {seqn {print-out "a"} 5}}}
                            {resolved {after {after a {fun {val} {promise {seqn {print-out "b"} 4}}}}
                                             {fun {val} {print-out "c"}}}}})
      "abc")

(test (run '{with {a {promise {seqn {print-out "a"} 5}}}
                  {resolved {after {after a {fun {val} {promise {seqn {print-out "b"}  {+ val 4}}}}}
                                   {fun {val} {+ val 1}}}}})
      (numV 10))

;; using all, we should get a list of values
(test (run '{with {a {promise {seqn {print-out "a"} {seqn {yield} 5}}}}
                  {with {b {promise {seqn {print-out "giraffe"} 9}}}
                        {resolved {all a b}}}})
      (consV (numV 5) (consV (numV 9) (mtV))))

(test/pred (run '{with {a {promise {seqn {print-out "a"} {seqn {yield} 5}}}}
                  {with {b {promise {seqn {print-out "giraffe"} 9}}}
                        {all a b}}})
      promiseV?)

(test (run-for-print '{with {a {promise {seqn {print-out "a"} {seqn {yield} 5}}}}
                            {with {b {promise {seqn {print-out "giraffe"} 9}}}
                                  {all a b}}})
      "")

;; using race, we should only get one value 
(test (run '{with {a {promise {seqn {print-out "a"} {seqn {yield} 5}}}}
                  {with {b {promise {seqn {print-out "giraffe"} 9}}}
                        {resolved {race a b}}}})
      (numV 9))

(test (run '{with {a {promise {seqn {print-out "a"} 5}}}
                  {with {b {promise {seqn {print-out "giraffe"} {seqn {yield} 9}}}}
                        {resolved {race a b}}}})
      (numV 5))

(test/pred (run '{with {a {promise {seqn {print-out "a"} 5}}}
                  {with {b {promise {seqn {print-out "giraffe"} {seqn {yield} 9}}}}
                        {race a b}}})
           promiseV?)
(test (run-for-print '{with {a {promise {seqn {print-out "a"} 5}}}
                            {with {b {promise {seqn {print-out "giraffe"} {seqn {yield} 9}}}}
                                  {race a b}}})
           "")
