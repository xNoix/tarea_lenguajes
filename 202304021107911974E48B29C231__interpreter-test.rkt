#lang racket

;; =============================================================================
;; Interpreter: interpreter-tests.rkt
;; =============================================================================

(require (only-in "interpreter.rkt" eval)
         "support.rkt"
         "test-support.rkt")

;; DO NOT EDIT ABOVE THIS LINE =================================================

(define/provide-test-suite interpreter-tests ;; DO NOT EDIT THIS LINE ==========
  ; TODO: Add your own tests below!

  ; Here, we provide some examples of how to use the testing forms provided in
  ; "test-support.rkt". You should not use any external testing library other
  ; than those specifically provided; otherwise, we will not be able to grade
  ; your code.
  (test-equal? "Works with Num primitive"
               (eval `2) (v-num 2))
  (test-true "Works with lambda"
             (v-fun? (eval `{lam x 5})))
  (test-pred "Equivalent to the test case above, but with test-pred"
             v-fun? (eval `{lam x 5}))
  (test-raises-interp-error? "Passing Str to + results in err-bad-arg-to-op"
                             (eval `{+ "bad" 1})
                             (err-bad-arg-to-op (op-plus) (v-str "bad"))))

;; DO NOT EDIT BELOW THIS LINE =================================================

(run-tests interpreter-tests)
