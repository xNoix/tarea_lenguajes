#lang plait

;; =============================================================================
;; Interpreter: interpreter.rkt
;; =============================================================================

(require "support.rkt")

(define (eval [str : S-Exp]): Value
  (interp (desugar (parse str))))

;; DO NOT EDIT ABOVE THIS LINE =================================================

(define (desugar [expr : Expr]): Expr
  (type-case Expr expr
    [(sugar-and [left : Expr] [right : Expr])
      (cond 
        [(interp left)
          (cond
            [(interp right)
              [(v-bool true)]
            ]
            [else (v-bool false)]
          )
        ]
        [else (v-bool false)]
      )
    ]
    [(sugar-or [left : Expr][right : Expr])
      (cond
        [(interp left)
          [(v-bool true)]
        ]
        [else
          (cond
            [(interp right)
              [(v-bool true)]
            ]
            [else (v-bool false)]
          )
        ]
      )
    ]
    [(sugar-let [id : Symbol][value : Expr][body : Expr])
      (e-app (e-lam (interp id) (interp value)) (interp body))
    ]
  )
)

(define (interp [expr : Expr] [env : Env]): Value
  (type-case Expr expr
    [(e-num [value : Number]) (v-num value)]
    [(e-str [value : String]) (v-str value)]
    [(e-bool [value : Boolean]) (v-bool value)]
    [(e-op [op : Operator] [left : Expr] [right : Expr])
      (cond
        [(eq? op 'op-plus) 
          (cond
            [(sugar-and (v-num? (interp left)) (v-num? (interp right)))
              (+ (interp left) (interp right))
            ]
            [else (err-bad-arg-to-op op left)]
          )
        ]
        [(eq? op 'op-append) 
          (cond
            [(sugar-and (v-str? (interp left)) (v-str? (interp right)))
              (string-append (interp left) (interp right))
            ]
            [else (err-bad-arg-to-op op left)]
          )
        ]
        [(eq? op 'op-str-eq)
          (cond
            [(v-str? left) (string=? (interp left))]
            [else (err-bad-arg-to-op op left)]
          ) 
        ]
        [(eq? op 'op-num-eq)
          (cond
            [(v-num? left) (= (interp left))]
            [else (err-bad-arg-to-op op left)]
          )
        ]
    
      )
    ]
    [(e-if [cond: Expr] [consq: Expr] [altern: Expr])
      (cond
        [(v-bool? (interp cond))
          (cond
            [(interp cond)
              [(interp consq)]
            ]
            [else (interp altern)]
          )
        ]
        [else (err-if-got-non-boolean cond)]
      )
    ]
    [(e-lam param body) (v-fun param body env)]
    [(e-app func arg)(local ([define fd (interp func env)])
      (interp (v-fun-body fd)
        (extend-env (bind (v-fun-arg fd)
          (interp arg env)
        ) mt-env)
      )
    )]
    [(e-id name) (lookup name env) ]
  )
)

(define (lookup [for : Symbol] [env : Env]) : Value
  (cond
    [(empty? env) (err-unbound-id)]
    [else (cond
            [(symbol=? for (bind-name (first env)))
              (bind-val (first env))]
            [else (lookup for (rest env))])]
  )
)