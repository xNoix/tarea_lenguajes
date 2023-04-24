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
    [(sugar-and [left : Expr][right : Expr])
      (e-if (interp left)
        #f
        (e-if (interp right)
            #f
          #t
        )
      )
    ]
    [(sugar-or [left : Expr][right : Expr])
      (e-if (interp left)
        (e-if (interp right)
          #f
          #t
        )
        #t
      )
    ]
    [(sugar-let [id : Symbol][value : Expr][body : Expr])
      (e-app (e-lam (interp id) (interp value)) (interp body))
    ]
  )
)

(define (interp [expr : Expr]): Value
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
    [(e-lam [param: Symbol] [body: Expr])
      (lambda (param)
        body
      )
    ]
    [(e-app [func: Expr] [arg: Expr])]
    [(e-id [name: Symbol])
      (cond
        []
      )
    ]
  )
)