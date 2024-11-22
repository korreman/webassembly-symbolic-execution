(module
;; The `unchecked_average` function will try to divide by zero if given an empty
;; array. It's not really a failure per se, but the engine can demonstrate its
;; ability to catch this.
  (export "unchecked_average" (func $unchecked_average))
  (func $unchecked_average (param $addr i32) (param $length i32) (result i32)
                        (local $acc i32) (local $counter i32)
    (local.set $counter (local.get $length))
    (loop $sum
      (block $sum_inner
        (br_if $sum_inner (i32.le_u (local.get $counter) (i32.const 0)))
        (local.set $acc (i32.add
          (i32.load (local.get $addr))
          (local.get $acc)
        ))
        (local.tee $counter (i32.sub
          (local.get $counter)
          (i32.const 1)
        ))
        br $sum
      )
    )
    (i32.div_u
      (local.get $acc)
      (local.get $length)
    )
  )

;; This one verifies that the input is positive before running
  (export "average" (func $average))
  (func $average (param $addr i32) (param $length i32) (result i32)
                 (local $acc i32) (local $counter i32)
    (if (result i32) (i32.lt_u (local.get $counter) (i32.const 1))
      (then i32.const 0)
      (else
        local.get $addr
        local.get $length
        call $unchecked_average
      )
    )
  )

  (memory $mem 0)
  (export "memory" (memory 0))
)
