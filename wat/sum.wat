(module
  (memory $mem 0)
  (func $avg (param $addr i32) (param $length i32) (local $accum i32)
      (local $counter i32)
    (local.get $length)
    (local.set $counter)
    (loop $add_to_sum (result i32)
       (local.get $accum)
       (local.get $addr)
       (i32.load)
       (i32.add)
       (local.set $accum)

       (local.get $counter)
       (i32.const 1)
       (i32.sub)
       (local.tee $counter)

       (i32.eqz)
       (i32.eqz) ;; negate
       (br_if $add_to_sum)

       (local.get $accum)
    )
    return
  )
)
