(module
  (func $test_sort (param $length i32) (local $inc i32)
    (call $quicksort (i32.const 0) (local.get $length))
    (loop
      (if (i32.gt_u
          (i32.load (local.get $inc))
          (i32.load (i32.add (local.get $inc) (i32.const 4))))
        (then unreachable)
      )
      (local.set $inc (i32.sub (local.get $inc) (i32.const 4)))
      (br_if 0
        (i32.lt_u (local.get $inc) (i32.mul (local.get $length) (i32.const 4)))
      )
    )
  )

  (func $quicksort (param $start i32) (param $end i32) (local $pivot i32)
    (local.set $pivot (call $partition (local.get $start) (local.get $end)))
    (call $quicksort
      (local.get $start)
      (i32.sub (local.get $pivot) (i32.const 4))
    )

    (call $quicksort
      (i32.add (local.get $pivot) (i32.const 4))
      (local.get $end)
    )
  )

  (func $partition (param $start i32) (param $end i32) (result i32)
                   (local $pivot_val i32) (local $lo i32) (local $hi i32)

    ;; Set pivot value to the leftmost element
    (local.set $pivot_val (i32.load (local.get $end)))

    ;; Set low element and high element
    (local.set $lo (local.get $start))
    (local.set $hi (i32.sub (local.get $end) (i32.const 1)))

    (loop $L1
      (block $inner
        ;; Advance the low element right if value isn't higher than the pivot
        (if (i32.le_u (i32.load (local.get $lo)) (local.get $pivot_val))
          (local.set $lo (i32.add (local.get $lo) (i32.const 4)))
        )

        ;; Advance the high element left if value isn't lower than the pivot
        (if (i32.ge_u (i32.load (local.get $hi)) (local.get $pivot_val))
          (local.set $hi (i32.sub (local.get $hi) (i32.const 4)))
        )

        ;; Exits loop if low and high elements have reached each other
        (br_if $inner (i32.ge_u (local.get $lo) (local.get $hi)))

        (if (i32.and
            (i32.gt_u (i32.load (local.get $lo)) (local.get $pivot_val))
            (i32.lt_u (i32.load (local.get $hi)) (local.get $pivot_val)))
          (call $swap (local.get $lo) (local.get $hi))
        )
        br $L1
      )
    )
    ;; Lo is either the same as Hi, or has moved into Hi territory
    ;; Good candidate for artificial error
    (call $swap (local.get $lo) (local.get $end))
    local.get $hi
  )

  (func $swap (param $addrA i32) (param $addrB i32)
    local.get $addrA
    (i32.load (local.get $addrA))
    local.get $addrB
    (i32.load (local.get $addrB))
    i32.store
    i32.store
  )

  (memory $mem 0)
  (export "quicksort" (func $quicksort))
  (export "memory" (memory 0))
)
