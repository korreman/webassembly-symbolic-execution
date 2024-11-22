;; Various tests. The postfix indicates whether the function can reach an
;; undesirable state - functions labeled with "good" cannot

(module
  ;; Contains an unreachable instruction which is indeed unreachable
  (export "unreach_good" (func $unreach_good))
  (func $unreach_good (param $a i32) (param $b i32) (param $c i32)
                          (param $d i32)
                          (local $r1 i32) (local $r2 i32) (local $r3 i32)
                          (local $r4 i32)

    (local.set $r1 (i32.gt_u (local.get $a) (local.get $b)))
    (local.set $r2 (i32.gt_u (local.get $b) (local.get $c)))
    (local.set $r3 (i32.gt_u (local.get $c) (local.get $d)))
    (local.set $r4 (i32.gt_u (local.get $d) (local.get $a)))

    (if (local.get $r1)
    (if (local.get $r2)
    (if (local.get $r3)
    (if (local.get $r4) (unreachable)))))
  )

  ;; Contains an unreachable instruction which can be reached - should fail
  (export "unreach_bad" (func $unreach_bad))
  (func $unreach_bad (param $a i32) (param $b i32) (param $c i32)
                         (param $d i32)
                         (local $r1 i32) (local $r2 i32) (local $r3 i32)
                         (local $r4 i32)

    (local.set $r1 (i32.gt_u (local.get $a) (local.get $b)))
    (local.set $r2 (i32.gt_u (local.get $b) (local.get $c)))
    (local.set $r3 (i32.gt_u (local.get $c) (local.get $d)))
    (local.set $r4 (i32.gt_u (local.get $d) (local.get $a)))

    (if (local.get $r1)
    (if (local.get $r2)
    (if (local.get $r3)
    (if (local.get $r4) (nop) (unreachable)))))
  )

  ;; Direct division by zero test
  (export "divzero_bad" (func $divzero_bad))
  (func $divzero_bad (param $n i32) (result i32)
     local.get $n
     i32.const 0
     i32.div_u
  )

  ;; A layer of indirection. Does check that y is non-zero, but not that the
  ;; remainder from dividing x by is non-zero.
  (export "modulo_div_bad" (func $modulo_div_bad))
  (func $modulo_div_bad (param $x i32) (param $y i32) (param $z i32)
                        (result i32)
    local.get $y
    (if (result i32) (then
      local.get $z
      local.get $x
      local.get $y
      i32.rem_u
      i32.div_u
    ) (else
      i32.const -1
    ))
  )

  ;; A layer of indirection, but properly checked.
  (export "modulo_div_good" (func $modulo_div_good))
  (func $modulo_div_good (param $x i32) (param $y i32) (param $z i32)
                         (result i32)
    local.get $y
    (if (result i32) (then
      local.get $z
      local.get $x
      local.get $y
      i32.rem_u
      local.set $y
      local.get $y
      (if (result i32) (then
        local.get $y
      ) (else
        i32.const -1
      ))
      i32.div_u
    ) (else
      i32.const -1
    ))
  )

  ;; Another layer of indirection, this time demonstrating a function call.
  (export "modulo_func_bad" (func $modulo_func_bad))
  (func $modulo_func_bad (param $w i32) (param $x i32) (param $y i32)
                         (param $z i32)
    local.get $w

    local.get $x
    local.get $y
    local.get $z
    call $modulo_div_good

    i32.div_u
    drop
  )

  ;; Demonstrate that assertions can be broken inside function calls
  (export "inside_func_bad" (func $inside_func_bad))
  (func $inside_func_bad (param $x i32) (param $y i32) (param $z i32)
    local.get $x
    local.get $y
    local.get $z
    call $modulo_div_bad
    drop
  )

  ;; A function which will encounter an unreachable after running a loop for
  ;; some time
  (export "loopy_func_bad" (func $loopy_func_bad))
  (func $loopy_func_bad (param $x i32) (param $y i32)
    ;; Limit the initial value: x < 100
    local.get $x
    i32.const 100
    i32.lt_u

    ;; Limit y: y < 30
    local.get $y
    i32.const 30
    i32.lt_u

    ;; Combined constraints
    i32.and
    (if (then
      (loop $l
        ;; Add y to x
        local.get $x
        local.get $y
        i32.add
        local.set $x

        ;; If we surpass a constant, fail
        local.get $x
        i32.const 10000
        i32.gt_u
        (if (then unreachable))

        ;; Keep looping until x is larger than our constant
        local.get $x
        i32.const 10000
        i32.lt_u
        br_if $l
      )
    ))
  )
)
