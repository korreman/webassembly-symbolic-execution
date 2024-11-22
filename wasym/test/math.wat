;; Testing conditionals mainly through loopy mathematic calculations
(module
  ;; Is the output of the two fibonacci functions equal?
  (export "test_fibo_equiv" (func $test_fibo_equiv))
  (func $test_fibo_equiv (param $n i32)
    local.get $n
    call $fibo_rec
    local.get $n
    call $fibo_loop

    i32.eq
    (if (then unreachable))
  )

  ;; Calculates the nth fibonacci number using a loop method
  (export "fibo_loop" (func $fibo_loop))
  (func $fibo_loop (param $n i32)
                   (result i32)
                   (local $acc i32)
                   (local $acc_old i32)
    ;; Initialize both accumulators to 1
    i32.const 1
    local.tee $acc
    local.set $acc_old

    (loop $add_loop
      ;; Retrieve both accumulators, add them together. Write the new
      ;; accumulator to the old one, write the result to the new accumulator
      local.get $acc_old
      local.get $acc
      local.tee $acc_old
      i32.add
      local.set $acc

      ;; Repeat n times
      local.get $n
      i32.const 1
      i32.sub
      local.tee $n
      br_if $add_loop
    )
    local.get $acc
  )

  ;; Calculates the nth fibonacci number using a recursive method
  (export "fibo_rec" (func $fibo_rec))
  (func $fibo_rec (param $n i32) (result i32)
    local.get $n
    i32.const 1
    i32.lt_u
    (if (result i32) (then
      i32.const 1
    ) (else
      local.get $n
      i32.const 1
      i32.sub
      call $fibo_rec

      local.get $n
      i32.const 2
      i32.sub
      call $fibo_rec

      i32.add
    ))
  )

  ;; Calculates n factorial using a loop method
  (export "factorial" (func $factorial))
  (func $factorial (param $n i32) (result i32) (local $acc i32)
    ;; Initialize the accumulator to one
    i32.const 1
    local.set $acc

    (loop
      ;; Multiply n into the accumulator
      local.get $acc
      local.get $n
      i32.mul
      local.set $acc

      ;; Subtract 1 from n
      local.get $n
      i32.const 1
      i32.sub
      local.tee $n

      ;; Repeat of n is non-zero
      br_if 0
    )
    local.get $acc
  )

  ;; Integer power function for non-negative exponents
  (export "power" (func $power))
  (func $power (param $val i32) (param $power i32) (result i32)
                   (local $acc i32)
    ;; Initialize the accumulator to 1
    i32.const 1
    local.set $acc

    (loop $mul_loop
      (block $mul_block
        ;; Subtract 1 from power
        local.get $power
        i32.const 1
        i32.sub
        local.tee $power

        ;; If power is less than 0, break out of loop
        i32.const 0
        i32.lt_u
        br_if $mul_block

        ;; Multiply the value into the accumulator
        local.get $acc
        local.get $val
        i32.mul
        local.set $acc

        ;; Repeat
        br $mul_loop
      )
    )
    local.get $acc
  )

  ;; Is the output of the two triangular functions equal?
  (export "test_triangular_equiv" (func $test_triangular_equiv))
  (func $test_triangular_equiv (param $n i32)
    local.get $n
    call $triangular_expr
    local.get $n
    call $triangular_loop

    i32.eq
    (if (then unreachable))
  )

  ;; Triangular sum, using loop method
  (export "triangular_loop" (func $triangular_loop))
  (func $triangular_loop (param $n i32) (result i32) (local $acc i32)
    ;; Initialize accumulator to zero
    i32.const 0
    local.set $acc

    (loop
      ;; Add n to accumulator
      local.get $acc
      local.get $n
      i32.add
      local.set $acc

      ;; Subtract 1 from n
      local.get $n
      i32.const 1
      i32.sub
      local.tee $n

      ;; Repeat if n is non-zero
      br_if 0
    )

    local.get $acc
  )

  ;; Triangular sum, using constant expression
  (export "triangular_expr" (func $triangular_expr))
  (func $triangular_expr (param $n i32) (result i32)
    local.get $n
    local.get $n
    i32.const 1
    i32.sub
    i32.mul
    i32.const 2
    i32.div_u
  )

  ;; Is the output of the two gcd functions equal?
  (export "test_gcd_equiv" (func $test_gcd_equiv))
  (func $test_gcd_equiv (param $a i32) (param $b i32)
    local.get $a
    local.get $b
    call $gcd_rec

    local.get $a
    local.get $b
    call $gcd_loop

    i32.eq
    (if (then) (else
      unreachable
    ))
  )

  ;; Calculate the greatest common divisor using a loop
  (export "gcd_loop" (func $gcd_loop))
  (func $gcd_loop (param $a i32) (param $b i32) (result i32)
    (loop $l
      ;; If b is zero, we're done and can fallthrough to exit the loop
      local.get $b
      (if (then
        ;; Calculate a mod b
        local.get $a
        local.get $b
        i32.rem_u

        ;; Write b to a, write (a mod b) to b
        local.get $b
        local.set $a
        local.set $b
        br $l
      ))
    )
    local.get $a
  )

  ;; Calculate the greatest common divisor using recursion
  (export "gcd_rec" (func $test_gcd_equiv))
  (func $gcd_rec (param $a i32) (param $b i32) (result i32)
    local.get $b
    (if (result i32) (then
      local.get $b
      local.get $a
      local.get $b
      i32.rem_u
      call $gcd_rec
    ) (else
      local.get $a
    ))
  )

  ;; Collatz conjecture
  (export "collatz_loop" (func $collatz_loop))
  (func $collatz_loop (param $n i32)
    (loop $l
      (block $b
        ;; Quit if the number equals 1
        local.get $n
        i32.const 1
        i32.eq
        br_if $b

        ;; Test whether n is even
        local.get $n
        i32.const 2
        i32.rem_u
        (if (result i32) (then
          ;; 3n + 1
          local.get $n
          i32.const 3
          i32.mul
          i32.const 1
          i32.add
        ) (else
          ;; n / 2
          local.get $n
          i32.const 2
          i32.div_u
        ))
        local.set $n
        br $l
      )
    )
  )
)
