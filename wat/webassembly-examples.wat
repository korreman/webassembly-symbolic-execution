(module
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    get_local $lhs
    get_local $rhs
    i32.add)
  (export "add" (func $add))

  (func $max (param $a i32) (param $b i32) (result i32)
    get_local $a
    get_local $b
    i32.ge_s
    (if (result i32)
      (then
        (get_local $a)
      )
      (else
        (get_local $b)
      )
    )
  )
  (export "max" (func $max))

  (func $pow (param $val i32) (param $power i32) (result i32) (local $acc i32)
    i32.const 1
    set_local $acc
    (loop (result i32)
      get_local $acc
      i32.const 0
      get_local $power
      i32.ge_u
      br_if 1
      get_local $power
      i32.const 1
      i32.sub
      set_local $power
      get_local $val
      i32.mul
      set_local $acc
      br 0
    )
  )
  (export "pow" (func $pow))

  (func $pow_alt (param $val i32) (param $power i32) (result i32) (local $acc i32)
    i32.const 1
    set_local $acc
    (loop (result i32)
      get_local $acc
      i32.const 0
      br_if 1 (i32.ge_u (get_local $power) (i32.const 0))
      i32.sub (get_local $power) (i32.const 1)
      set_local $power
      get_local $val
      i32.mul
      set_local $acc
      br 0
    )
  )
  (export "pow_alt" (func $pow_alt))
)
