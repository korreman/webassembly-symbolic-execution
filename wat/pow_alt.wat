(module
  (func $pow_alt (param $val i32) (param $power i32) (result i32) (local $acc i32)
    (local.set $acc (i32.const 1))
    (loop (result i32)
      local.get $acc
      (br_if 1 (i32.ge_u (i32.const 0) (local.get $power)))
      drop
      (local.set $power (i32.sub (local.get $power) (i32.const 1)))
      (local.set $acc (i32.mul (local.get $acc) (local.get $val)))
      br 0
    )
  )
  (export "pow_alt" (func $pow_alt))
)
