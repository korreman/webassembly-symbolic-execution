(module
  (func $fibonacci (param $n i32)
                    (result i32)
                    (local $acc i32)
                    (local $acc_prev i32)
    i32.const 1
    local.tee $acc
    local.set $acc_prev
    (loop
      local.get $acc_prev
      local.get $acc
      local.tee $acc_prev
      i32.add
      local.set $acc

      local.get $n
      i32.const 1
      i32.sub
      local.tee $n
      i32.eqz
      i32.eqz
      br_if 0
    )
    local.get $acc
    return
  )
  (export "fibonacci" (func $fibonacci))
)
