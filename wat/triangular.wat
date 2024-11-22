(module
  (func $triangular (param $n i32) (result i32) (local $acc i32)
    i32.const 0
    local.set $acc
    (loop
      local.get $acc
      local.get $n
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
)
