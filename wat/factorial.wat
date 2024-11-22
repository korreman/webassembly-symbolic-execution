(module
  (func $factorial (param $n i32) (result i32) (local $acc i32)
    i32.const 1
    local.set $acc
    (loop
      local.get $acc
      local.get $n
      i32.mul
      local.set $acc

      local.get $n
      i32.const 1
      i32.sub
      local.tee $n
      br_if 0
    )
    local.get $acc
    return
  )
)
