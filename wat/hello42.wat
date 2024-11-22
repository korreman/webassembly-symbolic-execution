(module
  (import "host" "print" (func $pri (param i32)))
  (func $awesome
    i32.const 42
    call $pri)
  (export "awesomefun" (func $awesome))
)
