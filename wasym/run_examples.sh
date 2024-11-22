#!/usr/bin/bash
stack run test/examples.wasm unreach_good none unreachable
stack run test/examples.wasm unreach_bad none unreachable
stack run test/examples.wasm divzero_bad none intDivZero
stack run test/examples.wasm modulo_div_bad none intDivZero
stack run test/examples.wasm modulo_div_good none intDivZero
stack run test/examples.wasm modulo_func_bad none intDivZero
stack run test/examples.wasm inside_func_bad none intDivZero
stack run test/examples.wasm loopy_func_bad none unreachable
