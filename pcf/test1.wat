(module
  (memory 1 10)
  (global $HEAP (mut i32) (i32.const 0)) ;; heap pointer initialized to 0
  (global $ENV  (mut i32) (i32.const 0)) ;; env pointer initialized to NIL
  (global $ACC  (mut i32) (i32.const 999)) ;; accumulator initialized to 999
  (global $LIST i32 (i32.const 1))       ;; LIST tag (for non empty lists)
  (global $NIL  i32 (i32.const 0))       ;; NIL tag (for empty lists)
  (func $head (param $list i32) (result i32)
    (i32.load (i32.add (local.get $list (i32.const 4))))
    return)
  (func $tail (param $list i32) (result i32)
    (i32.load (i32.add (local.get $list (i32.const 8))))
    return)
  ;; a cons is stored as a block of 3 words: a LIST tag, the head and the tail  
  (func $cons (param $head i32) (param $tail i32) (result i32)
    (local $result i32)
    (local.set $result (global.get $HEAP))
    (i32.store (global.get $HEAP) (global.get $LIST))
    (global.set $HEAP (i32.add (global.get $HEAP) (i32.const 4)))
    (i32.store (global.get $HEAP) (local.get $head))
    (global.set $HEAP (i32.add (global.get $HEAP) (i32.const 4)))
    (i32.store (global.get $HEAP) (local.get $tail))
    (global.set $HEAP (i32.add (global.get $HEAP) (i32.const 4)))
    (local.get $result)
    return)
    ;; retrieves the element $n of the list $list (starting from 0)
  ;; precondition: the size of the list is greater than $n
  (func $search (param $n i32) (param $list i32) (result i32)
    (local.get $n)
    (if (result i32)
      (then            ;; n is non zero
       (i32.sub (local.get $n) (i32.const 1))
       (local.get $list)
       (call $tail)
       (call $search))
      (else            ;; n is zero
       (local.get $list)
       (call $head)))
    return)
  (func (export "main") (result i32)
    i32.const 9

  i32.const 6

  i32.sub

  return))