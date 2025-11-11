use forth::primitives::{Stack, VM, Primitive, ForthError};

#[test]
fn test_stack_operations() {
    let mut stack = Stack::new();
    assert!(stack.is_empty());

    stack.push(42);
    assert_eq!(stack.depth(), 1);
    assert_eq!(stack.peek().unwrap(), 42);

    stack.push(99);
    assert_eq!(stack.depth(), 2);
    assert_eq!(stack.pop().unwrap(), 99);
    assert_eq!(stack.pop().unwrap(), 42);

    assert!(stack.pop().is_err());
}

#[test]
fn test_stack_get_and_iter() {
    let mut stack = Stack::new();
    stack.push(10);
    stack.push(20);
    stack.push(30);

    // Test get method (0 = bottom of stack)
    assert_eq!(stack.get(0), Some(10));
    assert_eq!(stack.get(1), Some(20));
    assert_eq!(stack.get(2), Some(30));
    assert_eq!(stack.get(3), None);

    // Test iter method
    let values: Vec<i64> = stack.iter().copied().collect();
    assert_eq!(values, vec![10, 20, 30]);
}

#[test]
fn test_dup() {
    let mut vm = VM::new();
    vm.data_stack.push(42);
    vm.execute_primitive(Primitive::Dup).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 42);
    assert_eq!(vm.data_stack.pop().unwrap(), 42);
}

#[test]
fn test_drop() {
    let mut vm = VM::new();
    vm.data_stack.push(42);
    vm.data_stack.push(99);
    vm.execute_primitive(Primitive::Drop).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 42);
}

#[test]
fn test_swap() {
    let mut vm = VM::new();
    vm.data_stack.push(1);
    vm.data_stack.push(2);
    vm.execute_primitive(Primitive::Swap).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 1);
    assert_eq!(vm.data_stack.pop().unwrap(), 2);
}

#[test]
fn test_over() {
    let mut vm = VM::new();
    vm.data_stack.push(1);
    vm.data_stack.push(2);
    vm.execute_primitive(Primitive::Over).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 1);
    assert_eq!(vm.data_stack.pop().unwrap(), 2);
    assert_eq!(vm.data_stack.pop().unwrap(), 1);
}

#[test]
fn test_rot() {
    let mut vm = VM::new();
    vm.data_stack.push(1);
    vm.data_stack.push(2);
    vm.data_stack.push(3);
    vm.execute_primitive(Primitive::Rot).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 1);
    assert_eq!(vm.data_stack.pop().unwrap(), 3);
    assert_eq!(vm.data_stack.pop().unwrap(), 2);
}

#[test]
fn test_add() {
    let mut vm = VM::new();
    vm.data_stack.push(3);
    vm.data_stack.push(4);
    vm.execute_primitive(Primitive::Add).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 7);
}

#[test]
fn test_sub() {
    let mut vm = VM::new();
    vm.data_stack.push(10);
    vm.data_stack.push(3);
    vm.execute_primitive(Primitive::Sub).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 7);
}

#[test]
fn test_mul() {
    let mut vm = VM::new();
    vm.data_stack.push(6);
    vm.data_stack.push(7);
    vm.execute_primitive(Primitive::Mul).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 42);
}

#[test]
fn test_div() {
    let mut vm = VM::new();
    vm.data_stack.push(20);
    vm.data_stack.push(4);
    vm.execute_primitive(Primitive::Div).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 5);
}

#[test]
fn test_div_by_zero() {
    let mut vm = VM::new();
    vm.data_stack.push(10);
    vm.data_stack.push(0);
    assert_eq!(
        vm.execute_primitive(Primitive::Div),
        Err(ForthError::DivisionByZero)
    );
}

#[test]
fn test_mod() {
    let mut vm = VM::new();
    vm.data_stack.push(17);
    vm.data_stack.push(5);
    vm.execute_primitive(Primitive::Mod).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 2);
}

#[test]
fn test_equals() {
    let mut vm = VM::new();
    vm.data_stack.push(5);
    vm.data_stack.push(5);
    vm.execute_primitive(Primitive::Equals).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), -1); // true

    vm.data_stack.push(5);
    vm.data_stack.push(6);
    vm.execute_primitive(Primitive::Equals).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 0); // false
}

#[test]
fn test_less() {
    let mut vm = VM::new();
    vm.data_stack.push(3);
    vm.data_stack.push(5);
    vm.execute_primitive(Primitive::Less).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), -1); // true

    vm.data_stack.push(5);
    vm.data_stack.push(3);
    vm.execute_primitive(Primitive::Less).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 0); // false
}

#[test]
fn test_greater() {
    let mut vm = VM::new();
    vm.data_stack.push(5);
    vm.data_stack.push(3);
    vm.execute_primitive(Primitive::Greater).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), -1); // true

    vm.data_stack.push(3);
    vm.data_stack.push(5);
    vm.execute_primitive(Primitive::Greater).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 0); // false
}

#[test]
fn test_and() {
    let mut vm = VM::new();
    vm.data_stack.push(0b1100);
    vm.data_stack.push(0b1010);
    vm.execute_primitive(Primitive::And).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 0b1000);
}

#[test]
fn test_or() {
    let mut vm = VM::new();
    vm.data_stack.push(0b1100);
    vm.data_stack.push(0b1010);
    vm.execute_primitive(Primitive::Or).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 0b1110);
}

#[test]
fn test_xor() {
    let mut vm = VM::new();
    vm.data_stack.push(0b1100);
    vm.data_stack.push(0b1010);
    vm.execute_primitive(Primitive::Xor).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 0b0110);
}

#[test]
fn test_invert() {
    let mut vm = VM::new();
    vm.data_stack.push(0);
    vm.execute_primitive(Primitive::Invert).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), -1);
}

#[test]
fn test_return_stack() {
    let mut vm = VM::new();
    vm.data_stack.push(42);
    vm.execute_primitive(Primitive::ToR).unwrap();
    assert_eq!(vm.return_stack.depth(), 1);

    vm.execute_primitive(Primitive::RFetch).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 42);
    assert_eq!(vm.return_stack.depth(), 1);

    vm.execute_primitive(Primitive::FromR).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 42);
    assert_eq!(vm.return_stack.depth(), 0);
}

#[test]
fn test_memory_fetch_store() {
    let mut vm = VM::new();

    // Store 42 at address 100
    vm.data_stack.push(42);
    vm.data_stack.push(100);
    vm.execute_primitive(Primitive::Store).unwrap();

    // Fetch from address 100
    vm.data_stack.push(100);
    vm.execute_primitive(Primitive::Fetch).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 42);
}

#[test]
fn test_memory_c_fetch_c_store() {
    let mut vm = VM::new();

    // Store byte 65 ('A') at address 50
    vm.data_stack.push(65);
    vm.data_stack.push(50);
    vm.execute_primitive(Primitive::CStore).unwrap();

    // Fetch byte from address 50
    vm.data_stack.push(50);
    vm.execute_primitive(Primitive::CFetch).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 65);
}

#[test]
fn test_square_example() {
    // Test the example: : SQUARE DUP * ;
    let mut vm = VM::new();
    vm.data_stack.push(5);
    vm.execute_primitive(Primitive::Dup).unwrap();
    vm.execute_primitive(Primitive::Mul).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 25);
}

#[test]
fn test_complex_expression() {
    // Test: 3 4 + 5 * ( (3+4)*5 = 35 )
    let mut vm = VM::new();
    vm.data_stack.push(3);
    vm.data_stack.push(4);
    vm.execute_primitive(Primitive::Add).unwrap();
    vm.data_stack.push(5);
    vm.execute_primitive(Primitive::Mul).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 35);
}

#[test]
fn test_stack_underflow() {
    let mut vm = VM::new();
    assert_eq!(
        vm.execute_primitive(Primitive::Drop),
        Err(ForthError::StackUnderflow)
    );
}

#[test]
fn test_invalid_memory_address() {
    let mut vm = VM::new();
    vm.data_stack.push(999999); // Way beyond memory size
    assert_eq!(
        vm.execute_primitive(Primitive::Fetch),
        Err(ForthError::InvalidMemoryAddress)
    );
}

#[test]
fn test_dot() {
    let mut vm = VM::new();
    vm.data_stack.push(42);
    vm.data_stack.push(99);

    // Execute . on 99
    vm.execute_primitive(Primitive::Dot).unwrap();

    // 99 should be removed, 42 should remain
    assert_eq!(vm.data_stack.depth(), 1);
    assert_eq!(vm.data_stack.pop().unwrap(), 42);
}

#[test]
fn test_dot_multiple_values() {
    let mut vm = VM::new();
    vm.data_stack.push(1);
    vm.data_stack.push(2);
    vm.data_stack.push(3);

    // Print all three
    vm.execute_primitive(Primitive::Dot).unwrap();
    vm.execute_primitive(Primitive::Dot).unwrap();
    vm.execute_primitive(Primitive::Dot).unwrap();

    // Stack should be empty
    assert_eq!(vm.data_stack.depth(), 0);
}

#[test]
fn test_dot_negative() {
    let mut vm = VM::new();
    vm.data_stack.push(-42);

    vm.execute_primitive(Primitive::Dot).unwrap();

    assert_eq!(vm.data_stack.depth(), 0);
}

#[test]
fn test_dot_underflow() {
    let mut vm = VM::new();

    // Stack is empty, should fail
    assert_eq!(
        vm.execute_primitive(Primitive::Dot),
        Err(ForthError::StackUnderflow)
    );
}

#[test]
fn test_i_basic() {
    let mut vm = VM::new();

    // Simulate being inside a loop by pushing a loop index
    vm.loop_stack.push(5);

    // I should push the loop index to data stack
    vm.execute_primitive(Primitive::I).unwrap();

    assert_eq!(vm.data_stack.depth(), 1);
    assert_eq!(vm.data_stack.pop().unwrap(), 5);
    // Loop stack should remain unchanged
    assert_eq!(vm.loop_stack.depth(), 1);
}

#[test]
fn test_i_nested_loops() {
    let mut vm = VM::new();

    // Simulate nested loops
    vm.loop_stack.push(10); // outer loop index
    vm.loop_stack.push(3);  // inner loop index

    // I should return the innermost (top) loop index
    vm.execute_primitive(Primitive::I).unwrap();

    assert_eq!(vm.data_stack.depth(), 1);
    assert_eq!(vm.data_stack.pop().unwrap(), 3);
    assert_eq!(vm.loop_stack.depth(), 2);
}

#[test]
fn test_i_no_loop() {
    let mut vm = VM::new();

    // No loop running, should fail
    assert_eq!(
        vm.execute_primitive(Primitive::I),
        Err(ForthError::StackUnderflow)
    );
}

#[test]
fn test_i_multiple_calls() {
    let mut vm = VM::new();

    vm.loop_stack.push(7);

    // Multiple calls to I should all return the same value
    vm.execute_primitive(Primitive::I).unwrap();
    vm.execute_primitive(Primitive::I).unwrap();
    vm.execute_primitive(Primitive::I).unwrap();

    assert_eq!(vm.data_stack.depth(), 3);
    assert_eq!(vm.data_stack.pop().unwrap(), 7);
    assert_eq!(vm.data_stack.pop().unwrap(), 7);
    assert_eq!(vm.data_stack.pop().unwrap(), 7);
}

#[test]
fn test_depth_empty() {
    let mut vm = VM::new();

    // Empty stack should have depth 0
    vm.execute_primitive(Primitive::Depth).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 0);
}

#[test]
fn test_depth_with_items() {
    let mut vm = VM::new();

    // Push some values
    vm.data_stack.push(10);
    vm.data_stack.push(20);
    vm.data_stack.push(30);

    // DEPTH should return 3
    vm.execute_primitive(Primitive::Depth).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 3);

    // Stack should still have original 3 items
    assert_eq!(vm.data_stack.depth(), 3);
}

#[test]
fn test_depth_incremental() {
    let mut vm = VM::new();

    // Check depth at each step
    vm.execute_primitive(Primitive::Depth).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 0);

    vm.data_stack.push(1);
    vm.execute_primitive(Primitive::Depth).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 1);

    vm.data_stack.push(2);
    vm.data_stack.push(3);
    vm.execute_primitive(Primitive::Depth).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 3);
}

#[test]
fn test_char_plus_basic() {
    let mut vm = VM::new();
    vm.data_stack.push(1000);
    vm.execute_primitive(Primitive::CharPlus).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 1001);
}

#[test]
fn test_char_plus_zero() {
    let mut vm = VM::new();
    vm.data_stack.push(0);
    vm.execute_primitive(Primitive::CharPlus).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 1);
}

#[test]
fn test_char_plus_multiple() {
    let mut vm = VM::new();
    vm.data_stack.push(100);
    vm.execute_primitive(Primitive::CharPlus).unwrap();
    vm.execute_primitive(Primitive::CharPlus).unwrap();
    vm.execute_primitive(Primitive::CharPlus).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 103);
}

#[test]
fn test_char_plus_negative() {
    let mut vm = VM::new();
    vm.data_stack.push(-5);
    vm.execute_primitive(Primitive::CharPlus).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), -4);
}

#[test]
fn test_char_plus_underflow() {
    let mut vm = VM::new();
    let result = vm.execute_primitive(Primitive::CharPlus);
    assert!(result.is_err());
}

#[test]
fn test_c_store_basic() {
    let mut vm = VM::new();
    vm.data_stack.push(42);
    vm.data_stack.push(100);
    vm.execute_primitive(Primitive::CStore).unwrap();
    assert_eq!(vm.memory[100], 42);
}

#[test]
fn test_c_store_truncation() {
    let mut vm = VM::new();
    // Store 0x1234 - should only store 0x34 (low byte)
    vm.data_stack.push(0x1234);
    vm.data_stack.push(200);
    vm.execute_primitive(Primitive::CStore).unwrap();
    assert_eq!(vm.memory[200], 0x34);
}

#[test]
fn test_c_store_negative() {
    let mut vm = VM::new();
    // -1 as i64 is 0xFFFFFFFFFFFFFFFF, should store 0xFF
    vm.data_stack.push(-1);
    vm.data_stack.push(300);
    vm.execute_primitive(Primitive::CStore).unwrap();
    assert_eq!(vm.memory[300], 0xFF);
}

#[test]
fn test_c_store_zero() {
    let mut vm = VM::new();
    // First set a non-zero value
    vm.memory[400] = 99;
    // Now store zero
    vm.data_stack.push(0);
    vm.data_stack.push(400);
    vm.execute_primitive(Primitive::CStore).unwrap();
    assert_eq!(vm.memory[400], 0);
}

#[test]
fn test_c_store_multiple_locations() {
    let mut vm = VM::new();
    // Store different values at different addresses
    vm.data_stack.push(65); // 'A'
    vm.data_stack.push(500);
    vm.execute_primitive(Primitive::CStore).unwrap();

    vm.data_stack.push(66); // 'B'
    vm.data_stack.push(501);
    vm.execute_primitive(Primitive::CStore).unwrap();

    vm.data_stack.push(67); // 'C'
    vm.data_stack.push(502);
    vm.execute_primitive(Primitive::CStore).unwrap();

    assert_eq!(vm.memory[500], 65);
    assert_eq!(vm.memory[501], 66);
    assert_eq!(vm.memory[502], 67);
}

#[test]
fn test_c_store_invalid_address() {
    let mut vm = VM::new();
    vm.data_stack.push(42);
    vm.data_stack.push(999999); // Out of bounds
    let result = vm.execute_primitive(Primitive::CStore);
    assert!(result.is_err());
}

#[test]
fn test_c_store_underflow() {
    let mut vm = VM::new();
    vm.data_stack.push(100); // Only one value
    let result = vm.execute_primitive(Primitive::CStore);
    assert!(result.is_err());
}

#[test]
fn test_chars_identity() {
    let mut vm = VM::new();
    // CHARS should return same value (1 char = 1 address unit)
    vm.data_stack.push(5);
    vm.execute_primitive(Primitive::Chars).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 5);
}

#[test]
fn test_chars_zero() {
    let mut vm = VM::new();
    vm.data_stack.push(0);
    vm.execute_primitive(Primitive::Chars).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 0);
}

#[test]
fn test_chars_one() {
    let mut vm = VM::new();
    vm.data_stack.push(1);
    vm.execute_primitive(Primitive::Chars).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 1);
}

#[test]
fn test_chars_large() {
    let mut vm = VM::new();
    vm.data_stack.push(1000);
    vm.execute_primitive(Primitive::Chars).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), 1000);
}

#[test]
fn test_chars_negative() {
    let mut vm = VM::new();
    vm.data_stack.push(-10);
    vm.execute_primitive(Primitive::Chars).unwrap();
    assert_eq!(vm.data_stack.pop().unwrap(), -10);
}

#[test]
fn test_chars_underflow() {
    let mut vm = VM::new();
    let result = vm.execute_primitive(Primitive::Chars);
    assert!(result.is_err());
}

#[test]
fn test_lookup_primitive_fetch() {
    let mut vm = VM::new();

    // Store "@" in memory
    let word = "@";
    let addr = 0x1000;
    vm.memory[addr] = b'@';

    // Push addr and len
    vm.data_stack.push(addr as i64);
    vm.data_stack.push(1);

    // Execute LOOKUP-PRIMITIVE
    vm.execute_primitive(Primitive::LookupPrimitive).unwrap();

    // Should return 0 (Fetch is the first primitive)
    let prim_id = vm.data_stack.pop().unwrap();
    assert_eq!(prim_id, 0);
}

#[test]
fn test_lookup_primitive_dup() {
    let mut vm = VM::new();

    // Store "DUP" in memory
    let word = "DUP";
    let addr = 0x1000;
    for (i, &byte) in word.as_bytes().iter().enumerate() {
        vm.memory[addr + i] = byte;
    }

    // Push addr and len
    vm.data_stack.push(addr as i64);
    vm.data_stack.push(word.len() as i64);

    // Execute LOOKUP-PRIMITIVE
    vm.execute_primitive(Primitive::LookupPrimitive).unwrap();

    // DUP should be at index 15 (count from @ at 0)
    let prim_id = vm.data_stack.pop().unwrap();
    assert_eq!(prim_id, 15);

    // Verify by checking the primitive at that index
    let primitives = Primitive::all();
    assert_eq!(primitives[15].0, "DUP");
}

#[test]
fn test_lookup_primitive_add() {
    let mut vm = VM::new();

    // Store "+" in memory
    let addr = 0x1000;
    vm.memory[addr] = b'+';

    // Push addr and len
    vm.data_stack.push(addr as i64);
    vm.data_stack.push(1);

    // Execute LOOKUP-PRIMITIVE
    vm.execute_primitive(Primitive::LookupPrimitive).unwrap();

    // + should be at index 23 (Add primitive)
    let prim_id = vm.data_stack.pop().unwrap();
    assert_eq!(prim_id, 23);

    // Verify
    let primitives = Primitive::all();
    assert_eq!(primitives[23].0, "+");
}

#[test]
fn test_lookup_primitive_case_insensitive() {
    let mut vm = VM::new();

    // Store "dup" (lowercase) in memory
    let word = "dup";
    let addr = 0x1000;
    for (i, &byte) in word.as_bytes().iter().enumerate() {
        vm.memory[addr + i] = byte;
    }

    // Push addr and len
    vm.data_stack.push(addr as i64);
    vm.data_stack.push(word.len() as i64);

    // Execute LOOKUP-PRIMITIVE
    vm.execute_primitive(Primitive::LookupPrimitive).unwrap();

    // Should still find DUP (case-insensitive)
    let prim_id = vm.data_stack.pop().unwrap();
    assert_eq!(prim_id, 15);
}

#[test]
fn test_lookup_primitive_not_found() {
    let mut vm = VM::new();

    // Store "NOTAPRIMITIVE" in memory
    let word = "NOTAPRIMITIVE";
    let addr = 0x1000;
    for (i, &byte) in word.as_bytes().iter().enumerate() {
        vm.memory[addr + i] = byte;
    }

    // Push addr and len
    vm.data_stack.push(addr as i64);
    vm.data_stack.push(word.len() as i64);

    // Execute LOOKUP-PRIMITIVE
    vm.execute_primitive(Primitive::LookupPrimitive).unwrap();

    // Should return -1 (not found)
    let prim_id = vm.data_stack.pop().unwrap();
    assert_eq!(prim_id, -1);
}

#[test]
fn test_lookup_primitive_empty_string() {
    let mut vm = VM::new();

    // Push addr and len=0
    vm.data_stack.push(0x1000);
    vm.data_stack.push(0);

    // Execute LOOKUP-PRIMITIVE
    vm.execute_primitive(Primitive::LookupPrimitive).unwrap();

    // Should return -1 (not found)
    let prim_id = vm.data_stack.pop().unwrap();
    assert_eq!(prim_id, -1);
}

#[test]
fn test_lookup_primitive_underflow() {
    let mut vm = VM::new();

    // Stack underflow - only push one value
    vm.data_stack.push(0x1000);

    // Should return error
    let result = vm.execute_primitive(Primitive::LookupPrimitive);
    assert!(result.is_err());
}
