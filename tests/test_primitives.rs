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
