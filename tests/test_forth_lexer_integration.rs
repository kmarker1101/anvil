// test_forth_lexer_integration.rs - Integration test for Forth lexer

use std::process::Command;

#[test]
fn test_forth_lexer() {
    // Run the Forth lexer tests
    let output = Command::new("cargo")
        .args(&["run", "--bin", "anvil", "--", "tests/lexer-forth-tests.fth"])
        .output()
        .expect("Failed to run Forth lexer tests");

    // Check that the command succeeded
    assert!(
        output.status.success(),
        "Forth lexer tests exited with error: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Convert output to string
    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify all tests ran
    assert!(
        stdout.contains("ALL TESTS COMPLETE"),
        "Test suite did not complete. Output:\n{}",
        stdout
    );

    // Verify each test number appears (Tests 1-20)
    // Note: input gets uppercased, so "Test" becomes "TEST"
    for i in 1..=20 {
        assert!(
            stdout.contains(&format!("TEST {}", i)),
            "Test {} did not run",
            i
        );
    }

    // Check that no error messages appeared
    assert!(
        !stdout.contains("Error"),
        "Tests contained errors:\n{}",
        stdout
    );
}
