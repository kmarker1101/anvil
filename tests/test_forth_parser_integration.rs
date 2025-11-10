// test_forth_parser_integration.rs - Integration test for Forth parser

use std::process::Command;

#[test]
fn test_forth_parser() {
    // Run the Forth parser tests
    let output = Command::new("cargo")
        .args(&["run", "--bin", "anvil", "--", "tests/parser-forth-tests.fth"])
        .output()
        .expect("Failed to run Forth parser tests");

    // Check that the command succeeded
    assert!(
        output.status.success(),
        "Forth parser tests exited with error: {}",
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

    // Verify test numbers appear (we have 23 tests)
    for i in 0..23 {
        assert!(
            stdout.contains(&format!("TEST {}", i)),
            "Test {} did not run. Output:\n{}",
            i,
            stdout
        );
    }

    // Verify all tests passed
    let passed_count = stdout.matches("PASSED").count();
    assert_eq!(
        passed_count, 23,
        "Expected 23 tests to pass, but {} passed",
        passed_count
    );

    // Check that no error messages appeared
    assert!(
        !stdout.contains("ASSERTION FAILED"),
        "Tests contained assertion failures:\n{}",
        stdout
    );

    assert!(
        !stdout.contains("ABORT"),
        "Tests aborted:\n{}",
        stdout
    );
}
