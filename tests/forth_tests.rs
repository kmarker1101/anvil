// Integration test that runs the Forth test suite
use std::process::Command;

#[test]
fn run_forth_test_suite() {
    // Build the binary first
    let status = Command::new("cargo")
        .args(&["build", "--release", "--bin", "forth"])
        .status()
        .expect("Failed to build forth binary");

    assert!(status.success(), "Failed to build forth binary");

    // Run the forth-test.sh script
    let output = Command::new("./forth-test.sh")
        .output()
        .expect("Failed to execute forth-test.sh");

    // Print the full output
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    println!("\n{}", "=".repeat(70));
    println!("FORTH TEST SUITE OUTPUT:");
    println!("{}", "=".repeat(70));
    print!("{}", stdout);
    if !stderr.is_empty() {
        println!("\nSTDERR:\n{}", stderr);
    }
    println!("{}", "=".repeat(70));

    // Check that it exited successfully
    assert!(
        output.status.success(),
        "Forth tests failed with exit code: {:?}",
        output.status.code()
    );

    // Verify the output contains success message
    assert!(
        stdout.contains("All modes tested successfully"),
        "Expected success message in output"
    );
}
