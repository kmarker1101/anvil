// main_bytecode.rs - Forth REPL using self-hosting metacompiler

use anvil::bootstrap::BootstrapCompiler;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use std::env;
use std::fs;

fn main() -> Result<()> {
    println!("Anvil Forth v0.4.0 (Self-Hosting)");
    println!("Type .help for help, bye to exit");
    println!();

    let mut compiler = BootstrapCompiler::new();

    // Load metacompiler first (defines control structures)
    let metacompiler = include_str!("metacompiler.fth");
    if let Err(e) = compiler.process_line(metacompiler) {
        eprintln!("Error loading metacompiler: {}", e);
        return Ok(());
    }

    // Load stdlib (uses control structures from metacompiler)
    let stdlib = include_str!("stdlib.fth");
    if let Err(e) = compiler.process_line(stdlib) {
        eprintln!("Error loading stdlib: {}", e);
        return Ok(());
    }

    // Load files from command line arguments
    let args: Vec<String> = env::args().collect();
    for file_path in args.iter().skip(1) {
        match load_file(&mut compiler, file_path) {
            Ok(()) => {
                println!("Loaded: {}", file_path);
                // Check if file contains BYE - if so, exit immediately
                if let Ok(contents) = fs::read_to_string(file_path) {
                    let has_bye = contents.lines()
                        .any(|line| line.trim().to_uppercase() == "BYE");
                    if has_bye {
                        return Ok(());
                    }
                }
            }
            Err(e) => {
                if e == "EXIT" {
                    return Ok(());
                }
                eprintln!("Error loading {}: {}", file_path, e)
            },
        }
    }

    // Create readline editor with history
    let mut rl = DefaultEditor::new()?;
    let history_file = dirs::home_dir()
        .map(|mut p| {
            p.push(".anvil_history");
            p
        });

    // Load history if file exists
    if let Some(ref path) = history_file {
        let _ = rl.load_history(path);
    }

    let mut in_definition = false;

    loop {
        // Read line with history support
        let prompt = if in_definition { "... " } else { "" };
        let readline = rl.readline(prompt);

        match readline {
            Ok(line) => {
                let input = line.trim();

                // Handle empty input
                if input.is_empty() {
                    continue;
                }

                // Add to history
                let _ = rl.add_history_entry(input);

                // Move cursor back to the end of the input line (gforth style)
                print!("\x1b[A\x1b[{}C ", input.len());

                // Handle REPL commands (but not the Forth "." word or ." string literal)
                if input.starts_with('.') && input != "." && !input.starts_with(".\"") {
                    let cmd = input.to_lowercase();
                    match cmd.as_str() {
                        ".quit" | ".exit" | ".q" => {
                            println!();
                            break;
                        }
                        ".help" | ".h" => {
                            println!();
                            print_help();
                            continue;
                        }
                        ".words" | ".w" => {
                            println!();
                            print_words(&compiler);
                            continue;
                        }
                        ".stack" | ".s" => {
                            print_stack(&compiler);
                            println!(" ok");
                            continue;
                        }
                        ".clear" => {
                            compiler.interpreter.vm.data_stack.clear();
                            println!(" ok");
                            continue;
                        }
                        _ => {
                            println!();
                            println!("Unknown command: {}", input);
                            println!("Type .help for help");
                            continue;
                        }
                    }
                }

                // Check for BYE word (Forth standard exit)
                if input.to_uppercase() == "BYE" {
                    println!();
                    break;
                }

                // Track if we're in a definition
                if input.contains(':') && !input.contains(';') {
                    in_definition = true;
                }
                if input.contains(';') {
                    in_definition = false;
                }

                // Check for INCLUDE word (load a file)
                let upper_input = input.to_uppercase();
                if upper_input.starts_with("INCLUDE ") {
                    let file_path = input[8..].trim();
                    match load_file(&mut compiler, file_path) {
                        Ok(()) => {
                            println!(" ok");
                            // Check if file contains BYE - if so, exit REPL
                            if let Ok(contents) = std::fs::read_to_string(file_path) {
                                let has_bye = contents.lines()
                                    .any(|line| line.trim().to_uppercase() == "BYE");
                                if has_bye {
                                    break;
                                }
                            }
                        }
                        Err(e) => {
                            if e == "EXIT" {
                                println!();
                                break;
                            }
                            println!(" Error loading {}: {}", file_path, e)
                        },
                    }
                    continue;
                }

                // Process Forth code
                match compiler.process_line(input) {
                    Ok(()) => {
                        println!(" ok");
                    }
                    Err(e) => {
                        if e.contains("BYE") {
                            println!();
                            break;
                        }
                        println!(" {}", e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!();
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    // Save history
    if let Some(ref path) = history_file {
        let _ = rl.save_history(path);
    }

    Ok(())
}

fn load_file(compiler: &mut BootstrapCompiler, file_path: &str) -> std::result::Result<(), String> {
    let contents = fs::read_to_string(file_path)
        .map_err(|e| format!("Failed to read file: {}", e))?;

    let processed = preprocess_includes(&contents, file_path)?;
    compiler.process_line(&processed)?;

    Ok(())
}

fn preprocess_includes(contents: &str, base_path: &str) -> std::result::Result<String, String> {
    let mut result = String::new();

    for line in contents.lines() {
        let trimmed = line.trim();
        let upper_trimmed = trimmed.to_uppercase();

        if upper_trimmed.starts_with("INCLUDE ") {
            let include_path = trimmed[8..].trim();

            let full_path = if std::path::Path::new(include_path).is_absolute()
                || std::path::Path::new(include_path).exists() {
                include_path.to_string()
            } else {
                let base_dir = std::path::Path::new(base_path)
                    .parent()
                    .unwrap_or(std::path::Path::new("."));
                let relative_path = base_dir.join(include_path);
                if relative_path.exists() {
                    relative_path.to_string_lossy().to_string()
                } else {
                    include_path.to_string()
                }
            };

            let included_contents = fs::read_to_string(&full_path)
                .map_err(|e| format!("Failed to include {}: {}", full_path, e))?;
            let processed_included = preprocess_includes(&included_contents, &full_path)?;
            result.push_str(&processed_included);
            result.push('\n');
        } else {
            result.push_str(line);
            result.push('\n');
        }
    }

    Ok(result)
}

fn print_help() {
    println!("Anvil Forth REPL Commands:");
    println!("  .help, .h      Show this help");
    println!("  .words, .w     List all defined words");
    println!("  .stack, .s     Show the data stack");
    println!("  .clear         Clear the data stack");
    println!("  bye            Exit the REPL");
    println!();
    println!("Forth Syntax:");
    println!("  Numbers:       123, -456");
    println!("  Words:         DUP, SWAP, +, -, *, /, .");
    println!("  Define word:   : SQUARE DUP * ;");
    println!("  Conditionals:  IF ... THEN, IF ... ELSE ... THEN");
    println!("  Loops:         BEGIN ... UNTIL, BEGIN ... WHILE ... REPEAT");
    println!("  Load file:     INCLUDE filename.fth");
    println!();
    println!("Examples:");
    println!("  5 DUP *           ( 5 squared = 25 )");
    println!("  : SQUARE DUP * ;  ( define SQUARE word )");
    println!("  7 SQUARE          ( use it: 7 * 7 = 49 )");
    println!("  3 4 5 . . .       ( prints: 5 4 3 )");
    println!("  INCLUDE mylib.fth ( load definitions from file )");
}

fn print_words(compiler: &BootstrapCompiler) {
    use anvil::primitives::LATEST_ADDR;

    let mut words = Vec::new();

    // Walk the memory-based dictionary
    let mut latest_bytes = [0u8; 8];
    latest_bytes.copy_from_slice(&compiler.interpreter.vm.memory[LATEST_ADDR..LATEST_ADDR + 8]);
    let mut current = i64::from_le_bytes(latest_bytes) as usize;

    while current != 0 {
        // Skip LINK (8) and FLAGS (8) = 16 bytes
        let name_len_addr = current + 16;
        let name_len = compiler.interpreter.vm.memory[name_len_addr] as usize;
        let name_start = name_len_addr + 1;

        if name_len > 0 && name_len < 64 { // Sanity check
            let name_bytes = &compiler.interpreter.vm.memory[name_start..name_start + name_len];
            if let Ok(name) = std::str::from_utf8(name_bytes) {
                words.push(name.to_string());
            }
        }

        // Get LINK to previous word
        let mut link_bytes = [0u8; 8];
        link_bytes.copy_from_slice(&compiler.interpreter.vm.memory[current..current + 8]);
        current = i64::from_le_bytes(link_bytes) as usize;
    }

    println!("Defined words ({}):", words.len());
    for (i, word) in words.iter().enumerate() {
        print!("{:12}", word);
        if (i + 1) % 6 == 0 {
            println!();
        }
    }
    if !words.len().is_multiple_of(6) {
        println!();
    }
}

fn print_stack(compiler: &BootstrapCompiler) {
    let depth = compiler.interpreter.vm.data_stack.depth();
    print!("<{}> ", depth);
    for val in compiler.interpreter.vm.data_stack.iter() {
        print!("{} ", val);
    }
}
