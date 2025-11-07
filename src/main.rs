// main.rs - Forth REPL (Read-Eval-Print Loop)

use forth::compiler::Executor;
use forth::lexer::Lexer;
use forth::parser::Parser;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use std::env;
use std::fs;

fn main() -> Result<()> {
    println!("Anvil Forth v0.1.0");
    println!("Type .help for help, bye to exit");
    println!();

    let mut executor = match Executor::with_stdlib() {
        Ok(exec) => exec,
        Err(e) => {
            eprintln!("Error loading standard library: {}", e);
            eprintln!("Starting without stdlib...");
            Executor::new()
        }
    };

    // Load files from command line arguments
    let args: Vec<String> = env::args().collect();
    for file_path in args.iter().skip(1) {
        match load_file(&mut executor, file_path) {
            Ok(()) => println!("Loaded: {}", file_path),
            Err(e) => eprintln!("Error loading {}: {}", file_path, e),
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
                // Rustyline has moved to a new line, so we go back up
                print!("\x1b[A\x1b[{}C ", input.len());

                // Handle REPL commands (but not the Forth "." word)
                if input.starts_with('.') && input != "." {
                    let cmd = input.to_lowercase();
                    match cmd.as_str() {
                        ".quit" | ".exit" | ".q" => {
                            println!();
                            println!("Goodbye!");
                            break;
                        }
                        ".help" | ".h" => {
                            println!();
                            print_help();
                            continue;
                        }
                        ".words" | ".w" => {
                            println!();
                            print_words(&executor);
                            continue;
                        }
                        ".stack" | ".s" => {
                            print_stack(&executor);
                            println!(" ok");
                            continue;
                        }
                        ".clear" => {
                            executor.vm_mut().data_stack.clear();
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

                // Track if we're in a definition
                if input.contains(':') && !input.contains(';') {
                    in_definition = true;
                }
                if input.contains(';') {
                    in_definition = false;
                }

                // Check for BYE word (Forth standard exit)
                if input.to_uppercase() == "BYE" {
                    println!(" Goodbye!");
                    break;
                }

                // Check for INCLUDE word (load a file)
                let upper_input = input.to_uppercase();
                if upper_input.starts_with("INCLUDE ") {
                    let file_path = input[8..].trim();
                    match load_file(&mut executor, file_path) {
                        Ok(()) => println!(" ok"),
                        Err(e) => println!(" Error loading {}: {}", file_path, e),
                    }
                    continue;
                }

                // Process Forth code
                match process_input(&mut executor, input) {
                    Ok(()) => {
                        // Print " ok" on same line (gforth style)
                        println!(" ok");
                    }
                    Err(e) => {
                        println!(" {}", e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Goodbye!");
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

fn load_file(executor: &mut Executor, file_path: &str) -> std::result::Result<(), String> {
    // Read file contents
    let contents = fs::read_to_string(file_path)
        .map_err(|e| format!("Failed to read file: {}", e))?;

    // Convert to uppercase for case-insensitive word matching (same as REPL input)
    let contents = contents.to_uppercase();

    // Process the file
    let mut lexer = Lexer::new(&contents);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;

    let mut parser = Parser::new(tokens);
    let program = parser.parse().map_err(|e| e.to_string())?;

    executor.execute_program(program)?;

    Ok(())
}

fn process_input(executor: &mut Executor, input: &str) -> std::result::Result<(), String> {
    // Convert to uppercase for case-insensitive word matching
    let input = input.to_uppercase();

    // Tokenize
    let mut lexer = Lexer::new(&input);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;

    // Parse
    let mut parser = Parser::new(tokens);
    let program = parser.parse().map_err(|e| e.to_string())?;

    // Compile and execute (handles both definitions and immediate expressions)
    executor.execute_program(program)?;

    Ok(())
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

fn print_words(executor: &Executor) {
    let mut words = executor.compiler().words();
    words.sort();

    println!("Defined words ({}):", words.len());
    for (i, word) in words.iter().enumerate() {
        print!("{:12}", word);
        if (i + 1) % 6 == 0 {
            println!();
        }
    }
    if words.len() % 6 != 0 {
        println!();
    }
}

fn print_stack(executor: &Executor) {
    let depth = executor.vm().data_stack.depth();
    print!("<{}> ", depth);
    // Print stack from bottom to top
    for val in executor.vm().data_stack.iter() {
        print!("{} ", val);
    }
}
