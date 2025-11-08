// aot.rs - Ahead-of-Time compiler for Forth
//
// Compiles Forth source files to native object files or executables

use forth::compiler::Compiler;
use forth::lexer::Lexer;
use forth::parser::{Parser, Program, Definition, Expression};
use inkwell::context::Context;
use inkwell::OptimizationLevel;
use std::env;
use std::fs;
use std::process;
use std::collections::HashSet;

fn print_usage() {
    eprintln!("Anvil Forth AOT Compiler v0.1.0");
    eprintln!();
    eprintln!("Usage:");
    eprintln!("  anvilc <input.fth> -o <output>       # Compile to executable");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -o <file>    Output executable");
    eprintln!("  -O<level>    Optimization level (0-3, default: 3)");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }

    let mut input_file = None;
    let mut output_file = None;
    let mut _opt_level = OptimizationLevel::Aggressive;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                i += 1;
                if i < args.len() {
                    output_file = Some(args[i].clone());
                } else {
                    eprintln!("Error: -o requires an argument");
                    process::exit(1);
                }
            }
            arg if arg.starts_with("-O") => {
                let level = &arg[2..];
                _opt_level = match level {
                    "0" => OptimizationLevel::None,
                    "1" => OptimizationLevel::Less,
                    "2" => OptimizationLevel::Default,
                    "3" => OptimizationLevel::Aggressive,
                    _ => {
                        eprintln!("Error: Invalid optimization level: {}", level);
                        process::exit(1);
                    }
                };
            }
            arg if !arg.starts_with('-') => {
                if input_file.is_none() {
                    input_file = Some(arg.to_string());
                } else {
                    eprintln!("Error: Multiple input files specified");
                    process::exit(1);
                }
            }
            _ => {
                eprintln!("Error: Unknown option: {}", args[i]);
                print_usage();
                process::exit(1);
            }
        }
        i += 1;
    }

    let input_file = match input_file {
        Some(f) => f,
        None => {
            eprintln!("Error: No input file specified");
            print_usage();
            process::exit(1);
        }
    };

    // Read input file
    let source = match fs::read_to_string(&input_file) {
        Ok(s) => s.to_uppercase(),  // Uppercase for case-insensitive matching
        Err(e) => {
            eprintln!("Error reading {}: {}", input_file, e);
            process::exit(1);
        }
    };

    // Parse stdlib to have available
    const STDLIB: &str = include_str!("stdlib.fth");
    let stdlib_upper = STDLIB.to_uppercase();
    let mut stdlib_lexer = Lexer::new(&stdlib_upper);
    let stdlib_tokens = match stdlib_lexer.tokenize() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Stdlib lexer error: {}", e);
            process::exit(1);
        }
    };
    let mut stdlib_parser = Parser::new(stdlib_tokens);
    let stdlib_program = match stdlib_parser.parse() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Stdlib parser error: {}", e);
            process::exit(1);
        }
    };

    // Parse user's Forth source
    let mut lexer = Lexer::new(&source);
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Lexer error: {}", e);
            process::exit(1);
        }
    };

    let mut parser = Parser::new(tokens);
    let user_program = match parser.parse() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parser error: {}", e);
            process::exit(1);
        }
    };

    // Analyze dependencies and build minimal program
    let minimal_program = build_minimal_program(user_program, stdlib_program);

    // Compile the minimal program
    let mut compiler = Compiler::default();
    if let Err(e) = compiler.compile_program(minimal_program) {
        eprintln!("Compiler error: {}", e);
        process::exit(1);
    }

    // Generate LLVM module
    let context = Box::leak(Box::new(Context::create()));
    let mut llvm_compiler = match forth::llvm_jit::LLVMCompiler::new_aot(
        context,
        &input_file.replace(".fth", ""),
        None,
    ) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("LLVM compiler error: {}", e);
            process::exit(1);
        }
    };

    // Compile all words to LLVM
    for (name, word) in compiler.compiled_words() {
        if let Err(e) = llvm_compiler.compile_word(name, &word.instructions) {
            eprintln!("Error compiling word {}: {}", name, e);
            process::exit(1);
        }
    }

    // Check that output was specified
    let output_binary = match output_file {
        Some(f) => f,
        None => {
            eprintln!("Error: No output specified (use -o)");
            process::exit(1);
        }
    };

    // Generate temporary files
    let temp_object = format!("{}.tmp.o", output_binary);
    let temp_wrapper = format!("{}.tmp.c", output_binary);

    // Write object file
    if let Err(e) = llvm_compiler.emit_object_file(&temp_object) {
        eprintln!("Error writing object file: {}", e);
        process::exit(1);
    }

    // Generate C wrapper
    generate_c_wrapper(&compiler, &temp_wrapper);

    // Get the library path (same directory as anvilc executable)
    let exe_path = env::current_exe().unwrap_or_else(|_| "anvilc".into());
    let lib_dir = exe_path.parent().unwrap_or_else(|| std::path::Path::new("."));

    // Link with clang
    let link_status = process::Command::new("clang")
        .arg(&temp_wrapper)
        .arg(&temp_object)
        .arg(format!("-L{}", lib_dir.display()))
        .arg("-lforth")
        .arg("-Wl,-w")  // Suppress linker warnings
        .arg("-o")
        .arg(&output_binary)
        .status();

    // Clean up temporary files
    let _ = fs::remove_file(&temp_object);
    let _ = fs::remove_file(&temp_wrapper);

    match link_status {
        Ok(status) if status.success() => {
            println!("Compiled {} -> {}", input_file, output_binary);
        }
        Ok(_) => {
            eprintln!("Error: Linking failed");
            process::exit(1);
        }
        Err(e) => {
            eprintln!("Error: Failed to run linker: {}", e);
            process::exit(1);
        }
    }
}

/// Build a minimal program with only needed stdlib definitions
fn build_minimal_program(user_program: Program, stdlib_program: Program) -> Program {
    // Build a map of stdlib definitions by name
    let mut stdlib_defs = std::collections::HashMap::new();
    for def in stdlib_program.definitions {
        let name = match &def {
            Definition::Word { name, .. } => Some(name.clone()),
            Definition::Variable { name } => Some(name.clone()),
            Definition::Expression(_) => None,
        };
        if let Some(n) = name {
            stdlib_defs.insert(n, def);
        }
    }

    // Build a set of words defined by the user (these override stdlib)
    let mut user_defined_words = HashSet::new();
    for def in &user_program.definitions {
        let name = match def {
            Definition::Word { name, .. } => Some(name.clone()),
            Definition::Variable { name } => Some(name.clone()),
            Definition::Expression(_) => None,
        };
        if let Some(n) = name {
            user_defined_words.insert(n);
        }
    }

    // Collect all words referenced in user program
    let mut needed_words = HashSet::new();
    collect_word_references(&user_program, &mut needed_words);

    // Transitively collect dependencies from stdlib
    // But don't include stdlib words that the user is redefining
    let mut added = true;
    while added {
        added = false;
        let current_words: Vec<String> = needed_words.iter().cloned().collect();
        for word in current_words {
            // Skip if user is redefining this word
            if user_defined_words.contains(&word) {
                continue;
            }

            if let Some(def) = stdlib_defs.get(&word) {
                let mut deps = HashSet::new();
                collect_word_references_from_def(def, &mut deps);
                for dep in deps {
                    if needed_words.insert(dep.clone()) {
                        added = true;
                    }
                }
            }
        }
    }

    // Build final program with stdlib deps first, then user code
    // Don't include stdlib words that user is redefining
    let mut final_defs = Vec::new();
    for word in &needed_words {
        if !user_defined_words.contains(word) {
            if let Some(def) = stdlib_defs.remove(word) {
                final_defs.push(def);
            }
        }
    }
    final_defs.extend(user_program.definitions);

    Program {
        definitions: final_defs,
    }
}

fn collect_word_references(program: &Program, words: &mut HashSet<String>) {
    for def in &program.definitions {
        collect_word_references_from_def(def, words);
    }
}

fn collect_word_references_from_def(def: &Definition, words: &mut HashSet<String>) {
    match def {
        Definition::Word { body, .. } => {
            for expr in body {
                collect_word_references_from_expr(expr, words);
            }
        }
        Definition::Expression(expr) => {
            collect_word_references_from_expr(expr, words);
        }
        Definition::Variable { .. } => {}
    }
}

fn collect_word_references_from_expr(expr: &Expression, words: &mut HashSet<String>) {
    match expr {
        Expression::WordCall(name) => {
            words.insert(name.clone());
        }
        Expression::If { condition, then_branch } => {
            for e in condition {
                collect_word_references_from_expr(e, words);
            }
            for e in then_branch {
                collect_word_references_from_expr(e, words);
            }
        }
        Expression::IfElse { condition, then_branch, else_branch } => {
            for e in condition {
                collect_word_references_from_expr(e, words);
            }
            for e in then_branch {
                collect_word_references_from_expr(e, words);
            }
            for e in else_branch {
                collect_word_references_from_expr(e, words);
            }
        }
        Expression::DoLoop { body } | Expression::DoPlusLoop { body } |
        Expression::QDoLoop { body } | Expression::QDoPlusLoop { body } |
        Expression::BeginUntil { body } => {
            for e in body {
                collect_word_references_from_expr(e, words);
            }
        }
        Expression::BeginWhileRepeat { condition, body } => {
            for e in condition {
                collect_word_references_from_expr(e, words);
            }
            for e in body {
                collect_word_references_from_expr(e, words);
            }
        }
        Expression::Number(_) | Expression::String(_) | Expression::DotQuote(_) |
        Expression::Recurse | Expression::Exit | Expression::Bye => {}
    }
}

fn generate_c_wrapper(compiler: &Compiler, output_path: &str) {
    let mut c_code = String::new();

    c_code.push_str("// Auto-generated C wrapper for Forth compiled code\n\n");
    c_code.push_str("#include <stdint.h>\n");
    c_code.push_str("#include <stdio.h>\n");
    c_code.push_str("#include <stdbool.h>\n\n");

    c_code.push_str("// Forth function signature\n");
    c_code.push_str("typedef void (*ForthFunc)(int64_t* data_stack, size_t* data_len,\n");
    c_code.push_str("                          int64_t* return_stack, size_t* return_len,\n");
    c_code.push_str("                          int64_t* loop_stack, size_t* loop_len,\n");
    c_code.push_str("                          uint8_t* memory, size_t* here, bool* exit_flag);\n\n");

    // Only declare MAIN if it exists (user-defined words with alphanumeric names)
    if compiler.get_word("MAIN").is_some() {
        c_code.push_str("extern void MAIN(int64_t*, size_t*, int64_t*, size_t*, int64_t*, size_t*, uint8_t*, size_t*, bool*);\n\n");
    }

    c_code.push_str("int main() {\n");
    c_code.push_str("    int64_t data_stack[256] = {0};\n");
    c_code.push_str("    size_t data_len = 0;\n");
    c_code.push_str("    int64_t return_stack[256] = {0};\n");
    c_code.push_str("    size_t return_len = 0;\n");
    c_code.push_str("    int64_t loop_stack[256] = {0};\n");
    c_code.push_str("    size_t loop_len = 0;\n");
    c_code.push_str("    uint8_t memory[65536] = {0};\n");
    c_code.push_str("    size_t here = 0;\n");
    c_code.push_str("    bool exit_flag = false;\n\n");

    c_code.push_str("    // Call MAIN Forth word if it exists\n");
    if compiler.get_word("MAIN").is_some() {
        c_code.push_str("    MAIN(data_stack, &data_len, return_stack, &return_len,\n");
        c_code.push_str("         loop_stack, &loop_len, memory, &here, &exit_flag);\n");
    } else {
        c_code.push_str("    // No MAIN word defined\n");
    }

    c_code.push_str("    return 0;\n");
    c_code.push_str("}\n");

    if let Err(e) = fs::write(output_path, c_code) {
        eprintln!("Error writing C wrapper: {}", e);
        process::exit(1);
    }
}
