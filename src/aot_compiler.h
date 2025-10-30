#ifndef ANVIL_AOT_COMPILER_H
#define ANVIL_AOT_COMPILER_H

#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/TargetParser/Host.h>
#include <string>
#include <memory>

namespace anvil {

// Initialize LLVM for AOT compilation
inline void initialize_aot() {
    static bool initialized = false;
    if (!initialized) {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();
        initialized = true;
    }
}

// Compile LLVM module to object file
inline bool compile_to_object(llvm::Module* module, const std::string& output_path, std::string* error_msg = nullptr) {
    // Get target triple
    llvm::Triple target_triple(llvm::sys::getDefaultTargetTriple());
    module->setTargetTriple(target_triple);

    // Look up target
    std::string error;
    const llvm::Target* target = llvm::TargetRegistry::lookupTarget(target_triple.str(), error);
    if (!target) {
        if (error_msg) *error_msg = "Failed to lookup target: " + error;
        return false;
    }

    // Configure target machine for minimal size
    llvm::TargetOptions opt;
    llvm::TargetMachine* target_machine = target->createTargetMachine(
        target_triple,
        "generic",
        "",
        opt,
        llvm::Reloc::PIC_,
        llvm::CodeModel::Small,
        llvm::CodeGenOptLevel::Aggressive  // Optimize for size and speed
    );

    if (!target_machine) {
        if (error_msg) *error_msg = "Failed to create target machine";
        return false;
    }

    module->setDataLayout(target_machine->createDataLayout());

    // Open output file
    std::error_code ec;
    llvm::raw_fd_ostream dest(output_path, ec, llvm::sys::fs::OF_None);
    if (ec) {
        if (error_msg) *error_msg = "Failed to open output file: " + ec.message();
        delete target_machine;
        return false;
    }

    // Emit object file
    llvm::legacy::PassManager pass;
    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, llvm::CodeGenFileType::ObjectFile)) {
        if (error_msg) *error_msg = "Target machine can't emit object file";
        delete target_machine;
        return false;
    }

    pass.run(*module);
    dest.flush();

    delete target_machine;
    return true;
}

} // namespace anvil

#endif // ANVIL_AOT_COMPILER_H
