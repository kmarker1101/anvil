#include "runtime_helpers.h"
#include "dictionary.h"
#include "execution_engine.h"
#include <string>
#include <cstring>

namespace anvil {
    extern Dictionary global_dictionary;
    extern AnvilExecutionEngine* global_engine; // Will be set by execution engine
}

using namespace anvil;

extern "C" {

// FIND helper: searches dictionary for a word
// Parameters: name_addr (string), name_len (count)
// Returns: xt (execution token, 0 if not found)
int64_t anvil_find_word(const char* name, int64_t len) {
    // Create string from name and length
    std::string word_name(name, static_cast<size_t>(len));

    // Look up in dictionary
    const DictionaryEntry* entry = global_dictionary.find_word(word_name);
    if (!entry) {
        return 0; // Not found
    }

    // If the word has an LLVM function, we need to get its JIT-compiled address
    if (entry->llvm_func && global_engine) {
        // Get the actual JIT-compiled address
        uint64_t addr = global_engine->get_engine()->getFunctionAddress(
            entry->llvm_func->getName().str()
        );
        if (addr) {
            return static_cast<int64_t>(addr);
        }
    }

    // Fall back to the execution token
    return reinterpret_cast<int64_t>(entry->xt);
}

} // extern "C"
