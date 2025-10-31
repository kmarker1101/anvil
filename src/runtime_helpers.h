#ifndef ANVIL_RUNTIME_HELPERS_H
#define ANVIL_RUNTIME_HELPERS_H

#include <cstdint>

// Runtime helpers that can be called from JIT-compiled code

extern "C" {

// FIND helper: searches dictionary for a word
// Parameters: name_addr (string), name_len (count)
// Returns: xt (execution token, 0 if not found)
int64_t anvil_find_word(const char* name, int64_t len);

} // extern "C"

#endif // ANVIL_RUNTIME_HELPERS_H
