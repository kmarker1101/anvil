#ifndef ANVIL_PRIMITIVES_REGISTRY_H
#define ANVIL_PRIMITIVES_REGISTRY_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <functional>
#include <unordered_map>
#include <string>
#include <algorithm>
#include <cctype>

namespace anvil {

// Type alias for primitive emit functions
// These generate LLVM IR inline (not function calls)
using PrimitiveEmitFn = std::function<void(
    llvm::IRBuilder<>&,      // builder
    llvm::Value*,            // data_stack_ptr
    llvm::Value*,            // return_stack_ptr (may be null for data-only primitives)
    llvm::Value*,            // dsp_ptr
    llvm::Value*             // rsp_ptr (may be null for data-only primitives)
)>;

// Registry of primitive words
class PrimitivesRegistry {
private:
    std::unordered_map<std::string, PrimitiveEmitFn> primitives_;

    // Convert string to uppercase for case-insensitive lookup
    static std::string to_upper(const std::string& str) {
        std::string upper = str;
        std::transform(upper.begin(), upper.end(), upper.begin(),
                      [](unsigned char c) { return std::toupper(c); });
        return upper;
    }

public:
    PrimitivesRegistry() = default;

    // Register a primitive word
    void register_primitive(const std::string& name, PrimitiveEmitFn emit_fn) {
        std::string upper_name = to_upper(name);
        primitives_[upper_name] = emit_fn;
    }

    // Check if a word is a primitive (case-insensitive)
    bool is_primitive(const std::string& name) const {
        std::string upper_name = to_upper(name);
        return primitives_.find(upper_name) != primitives_.end();
    }

    // Get the emit function for a primitive (case-insensitive)
    // Returns nullptr if not found
    const PrimitiveEmitFn* get_primitive(const std::string& name) const {
        std::string upper_name = to_upper(name);
        auto it = primitives_.find(upper_name);
        if (it != primitives_.end()) {
            return &it->second;
        }
        return nullptr;
    }

    // Get count of registered primitives
    size_t size() const {
        return primitives_.size();
    }

    // Clear all primitives
    void clear() {
        primitives_.clear();
    }
};

// Global primitives registry
extern PrimitivesRegistry global_primitives;

// Initialize all built-in primitives
void initialize_primitives();

} // namespace anvil

#endif // ANVIL_PRIMITIVES_REGISTRY_H
