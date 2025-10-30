#ifndef ANVIL_DICTIONARY_H
#define ANVIL_DICTIONARY_H

#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <cctype>

// Forward declare LLVM types
namespace llvm {
    class Function;
}

namespace anvil {

// Forward declaration
struct ExecutionContext;

// Execution token type - function pointer that takes ExecutionContext*
using ExecutionToken = void (*)(ExecutionContext*);

// Word flags
enum WordFlags : uint64_t {
    WORD_NORMAL = 0,
    WORD_IMMEDIATE = 1 << 0,  // Execute during compilation
    WORD_HIDDEN = 1 << 1,      // Hidden from dictionary searches
    WORD_COMPILE_ONLY = 1 << 2 // Can only be used during compilation
};

// Dictionary entry for a single word
struct DictionaryEntry {
    std::string name;           // Word name (stored in uppercase for case-insensitive lookup)
    ExecutionToken xt;          // Execution token (function pointer)
    llvm::Function* llvm_func;  // LLVM function (nullptr for non-compiled words)
    uint64_t flags;             // Word flags

    DictionaryEntry(const std::string& word_name, ExecutionToken exec_token, uint64_t word_flags = WORD_NORMAL, llvm::Function* func = nullptr)
        : name(to_upper(word_name)), xt(exec_token), llvm_func(func), flags(word_flags) {}

    // Helper to convert string to uppercase
    static std::string to_upper(const std::string& str) {
        std::string upper = str;
        std::transform(upper.begin(), upper.end(), upper.begin(),
                      [](unsigned char c) { return std::toupper(c); });
        return upper;
    }

    bool is_immediate() const { return flags & WORD_IMMEDIATE; }
    bool is_hidden() const { return flags & WORD_HIDDEN; }
    bool is_compile_only() const { return flags & WORD_COMPILE_ONLY; }
};

// Global dictionary
class Dictionary {
private:
    // Using unordered_map for O(1) average lookup time
    std::unordered_map<std::string, DictionaryEntry> words_;

public:
    Dictionary() = default;

    // Add a word to the dictionary
    void add_word(const std::string& name, ExecutionToken xt, uint64_t flags = WORD_NORMAL, llvm::Function* func = nullptr) {
        std::string upper_name = DictionaryEntry::to_upper(name);
        words_.insert_or_assign(upper_name, DictionaryEntry(name, xt, flags, func));
    }

    // Find a word in the dictionary (case-insensitive)
    // Returns nullptr if not found
    const DictionaryEntry* find_word(const std::string& name) const {
        std::string upper_name = DictionaryEntry::to_upper(name);
        auto it = words_.find(upper_name);
        if (it != words_.end() && !it->second.is_hidden()) {
            return &it->second;
        }
        return nullptr;
    }

    // Check if a word exists (case-insensitive)
    bool has_word(const std::string& name) const {
        return find_word(name) != nullptr;
    }

    // Remove a word from the dictionary
    void remove_word(const std::string& name) {
        std::string upper_name = DictionaryEntry::to_upper(name);
        words_.erase(upper_name);
    }

    // Get the number of words in the dictionary
    size_t size() const {
        return words_.size();
    }

    // Clear all words
    void clear() {
        words_.clear();
    }

    // Get all word names (for debugging/listing)
    std::vector<std::string> get_all_words() const {
        std::vector<std::string> names;
        names.reserve(words_.size());
        for (const auto& pair : words_) {
            if (!pair.second.is_hidden()) {
                names.push_back(pair.second.name);
            }
        }
        std::sort(names.begin(), names.end());
        return names;
    }
};

// Global dictionary instance
extern Dictionary global_dictionary;

} // namespace anvil

#endif // ANVIL_DICTIONARY_H
