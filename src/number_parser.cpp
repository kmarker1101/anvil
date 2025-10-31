#include "number_parser.h"
#include <string>
#include <cstring>
#include <cctype>

extern "C" {

// NUMBER helper: parses a string as a number
// Supports: decimal, negative numbers, 0x prefix for hex
void anvil_parse_number(const char* str, int64_t len, int64_t* number_out, int64_t* flag_out) {
    if (len <= 0) {
        *number_out = 0;
        *flag_out = 0;  // Invalid
        return;
    }

    // Create null-terminated string
    std::string num_str(str, static_cast<size_t>(len));

    try {
        size_t pos = 0;
        int base = 10;

        // Check for hex prefix
        if (num_str.length() > 2 && num_str[0] == '0' && (num_str[1] == 'x' || num_str[1] == 'X')) {
            base = 16;
        }

        int64_t result = std::stoll(num_str, &pos, base);

        // Check if entire string was consumed
        if (pos == num_str.length()) {
            *number_out = result;
            *flag_out = 1;  // Valid
        } else {
            *number_out = 0;
            *flag_out = 0;  // Invalid - not all characters were digits
        }
    } catch (...) {
        // Parse error
        *number_out = 0;
        *flag_out = 0;  // Invalid
    }
}

} // extern "C"
