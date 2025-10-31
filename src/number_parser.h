#ifndef ANVIL_NUMBER_PARSER_H
#define ANVIL_NUMBER_PARSER_H

#include <cstdint>

extern "C" {

// NUMBER helper: parses a string as a number
// Parameters: str_addr (string), str_len (count)
// Returns: number (parsed value), flag (1 if valid, 0 if invalid)
// Note: This returns TWO values - the calling code must handle both
void anvil_parse_number(const char* str, int64_t len, int64_t* number_out, int64_t* flag_out);

} // extern "C"

#endif // ANVIL_NUMBER_PARSER_H
