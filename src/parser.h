#ifndef ANVIL_PARSER_H
#define ANVIL_PARSER_H

#include <string>
#include <vector>
#include <optional>
#include <cstdint>
#include <cctype>
#include <algorithm>

namespace anvil {

// Token types for parsed Forth input
enum class TokenType {
    WORD,      // A word (to be looked up in dictionary)
    NUMBER,    // A numeric literal
    STRING,    // A string literal (for S" and .")
    COMMENT,   // A comment (ignored during execution)
    END        // End of input
};

// A single token
struct Token {
    TokenType type;
    std::string text;       // Original text
    int64_t number_value;   // Parsed number (if type == NUMBER)

    Token(TokenType t, const std::string& txt, int64_t num = 0)
        : type(t), text(txt), number_value(num) {}
};

// Parser for Forth source code
class Parser {
private:
    std::string input_;
    size_t pos_;

    // Skip whitespace
    void skip_whitespace() {
        while (pos_ < input_.size() && std::isspace(input_[pos_])) {
            pos_++;
        }
    }

    // Check if at end of input
    bool at_end() const {
        return pos_ >= input_.size();
    }

    // Peek at current character
    char peek() const {
        return at_end() ? '\0' : input_[pos_];
    }

    // Advance position
    char advance() {
        return at_end() ? '\0' : input_[pos_++];
    }

    // Parse a word (sequence of non-whitespace characters)
    std::string parse_word() {
        std::string word;
        while (!at_end() && !std::isspace(peek())) {
            word += advance();
        }
        return word;
    }

    // Try to parse a number from a string
    // Returns true if successful, false otherwise
    // Supports: decimal, hex (0x or $), binary (0b or %), negative numbers
    bool try_parse_number(const std::string& text, int64_t& result) {
        if (text.empty()) {
            return false;
        }

        std::string num_str = text;
        bool negative = false;
        size_t start_idx = 0;

        // Check for negative sign
        if (num_str[0] == '-') {
            negative = true;
            start_idx = 1;
            if (num_str.size() == 1) {
                return false; // Just a minus sign
            }
        }

        int base = 10;

        // Check for hex prefix (0x or $)
        if (start_idx < num_str.size() - 1) {
            if (num_str[start_idx] == '0' &&
                (num_str[start_idx + 1] == 'x' || num_str[start_idx + 1] == 'X')) {
                base = 16;
                start_idx += 2;
            } else if (num_str[start_idx] == '$') {
                base = 16;
                start_idx += 1;
            }
            // Check for binary prefix (0b or %)
            else if (num_str[start_idx] == '0' &&
                     (num_str[start_idx + 1] == 'b' || num_str[start_idx + 1] == 'B')) {
                base = 2;
                start_idx += 2;
            } else if (num_str[start_idx] == '%') {
                base = 2;
                start_idx += 1;
            }
        }

        // Parse the number
        try {
            size_t idx;
            result = std::stoll(num_str.substr(start_idx), &idx, base);

            // Check if entire string was consumed
            if (idx + start_idx != num_str.size()) {
                return false;
            }

            if (negative) {
                result = -result;
            }

            return true;
        } catch (...) {
            return false;
        }
    }

public:
    Parser() : pos_(0) {}

    // Set input string to parse
    void set_input(const std::string& input) {
        input_ = input;
        pos_ = 0;
    }

    // Get next token
    Token next_token() {
        skip_whitespace();

        if (at_end()) {
            return Token(TokenType::END, "", 0);
        }

        // Handle line comments: \ to end of line
        if (peek() == '\\') {
            advance(); // skip '\'
            std::string comment;
            while (!at_end() && peek() != '\n') {
                comment += advance();
            }
            return Token(TokenType::COMMENT, comment, 0);
        }

        // Handle block comments: ( to )
        if (peek() == '(') {
            size_t start = pos_;
            advance(); // skip '('
            std::string comment;
            int depth = 1;
            while (!at_end() && depth > 0) {
                char ch = advance();
                if (ch == '(') {
                    depth++;
                } else if (ch == ')') {
                    depth--;
                }
                if (depth > 0) {
                    comment += ch;
                }
            }
            return Token(TokenType::COMMENT, comment, 0);
        }

        // Check for string literals first: S" or ."
        if ((peek() == 'S' || peek() == 's' || peek() == '.') && pos_ + 1 < input_.size() && input_[pos_ + 1] == '"') {
            // Consume S" or ."
            advance(); // S or .
            advance(); // "

            // Skip one space after S" or ." if present
            if (!at_end() && peek() == ' ') {
                advance();
            }
            // Consume everything until closing "
            std::string str;
            while (!at_end() && peek() != '"') {
                str += advance();
            }
            if (!at_end() && peek() == '"') {
                advance(); // skip closing "
            }
            return Token(TokenType::STRING, str, 0);
        }

        // Parse a word
        std::string word = parse_word();

        // Check for common errors
        if (word.size() > 1 && word[0] == ':') {
            throw std::runtime_error(word + " ?");
        }
        if (word.size() > 1 && word[0] == ';') {
            throw std::runtime_error(word + " ?");
        }

        // Try to parse as number
        int64_t num_value;
        if (try_parse_number(word, num_value)) {
            return Token(TokenType::NUMBER, word, num_value);
        }

        // It's a word
        return Token(TokenType::WORD, word, 0);
    }

    // Tokenize entire input into vector
    std::vector<Token> tokenize(const std::string& input) {
        set_input(input);
        std::vector<Token> tokens;

        while (true) {
            Token token = next_token();
            if (token.type == TokenType::END) {
                break;
            }
            // Skip comments by default
            if (token.type != TokenType::COMMENT) {
                tokens.push_back(token);
            }
        }

        return tokens;
    }

    // Check if a string is a valid number
    static bool is_number(const std::string& text) {
        Parser p;
        int64_t dummy;
        return p.try_parse_number(text, dummy);
    }

    // Parse a number from string (throws if invalid)
    static int64_t parse_number(const std::string& text) {
        Parser p;
        int64_t result;
        if (!p.try_parse_number(text, result)) {
            throw std::runtime_error("Invalid number: " + text);
        }
        return result;
    }
};

} // namespace anvil

#endif // ANVIL_PARSER_H
