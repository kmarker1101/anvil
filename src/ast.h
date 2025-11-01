#ifndef ANVIL_AST_H
#define ANVIL_AST_H

#include <memory>
#include <vector>
#include <string>
#include <cstdint>
#include "parser.h"

namespace anvil {

// Forward declarations
struct ASTNode;
using ASTNodePtr = std::unique_ptr<ASTNode>;

// AST node types
enum class ASTNodeType {
    LITERAL,        // Number literal: pushes value onto stack
    WORD_CALL,      // Call to a word (primitive or user-defined)
    SEQUENCE,       // Sequential execution of multiple nodes
    DEFINITION,     // Word definition: : NAME ... ;
    IF_THEN,        // IF ... THEN (no else branch)
    IF_ELSE_THEN,   // IF ... ELSE ... THEN
    BEGIN_UNTIL,    // BEGIN ... UNTIL
    BEGIN_WHILE_REPEAT, // BEGIN ... WHILE ... REPEAT
    DO_LOOP,        // DO ... LOOP
    DO_PLUS_LOOP,   // DO ... +LOOP
    STRING_LITERAL, // String literal for S"
    DOT_QUOTE,      // Dot-quote string for ." (prints immediately)
    VARIABLE,       // VARIABLE name - creates a variable
    CONSTANT,       // CONSTANT name - creates a constant
    TICK            // ' name - get execution token of name
};

// Base AST node
struct ASTNode {
    ASTNodeType type;

    ASTNode(ASTNodeType t) : type(t) {}
    virtual ~ASTNode() = default;
};

// Literal node: pushes a 64-bit number
struct LiteralNode : public ASTNode {
    int64_t value;

    LiteralNode(int64_t val)
        : ASTNode(ASTNodeType::LITERAL), value(val) {}
};

// Word call node: calls a word by name
struct WordCallNode : public ASTNode {
    std::string word_name;

    WordCallNode(const std::string& name)
        : ASTNode(ASTNodeType::WORD_CALL), word_name(name) {}
};

// Sequence node: executes children in order
struct SequenceNode : public ASTNode {
    std::vector<ASTNodePtr> children;

    SequenceNode()
        : ASTNode(ASTNodeType::SEQUENCE) {}

    void add_child(ASTNodePtr node) {
        children.push_back(std::move(node));
    }
};

// Definition node: : NAME ... ;
struct DefinitionNode : public ASTNode {
    std::string name;
    ASTNodePtr body;  // Usually a SequenceNode

    DefinitionNode(const std::string& word_name, ASTNodePtr word_body)
        : ASTNode(ASTNodeType::DEFINITION), name(word_name), body(std::move(word_body)) {}
};

// Variable node: VARIABLE name
// Creates a word that pushes its address when called
struct VariableNode : public ASTNode {
    std::string name;

    VariableNode(const std::string& var_name)
        : ASTNode(ASTNodeType::VARIABLE), name(var_name) {}
};

// Constant node: CONSTANT name
// Creates a word that pushes a constant value when called
// The value must be a literal that precedes CONSTANT in the parse
struct ConstantNode : public ASTNode {
    std::string name;
    int64_t value;  // The constant value

    ConstantNode(const std::string& const_name, int64_t const_value)
        : ASTNode(ASTNodeType::CONSTANT), name(const_name), value(const_value) {}
};

// Tick node: ' name
// Pushes the execution token of the named word onto the stack
struct TickNode : public ASTNode {
    std::string name;  // The name of the word to get XT for

    TickNode(const std::string& word_name)
        : ASTNode(ASTNodeType::TICK), name(word_name) {}
};

// IF...THEN node (no else)
struct IfThenNode : public ASTNode {
    ASTNodePtr then_branch;

    IfThenNode(ASTNodePtr then_part)
        : ASTNode(ASTNodeType::IF_THEN), then_branch(std::move(then_part)) {}
};

// IF...ELSE...THEN node
struct IfElseThenNode : public ASTNode {
    ASTNodePtr then_branch;
    ASTNodePtr else_branch;

    IfElseThenNode(ASTNodePtr then_part, ASTNodePtr else_part)
        : ASTNode(ASTNodeType::IF_ELSE_THEN),
          then_branch(std::move(then_part)),
          else_branch(std::move(else_part)) {}
};

// BEGIN...UNTIL node
struct BeginUntilNode : public ASTNode {
    ASTNodePtr body;

    BeginUntilNode(ASTNodePtr loop_body)
        : ASTNode(ASTNodeType::BEGIN_UNTIL), body(std::move(loop_body)) {}
};

// BEGIN...WHILE...REPEAT node
struct BeginWhileRepeatNode : public ASTNode {
    ASTNodePtr condition_body;  // Executed before WHILE test
    ASTNodePtr loop_body;       // Executed after WHILE (if true)

    BeginWhileRepeatNode(ASTNodePtr cond_body, ASTNodePtr repeat_body)
        : ASTNode(ASTNodeType::BEGIN_WHILE_REPEAT),
          condition_body(std::move(cond_body)),
          loop_body(std::move(repeat_body)) {}
};

// DO...LOOP node
struct DoLoopNode : public ASTNode {
    ASTNodePtr body;
    bool is_plus_loop;   // true for +LOOP, false for LOOP
    bool is_question_do; // true for ?DO, false for DO

    DoLoopNode(ASTNodePtr loop_body, bool plus_loop = false, bool question_do = false)
        : ASTNode(ASTNodeType::DO_LOOP),
          body(std::move(loop_body)),
          is_plus_loop(plus_loop),
          is_question_do(question_do) {}
};

// String literal node
struct StringLiteralNode : public ASTNode {
    std::string value;

    StringLiteralNode(const std::string& str)
        : ASTNode(ASTNodeType::STRING_LITERAL), value(str) {}
};

// Dot-quote node (prints string immediately)
struct DotQuoteNode : public ASTNode {
    std::string value;

    DotQuoteNode(const std::string& str)
        : ASTNode(ASTNodeType::DOT_QUOTE), value(str) {}
};

// AST Builder - converts tokens to AST
class ASTBuilder {
private:
    std::vector<Token> tokens_;
    size_t pos_;

    // Check if at end of tokens
    bool at_end() const {
        return pos_ >= tokens_.size();
    }

    // Peek at current token
    const Token& peek() const {
        static Token end_token(TokenType::END, "", 0);
        return at_end() ? end_token : tokens_[pos_];
    }

    // Advance to next token
    const Token& advance() {
        return at_end() ? peek() : tokens_[pos_++];
    }

    // Check if current token matches text (case-insensitive for words)
    bool match_word(const std::string& word) const {
        if (at_end() || peek().type != TokenType::WORD) {
            return false;
        }
        // Case-insensitive comparison
        std::string upper_word = word;
        std::string upper_token = peek().text;
        std::transform(upper_word.begin(), upper_word.end(), upper_word.begin(), ::toupper);
        std::transform(upper_token.begin(), upper_token.end(), upper_token.begin(), ::toupper);
        return upper_word == upper_token;
    }

    // Consume a specific word, throw if not found
    void expect_word(const std::string& word) {
        if (!match_word(word)) {
            throw std::runtime_error("Expected '" + word + "' but got '" + peek().text + "'");
        }
        advance();
    }

    // Parse a single expression (literal, word call, or control structure)
    ASTNodePtr parse_expression() {
        if (at_end()) {
            return nullptr;
        }

        const Token& token = peek();

        // Handle literals
        if (token.type == TokenType::NUMBER) {
            int64_t value = token.number_value;
            advance();
            return std::make_unique<LiteralNode>(value);
        }

        // Handle string literals
        if (token.type == TokenType::STRING) {
            std::string value = token.text;
            advance();
            return std::make_unique<StringLiteralNode>(value);
        }

        // Handle dot-quote strings
        if (token.type == TokenType::DOT_QUOTE) {
            std::string value = token.text;
            advance();
            return std::make_unique<DotQuoteNode>(value);
        }

        // Handle words and control structures
        if (token.type == TokenType::WORD) {
            // Check for control structure keywords
            if (match_word(":")) {
                return parse_definition();
            } else if (match_word("VARIABLE")) {
                return parse_variable();
            } else if (match_word("CONSTANT")) {
                return parse_constant();
            } else if (match_word("'")) {
                return parse_tick();
            } else if (match_word("IF")) {
                return parse_if();
            } else if (match_word("BEGIN")) {
                return parse_begin();
            } else if (match_word("?DO")) {
                return parse_do_loop(true);  // true = is_question_do
            } else if (match_word("DO")) {
                return parse_do_loop(false); // false = regular DO
            } else {
                // Regular word call
                std::string word_name = token.text;
                advance();
                return std::make_unique<WordCallNode>(word_name);
            }
        }

        return nullptr;
    }

    // Parse word definition: : NAME ... ;
    ASTNodePtr parse_definition() {
        expect_word(":");

        if (at_end() || peek().type != TokenType::WORD) {
            throw std::runtime_error("Expected word name after ':'");
        }

        std::string name = peek().text;
        advance();

        // Parse body until ;
        auto body = std::make_unique<SequenceNode>();
        while (!at_end() && !match_word(";")) {
            auto expr = parse_expression();
            if (expr) {
                body->add_child(std::move(expr));
            }
        }

        expect_word(";");

        return std::make_unique<DefinitionNode>(name, std::move(body));
    }

    // Parse VARIABLE name
    ASTNodePtr parse_variable() {
        expect_word("VARIABLE");

        if (at_end() || peek().type != TokenType::WORD) {
            throw std::runtime_error("Expected variable name after 'VARIABLE'");
        }

        std::string name = peek().text;
        advance();

        return std::make_unique<VariableNode>(name);
    }

    // Parse CONSTANT name value
    // Syntax: CONSTANT name value
    // This is non-standard but simpler for our compile-only architecture
    ASTNodePtr parse_constant() {
        expect_word("CONSTANT");

        if (at_end() || peek().type != TokenType::WORD) {
            throw std::runtime_error("Expected constant name after 'CONSTANT'");
        }

        std::string name = peek().text;
        advance();

        if (at_end() || peek().type != TokenType::NUMBER) {
            throw std::runtime_error("Expected number value after constant name");
        }

        int64_t value = peek().number_value;
        advance();

        return std::make_unique<ConstantNode>(name, value);
    }

    // Parse ' name (tick)
    // Syntax: ' name
    // Pushes execution token of named word onto stack
    ASTNodePtr parse_tick() {
        expect_word("'");

        if (at_end() || peek().type != TokenType::WORD) {
            throw std::runtime_error("Expected word name after '");
        }

        std::string name = peek().text;
        advance();

        return std::make_unique<TickNode>(name);
    }

    // Parse IF...THEN or IF...ELSE...THEN
    ASTNodePtr parse_if() {
        expect_word("IF");

        // Parse then branch
        auto then_branch = std::make_unique<SequenceNode>();
        while (!at_end() && !match_word("ELSE") && !match_word("THEN")) {
            auto expr = parse_expression();
            if (expr) {
                then_branch->add_child(std::move(expr));
            }
        }

        // Check for ELSE
        if (match_word("ELSE")) {
            advance();

            // Parse else branch
            auto else_branch = std::make_unique<SequenceNode>();
            while (!at_end() && !match_word("THEN")) {
                auto expr = parse_expression();
                if (expr) {
                    else_branch->add_child(std::move(expr));
                }
            }

            expect_word("THEN");
            return std::make_unique<IfElseThenNode>(std::move(then_branch), std::move(else_branch));
        } else {
            expect_word("THEN");
            return std::make_unique<IfThenNode>(std::move(then_branch));
        }
    }

    // Parse BEGIN...UNTIL or BEGIN...WHILE...REPEAT
    ASTNodePtr parse_begin() {
        expect_word("BEGIN");

        // Parse body until UNTIL/WHILE
        auto body = std::make_unique<SequenceNode>();
        while (!at_end() && !match_word("UNTIL") && !match_word("WHILE")) {
            auto expr = parse_expression();
            if (expr) {
                body->add_child(std::move(expr));
            }
        }

        // Check for UNTIL vs WHILE
        if (match_word("UNTIL")) {
            advance();
            return std::make_unique<BeginUntilNode>(std::move(body));
        } else if (match_word("WHILE")) {
            advance();

            // Parse repeat body
            auto repeat_body = std::make_unique<SequenceNode>();
            while (!at_end() && !match_word("REPEAT")) {
                auto expr = parse_expression();
                if (expr) {
                    repeat_body->add_child(std::move(expr));
                }
            }

            expect_word("REPEAT");
            return std::make_unique<BeginWhileRepeatNode>(std::move(body), std::move(repeat_body));
        } else {
            throw std::runtime_error("Expected UNTIL or WHILE after BEGIN");
        }
    }

    // Parse DO...LOOP, DO...+LOOP, ?DO...LOOP, or ?DO...+LOOP
    ASTNodePtr parse_do_loop(bool is_question_do = false) {
        if (is_question_do) {
            expect_word("?DO");
        } else {
            expect_word("DO");
        }

        // Parse body
        auto body = std::make_unique<SequenceNode>();
        while (!at_end() && !match_word("LOOP") && !match_word("+LOOP")) {
            auto expr = parse_expression();
            if (expr) {
                body->add_child(std::move(expr));
            }
        }

        // Check for LOOP vs +LOOP
        bool is_plus_loop = match_word("+LOOP");
        if (is_plus_loop || match_word("LOOP")) {
            advance();
            return std::make_unique<DoLoopNode>(std::move(body), is_plus_loop, is_question_do);
        } else {
            throw std::runtime_error("Expected LOOP or +LOOP after DO or ?DO");
        }
    }

public:
    ASTBuilder() : pos_(0) {}

    // Build AST from tokens
    ASTNodePtr build(const std::vector<Token>& tokens) {
        tokens_ = tokens;
        pos_ = 0;

        // If single expression, return it directly
        // Otherwise, wrap in sequence
        auto first = parse_expression();
        if (!first) {
            return nullptr;
        }

        if (at_end()) {
            return first;
        }

        // Multiple expressions - create sequence
        auto seq = std::make_unique<SequenceNode>();
        seq->add_child(std::move(first));

        while (!at_end()) {
            auto expr = parse_expression();
            if (expr) {
                seq->add_child(std::move(expr));
            }
        }

        return seq;
    }

    // Convenience: parse string directly
    ASTNodePtr parse(const std::string& source) {
        Parser parser;
        auto tokens = parser.tokenize(source);
        return build(tokens);
    }
};

} // namespace anvil

#endif // ANVIL_AST_H
