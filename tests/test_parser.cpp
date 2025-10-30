#include <catch2/catch_test_macros.hpp>
#include "parser.h"

using namespace anvil;

TEST_CASE("Parser tokenizes simple words", "[parser][tokenize]") {
    Parser parser;

    SECTION("Single word") {
        auto tokens = parser.tokenize("DUP");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::WORD);
        REQUIRE(tokens[0].text == "DUP");
    }

    SECTION("Multiple words") {
        auto tokens = parser.tokenize("DUP SWAP DROP");
        REQUIRE(tokens.size() == 3);
        REQUIRE(tokens[0].text == "DUP");
        REQUIRE(tokens[1].text == "SWAP");
        REQUIRE(tokens[2].text == "DROP");
        for (const auto& token : tokens) {
            REQUIRE(token.type == TokenType::WORD);
        }
    }

    SECTION("Words with various whitespace") {
        auto tokens = parser.tokenize("  DUP  \t SWAP \n DROP  ");
        REQUIRE(tokens.size() == 3);
        REQUIRE(tokens[0].text == "DUP");
        REQUIRE(tokens[1].text == "SWAP");
        REQUIRE(tokens[2].text == "DROP");
    }
}

TEST_CASE("Parser parses decimal numbers", "[parser][numbers][decimal]") {
    Parser parser;

    SECTION("Positive decimal") {
        auto tokens = parser.tokenize("42");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == 42);
    }

    SECTION("Negative decimal") {
        auto tokens = parser.tokenize("-123");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == -123);
    }

    SECTION("Zero") {
        auto tokens = parser.tokenize("0");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == 0);
    }

    SECTION("Large number") {
        auto tokens = parser.tokenize("9223372036854775807"); // Max int64_t
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == 9223372036854775807LL);
    }
}

TEST_CASE("Parser parses hexadecimal numbers", "[parser][numbers][hex]") {
    Parser parser;

    SECTION("Hex with 0x prefix") {
        auto tokens = parser.tokenize("0xFF");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == 255);
    }

    SECTION("Hex with $ prefix (Forth style)") {
        auto tokens = parser.tokenize("$FF");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == 255);
    }

    SECTION("Negative hex") {
        auto tokens = parser.tokenize("-0x10");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == -16);
    }

    SECTION("Hex with lowercase") {
        auto tokens = parser.tokenize("0xdeadbeef");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == 0xdeadbeefLL);
    }
}

TEST_CASE("Parser parses binary numbers", "[parser][numbers][binary]") {
    Parser parser;

    SECTION("Binary with 0b prefix") {
        auto tokens = parser.tokenize("0b1010");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == 10);
    }

    SECTION("Binary with % prefix (Forth style)") {
        auto tokens = parser.tokenize("%1111");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == 15);
    }

    SECTION("Negative binary") {
        auto tokens = parser.tokenize("-0b101");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == -5);
    }
}

TEST_CASE("Parser handles comments", "[parser][comments]") {
    Parser parser;

    SECTION("Line comment") {
        auto tokens = parser.tokenize("DUP \\ this is a comment\n SWAP");
        REQUIRE(tokens.size() == 2);
        REQUIRE(tokens[0].text == "DUP");
        REQUIRE(tokens[1].text == "SWAP");
    }

    SECTION("Block comment") {
        auto tokens = parser.tokenize("DUP ( this is a comment ) SWAP");
        REQUIRE(tokens.size() == 2);
        REQUIRE(tokens[0].text == "DUP");
        REQUIRE(tokens[1].text == "SWAP");
    }

    SECTION("Nested block comments") {
        auto tokens = parser.tokenize("DUP ( outer ( inner ) comment ) SWAP");
        REQUIRE(tokens.size() == 2);
        REQUIRE(tokens[0].text == "DUP");
        REQUIRE(tokens[1].text == "SWAP");
    }

    SECTION("Comment at end") {
        auto tokens = parser.tokenize("DUP SWAP \\ final comment");
        REQUIRE(tokens.size() == 2);
        REQUIRE(tokens[0].text == "DUP");
        REQUIRE(tokens[1].text == "SWAP");
    }
}

TEST_CASE("Parser handles string literals", "[parser][strings]") {
    Parser parser;

    SECTION("Simple string") {
        auto tokens = parser.tokenize("S\" Hello World\"");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::STRING);
        REQUIRE(tokens[0].text == "Hello World");
    }

    SECTION("Empty string") {
        auto tokens = parser.tokenize("S\"\"");
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens[0].type == TokenType::STRING);
        REQUIRE(tokens[0].text == "");
    }

    SECTION("String with words") {
        auto tokens = parser.tokenize("DUP S\" test\" SWAP");
        REQUIRE(tokens.size() == 3);
        REQUIRE(tokens[0].type == TokenType::WORD);
        REQUIRE(tokens[0].text == "DUP");
        REQUIRE(tokens[1].type == TokenType::STRING);
        REQUIRE(tokens[1].text == "test");
        REQUIRE(tokens[2].type == TokenType::WORD);
        REQUIRE(tokens[2].text == "SWAP");
    }
}

TEST_CASE("Parser mixed content", "[parser][mixed]") {
    Parser parser;

    SECTION("Words and numbers") {
        auto tokens = parser.tokenize("42 DUP + 100 SWAP");
        REQUIRE(tokens.size() == 5);
        REQUIRE(tokens[0].type == TokenType::NUMBER);
        REQUIRE(tokens[0].number_value == 42);
        REQUIRE(tokens[1].type == TokenType::WORD);
        REQUIRE(tokens[1].text == "DUP");
        REQUIRE(tokens[2].type == TokenType::WORD);
        REQUIRE(tokens[2].text == "+");
        REQUIRE(tokens[3].type == TokenType::NUMBER);
        REQUIRE(tokens[3].number_value == 100);
        REQUIRE(tokens[4].type == TokenType::WORD);
        REQUIRE(tokens[4].text == "SWAP");
    }

    SECTION("Different number bases") {
        auto tokens = parser.tokenize("10 0x10 %10 $10");
        REQUIRE(tokens.size() == 4);
        REQUIRE(tokens[0].number_value == 10);  // decimal
        REQUIRE(tokens[1].number_value == 16);  // hex
        REQUIRE(tokens[2].number_value == 2);   // binary
        REQUIRE(tokens[3].number_value == 16);  // hex with $
    }

    SECTION("Complex Forth expression") {
        auto tokens = parser.tokenize(": SQUARE ( n -- n^2 ) DUP * ;");
        REQUIRE(tokens.size() == 5);
        REQUIRE(tokens[0].text == ":");
        REQUIRE(tokens[1].text == "SQUARE");
        REQUIRE(tokens[2].text == "DUP");
        REQUIRE(tokens[3].text == "*");
        REQUIRE(tokens[4].text == ";");
    }
}

TEST_CASE("Parser static helper functions", "[parser][helpers]") {
    SECTION("is_number detects numbers") {
        REQUIRE(Parser::is_number("42"));
        REQUIRE(Parser::is_number("-123"));
        REQUIRE(Parser::is_number("0xFF"));
        REQUIRE(Parser::is_number("$AA"));
        REQUIRE(Parser::is_number("0b1010"));
        REQUIRE(Parser::is_number("%1111"));
        REQUIRE(!Parser::is_number("DUP"));
        REQUIRE(!Parser::is_number(""));
        REQUIRE(!Parser::is_number("-"));
    }

    SECTION("parse_number returns correct values") {
        REQUIRE(Parser::parse_number("42") == 42);
        REQUIRE(Parser::parse_number("-100") == -100);
        REQUIRE(Parser::parse_number("0xFF") == 255);
        REQUIRE(Parser::parse_number("$10") == 16);
        REQUIRE(Parser::parse_number("%1010") == 10);
    }

    SECTION("parse_number throws on invalid") {
        REQUIRE_THROWS(Parser::parse_number("DUP"));
        REQUIRE_THROWS(Parser::parse_number("not-a-number"));
    }
}

TEST_CASE("Parser edge cases", "[parser][edge]") {
    Parser parser;

    SECTION("Empty input") {
        auto tokens = parser.tokenize("");
        REQUIRE(tokens.size() == 0);
    }

    SECTION("Only whitespace") {
        auto tokens = parser.tokenize("   \t\n  ");
        REQUIRE(tokens.size() == 0);
    }

    SECTION("Only comments") {
        auto tokens = parser.tokenize("\\ just a comment");
        REQUIRE(tokens.size() == 0);
    }

    SECTION("Punctuation as words") {
        auto tokens = parser.tokenize("+ - * / < > =");
        REQUIRE(tokens.size() == 7);
        for (const auto& token : tokens) {
            REQUIRE(token.type == TokenType::WORD);
        }
    }

    SECTION("Words with numbers in them") {
        auto tokens = parser.tokenize("2DUP 2SWAP");
        REQUIRE(tokens.size() == 2);
        // These should be words, not numbers
        REQUIRE(tokens[0].type == TokenType::WORD);
        REQUIRE(tokens[0].text == "2DUP");
        REQUIRE(tokens[1].type == TokenType::WORD);
        REQUIRE(tokens[1].text == "2SWAP");
    }
}

TEST_CASE("Parser incremental tokenization", "[parser][incremental]") {
    Parser parser;

    SECTION("next_token advances correctly") {
        parser.set_input("42 DUP SWAP");

        Token t1 = parser.next_token();
        REQUIRE(t1.type == TokenType::NUMBER);
        REQUIRE(t1.number_value == 42);

        Token t2 = parser.next_token();
        REQUIRE(t2.type == TokenType::WORD);
        REQUIRE(t2.text == "DUP");

        Token t3 = parser.next_token();
        REQUIRE(t3.type == TokenType::WORD);
        REQUIRE(t3.text == "SWAP");

        Token t4 = parser.next_token();
        REQUIRE(t4.type == TokenType::END);
    }
}
