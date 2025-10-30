#include <catch2/catch_test_macros.hpp>
#include "ast.h"

using namespace anvil;

TEST_CASE("AST builds literals", "[ast][literal]") {
    ASTBuilder builder;

    SECTION("Single number") {
        auto ast = builder.parse("42");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::LITERAL);

        auto* lit = dynamic_cast<LiteralNode*>(ast.get());
        REQUIRE(lit != nullptr);
        REQUIRE(lit->value == 42);
    }

    SECTION("Negative number") {
        auto ast = builder.parse("-123");
        auto* lit = dynamic_cast<LiteralNode*>(ast.get());
        REQUIRE(lit != nullptr);
        REQUIRE(lit->value == -123);
    }

    SECTION("Hex number") {
        auto ast = builder.parse("0xFF");
        auto* lit = dynamic_cast<LiteralNode*>(ast.get());
        REQUIRE(lit != nullptr);
        REQUIRE(lit->value == 255);
    }
}

TEST_CASE("AST builds word calls", "[ast][word]") {
    ASTBuilder builder;

    SECTION("Single word") {
        auto ast = builder.parse("DUP");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::WORD_CALL);

        auto* word = dynamic_cast<WordCallNode*>(ast.get());
        REQUIRE(word != nullptr);
        REQUIRE(word->word_name == "DUP");
    }

    SECTION("Operator word") {
        auto ast = builder.parse("+");
        auto* word = dynamic_cast<WordCallNode*>(ast.get());
        REQUIRE(word != nullptr);
        REQUIRE(word->word_name == "+");
    }
}

TEST_CASE("AST builds sequences", "[ast][sequence]") {
    ASTBuilder builder;

    SECTION("Multiple literals") {
        auto ast = builder.parse("10 20 30");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::SEQUENCE);

        auto* seq = dynamic_cast<SequenceNode*>(ast.get());
        REQUIRE(seq != nullptr);
        REQUIRE(seq->children.size() == 3);

        REQUIRE(seq->children[0]->type == ASTNodeType::LITERAL);
        REQUIRE(seq->children[1]->type == ASTNodeType::LITERAL);
        REQUIRE(seq->children[2]->type == ASTNodeType::LITERAL);
    }

    SECTION("Mixed literals and words") {
        auto ast = builder.parse("42 DUP +");
        auto* seq = dynamic_cast<SequenceNode*>(ast.get());
        REQUIRE(seq != nullptr);
        REQUIRE(seq->children.size() == 3);

        REQUIRE(seq->children[0]->type == ASTNodeType::LITERAL);
        REQUIRE(seq->children[1]->type == ASTNodeType::WORD_CALL);
        REQUIRE(seq->children[2]->type == ASTNodeType::WORD_CALL);
    }

    SECTION("Complex expression") {
        auto ast = builder.parse("5 10 + 2 *");
        auto* seq = dynamic_cast<SequenceNode*>(ast.get());
        REQUIRE(seq != nullptr);
        REQUIRE(seq->children.size() == 5);
    }
}

TEST_CASE("AST builds definitions", "[ast][definition]") {
    ASTBuilder builder;

    SECTION("Simple definition") {
        auto ast = builder.parse(": SQUARE DUP * ;");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::DEFINITION);

        auto* def = dynamic_cast<DefinitionNode*>(ast.get());
        REQUIRE(def != nullptr);
        REQUIRE(def->name == "SQUARE");
        REQUIRE(def->body != nullptr);
        REQUIRE(def->body->type == ASTNodeType::SEQUENCE);

        auto* body = dynamic_cast<SequenceNode*>(def->body.get());
        REQUIRE(body->children.size() == 2);
    }

    SECTION("Definition with literals") {
        auto ast = builder.parse(": ADD-10 10 + ;");
        auto* def = dynamic_cast<DefinitionNode*>(ast.get());
        REQUIRE(def != nullptr);
        REQUIRE(def->name == "ADD-10");

        auto* body = dynamic_cast<SequenceNode*>(def->body.get());
        REQUIRE(body->children.size() == 2);
        REQUIRE(body->children[0]->type == ASTNodeType::LITERAL);
        REQUIRE(body->children[1]->type == ASTNodeType::WORD_CALL);
    }

    SECTION("Empty definition") {
        auto ast = builder.parse(": NOOP ;");
        auto* def = dynamic_cast<DefinitionNode*>(ast.get());
        REQUIRE(def != nullptr);
        REQUIRE(def->name == "NOOP");

        auto* body = dynamic_cast<SequenceNode*>(def->body.get());
        REQUIRE(body->children.size() == 0);
    }
}

TEST_CASE("AST builds IF...THEN", "[ast][control][if]") {
    ASTBuilder builder;

    SECTION("IF...THEN without ELSE") {
        auto ast = builder.parse("IF 42 EMIT THEN");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::IF_THEN);

        auto* if_node = dynamic_cast<IfThenNode*>(ast.get());
        REQUIRE(if_node != nullptr);
        REQUIRE(if_node->then_branch != nullptr);

        auto* then_seq = dynamic_cast<SequenceNode*>(if_node->then_branch.get());
        REQUIRE(then_seq->children.size() == 2);
    }

    SECTION("IF...ELSE...THEN") {
        auto ast = builder.parse("IF 100 ELSE 200 THEN");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::IF_ELSE_THEN);

        auto* if_node = dynamic_cast<IfElseThenNode*>(ast.get());
        REQUIRE(if_node != nullptr);
        REQUIRE(if_node->then_branch != nullptr);
        REQUIRE(if_node->else_branch != nullptr);

        auto* then_seq = dynamic_cast<SequenceNode*>(if_node->then_branch.get());
        REQUIRE(then_seq->children.size() == 1);

        auto* else_seq = dynamic_cast<SequenceNode*>(if_node->else_branch.get());
        REQUIRE(else_seq->children.size() == 1);
    }

    SECTION("Empty IF...THEN") {
        auto ast = builder.parse("IF THEN");
        auto* if_node = dynamic_cast<IfThenNode*>(ast.get());
        REQUIRE(if_node != nullptr);

        auto* then_seq = dynamic_cast<SequenceNode*>(if_node->then_branch.get());
        REQUIRE(then_seq->children.size() == 0);
    }
}

TEST_CASE("AST builds BEGIN...UNTIL", "[ast][control][begin]") {
    ASTBuilder builder;

    SECTION("Simple BEGIN...UNTIL") {
        auto ast = builder.parse("BEGIN DUP 0 > UNTIL");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::BEGIN_UNTIL);

        auto* begin_node = dynamic_cast<BeginUntilNode*>(ast.get());
        REQUIRE(begin_node != nullptr);
        REQUIRE(begin_node->body != nullptr);

        auto* body = dynamic_cast<SequenceNode*>(begin_node->body.get());
        REQUIRE(body->children.size() == 3);
    }

    SECTION("BEGIN...WHILE...REPEAT") {
        auto ast = builder.parse("BEGIN DUP 0 > WHILE DUP . REPEAT");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::BEGIN_WHILE_REPEAT);

        auto* begin_node = dynamic_cast<BeginWhileRepeatNode*>(ast.get());
        REQUIRE(begin_node != nullptr);
        REQUIRE(begin_node->condition_body != nullptr);
        REQUIRE(begin_node->loop_body != nullptr);

        auto* cond = dynamic_cast<SequenceNode*>(begin_node->condition_body.get());
        REQUIRE(cond->children.size() == 3);

        auto* loop = dynamic_cast<SequenceNode*>(begin_node->loop_body.get());
        REQUIRE(loop->children.size() == 2);
    }
}

TEST_CASE("AST builds DO...LOOP", "[ast][control][do]") {
    ASTBuilder builder;

    SECTION("Simple DO...LOOP") {
        auto ast = builder.parse("DO I . LOOP");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::DO_LOOP);

        auto* do_node = dynamic_cast<DoLoopNode*>(ast.get());
        REQUIRE(do_node != nullptr);
        REQUIRE(!do_node->is_plus_loop);
        REQUIRE(do_node->body != nullptr);

        auto* body = dynamic_cast<SequenceNode*>(do_node->body.get());
        REQUIRE(body->children.size() == 2);
    }

    SECTION("DO...+LOOP") {
        auto ast = builder.parse("DO I . 2 +LOOP");
        auto* do_node = dynamic_cast<DoLoopNode*>(ast.get());
        REQUIRE(do_node != nullptr);
        REQUIRE(do_node->is_plus_loop);

        auto* body = dynamic_cast<SequenceNode*>(do_node->body.get());
        REQUIRE(body->children.size() == 3);
    }

    SECTION("Empty DO...LOOP") {
        auto ast = builder.parse("DO LOOP");
        auto* do_node = dynamic_cast<DoLoopNode*>(ast.get());
        REQUIRE(do_node != nullptr);

        auto* body = dynamic_cast<SequenceNode*>(do_node->body.get());
        REQUIRE(body->children.size() == 0);
    }
}

TEST_CASE("AST builds nested structures", "[ast][nested]") {
    ASTBuilder builder;

    SECTION("IF inside definition") {
        auto ast = builder.parse(": ABS DUP 0 < IF NEGATE THEN ;");
        REQUIRE(ast->type == ASTNodeType::DEFINITION);

        auto* def = dynamic_cast<DefinitionNode*>(ast.get());
        auto* body = dynamic_cast<SequenceNode*>(def->body.get());
        REQUIRE(body->children.size() == 4);

        // Fourth child should be IF...THEN
        REQUIRE(body->children[3]->type == ASTNodeType::IF_THEN);
    }

    SECTION("BEGIN inside definition") {
        auto ast = builder.parse(": COUNTDOWN BEGIN DUP . 1 - DUP 0 = UNTIL DROP ;");
        auto* def = dynamic_cast<DefinitionNode*>(ast.get());
        auto* body = dynamic_cast<SequenceNode*>(def->body.get());

        // Should have BEGIN...UNTIL and DROP
        REQUIRE(body->children.size() == 2);
        REQUIRE(body->children[0]->type == ASTNodeType::BEGIN_UNTIL);
    }

    SECTION("DO...LOOP inside definition") {
        auto ast = builder.parse(": TEST 10 0 DO I . LOOP ;");
        auto* def = dynamic_cast<DefinitionNode*>(ast.get());
        auto* body = dynamic_cast<SequenceNode*>(def->body.get());

        REQUIRE(body->children.size() == 3);
        REQUIRE(body->children[2]->type == ASTNodeType::DO_LOOP);
    }
}

TEST_CASE("AST builds string literals", "[ast][string]") {
    ASTBuilder builder;

    SECTION("Simple string") {
        auto ast = builder.parse("S\" Hello World\"");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::STRING_LITERAL);

        auto* str = dynamic_cast<StringLiteralNode*>(ast.get());
        REQUIRE(str != nullptr);
        REQUIRE(str->value == "Hello World");
    }

    SECTION("String in sequence") {
        auto ast = builder.parse("42 S\" test\" DROP");
        auto* seq = dynamic_cast<SequenceNode*>(ast.get());
        REQUIRE(seq != nullptr);
        REQUIRE(seq->children.size() == 3);
        REQUIRE(seq->children[1]->type == ASTNodeType::STRING_LITERAL);
    }
}

TEST_CASE("AST handles multiple definitions", "[ast][multiple]") {
    ASTBuilder builder;

    SECTION("Two definitions in sequence") {
        auto ast = builder.parse(": DOUBLE DUP + ; : QUADRUPLE DOUBLE DOUBLE ;");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::SEQUENCE);

        auto* seq = dynamic_cast<SequenceNode*>(ast.get());
        REQUIRE(seq->children.size() == 2);
        REQUIRE(seq->children[0]->type == ASTNodeType::DEFINITION);
        REQUIRE(seq->children[1]->type == ASTNodeType::DEFINITION);
    }
}

TEST_CASE("AST case-insensitive keywords", "[ast][case]") {
    ASTBuilder builder;

    SECTION("Lowercase keywords") {
        auto ast = builder.parse("if 42 then");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::IF_THEN);
    }

    SECTION("Mixed case keywords") {
        auto ast = builder.parse("BeGiN dup UnTiL");
        REQUIRE(ast != nullptr);
        REQUIRE(ast->type == ASTNodeType::BEGIN_UNTIL);
    }

    SECTION("Definition with mixed case") {
        auto ast = builder.parse(": test Do I Loop ;");
        auto* def = dynamic_cast<DefinitionNode*>(ast.get());
        REQUIRE(def != nullptr);

        auto* body = dynamic_cast<SequenceNode*>(def->body.get());
        REQUIRE(body->children[0]->type == ASTNodeType::DO_LOOP);
    }
}

TEST_CASE("AST complex real-world examples", "[ast][complex]") {
    ASTBuilder builder;

    SECTION("Factorial definition") {
        auto ast = builder.parse(": FACTORIAL DUP 1 = IF DROP 1 ELSE DUP 1 - FACTORIAL * THEN ;");
        REQUIRE(ast->type == ASTNodeType::DEFINITION);

        auto* def = dynamic_cast<DefinitionNode*>(ast.get());
        REQUIRE(def->name == "FACTORIAL");
    }

    SECTION("Fizzbuzz loop") {
        auto ast = builder.parse("100 1 DO I 3 /MOD 0 = IF 5 /MOD 0 = IF THEN THEN LOOP");
        REQUIRE(ast->type == ASTNodeType::SEQUENCE);

        auto* seq = dynamic_cast<SequenceNode*>(ast.get());
        REQUIRE(seq != nullptr);
        REQUIRE(seq->children.size() == 3); // 100, 1, DO...LOOP
        REQUIRE(seq->children[2]->type == ASTNodeType::DO_LOOP);
    }
}
