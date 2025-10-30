#include <catch2/catch_test_macros.hpp>
#include "dictionary.h"
#include "stack.h"

using namespace anvil;

// Test helper functions that can be used as execution tokens
void test_word_1(ExecutionContext* ctx) {
    ctx->data_stack[ctx->dsp++] = 42;
}

void test_word_2(ExecutionContext* ctx) {
    ctx->data_stack[ctx->dsp++] = 100;
}

void test_word_add(ExecutionContext* ctx) {
    if (ctx->dsp >= 2) {
        int64_t b = ctx->data_stack[--ctx->dsp];
        int64_t a = ctx->data_stack[--ctx->dsp];
        ctx->data_stack[ctx->dsp++] = a + b;
    }
}

TEST_CASE("Dictionary basic operations", "[dictionary]") {
    Dictionary dict;

    SECTION("Empty dictionary") {
        REQUIRE(dict.size() == 0);
        REQUIRE(dict.find_word("TEST") == nullptr);
        REQUIRE(!dict.has_word("TEST"));
    }

    SECTION("Add and find word") {
        dict.add_word("TEST", test_word_1);

        REQUIRE(dict.size() == 1);
        REQUIRE(dict.has_word("TEST"));

        const DictionaryEntry* entry = dict.find_word("TEST");
        REQUIRE(entry != nullptr);
        REQUIRE(entry->name == "TEST");
        REQUIRE(entry->xt == test_word_1);
        REQUIRE(entry->flags == WORD_NORMAL);
    }

    SECTION("Add multiple words") {
        dict.add_word("WORD1", test_word_1);
        dict.add_word("WORD2", test_word_2);
        dict.add_word("ADD", test_word_add);

        REQUIRE(dict.size() == 3);
        REQUIRE(dict.has_word("WORD1"));
        REQUIRE(dict.has_word("WORD2"));
        REQUIRE(dict.has_word("ADD"));
    }

    SECTION("Remove word") {
        dict.add_word("TEST", test_word_1);
        REQUIRE(dict.has_word("TEST"));

        dict.remove_word("TEST");
        REQUIRE(!dict.has_word("TEST"));
        REQUIRE(dict.size() == 0);
    }

    SECTION("Clear dictionary") {
        dict.add_word("WORD1", test_word_1);
        dict.add_word("WORD2", test_word_2);
        REQUIRE(dict.size() == 2);

        dict.clear();
        REQUIRE(dict.size() == 0);
        REQUIRE(!dict.has_word("WORD1"));
        REQUIRE(!dict.has_word("WORD2"));
    }
}

TEST_CASE("Dictionary case insensitivity", "[dictionary][case-insensitive]") {
    Dictionary dict;
    dict.add_word("TestWord", test_word_1);

    SECTION("Find with different cases") {
        REQUIRE(dict.has_word("TestWord"));
        REQUIRE(dict.has_word("TESTWORD"));
        REQUIRE(dict.has_word("testword"));
        REQUIRE(dict.has_word("tEsTwOrD"));
    }

    SECTION("Find returns same entry") {
        const DictionaryEntry* entry1 = dict.find_word("TestWord");
        const DictionaryEntry* entry2 = dict.find_word("TESTWORD");
        const DictionaryEntry* entry3 = dict.find_word("testword");

        REQUIRE(entry1 == entry2);
        REQUIRE(entry2 == entry3);
        REQUIRE(entry1->xt == test_word_1);
    }

    SECTION("Remove with different case") {
        dict.remove_word("testword");
        REQUIRE(!dict.has_word("TestWord"));
        REQUIRE(!dict.has_word("TESTWORD"));
    }
}

TEST_CASE("Dictionary word flags", "[dictionary][flags]") {
    Dictionary dict;

    SECTION("Normal word") {
        dict.add_word("NORMAL", test_word_1, WORD_NORMAL);
        const DictionaryEntry* entry = dict.find_word("NORMAL");

        REQUIRE(entry != nullptr);
        REQUIRE(!entry->is_immediate());
        REQUIRE(!entry->is_hidden());
        REQUIRE(!entry->is_compile_only());
    }

    SECTION("Immediate word") {
        dict.add_word("IMMEDIATE", test_word_1, WORD_IMMEDIATE);
        const DictionaryEntry* entry = dict.find_word("IMMEDIATE");

        REQUIRE(entry != nullptr);
        REQUIRE(entry->is_immediate());
        REQUIRE(!entry->is_hidden());
        REQUIRE(!entry->is_compile_only());
    }

    SECTION("Hidden word") {
        dict.add_word("HIDDEN", test_word_1, WORD_HIDDEN);

        // Hidden words should not be found
        REQUIRE(dict.find_word("HIDDEN") == nullptr);
        REQUIRE(!dict.has_word("HIDDEN"));

        // But they still count in the size
        REQUIRE(dict.size() == 1);
    }

    SECTION("Compile-only word") {
        dict.add_word("COMPILE-ONLY", test_word_1, WORD_COMPILE_ONLY);
        const DictionaryEntry* entry = dict.find_word("COMPILE-ONLY");

        REQUIRE(entry != nullptr);
        REQUIRE(!entry->is_immediate());
        REQUIRE(!entry->is_hidden());
        REQUIRE(entry->is_compile_only());
    }

    SECTION("Multiple flags") {
        dict.add_word("MULTI", test_word_1, WORD_IMMEDIATE | WORD_COMPILE_ONLY);
        const DictionaryEntry* entry = dict.find_word("MULTI");

        REQUIRE(entry != nullptr);
        REQUIRE(entry->is_immediate());
        REQUIRE(!entry->is_hidden());
        REQUIRE(entry->is_compile_only());
    }
}

TEST_CASE("Dictionary word execution", "[dictionary][execution]") {
    Dictionary dict;
    dict.add_word("PUSH42", test_word_1);
    dict.add_word("PUSH100", test_word_2);
    dict.add_word("ADD", test_word_add);

    ExecutionContext ctx;

    SECTION("Execute single word") {
        const DictionaryEntry* entry = dict.find_word("PUSH42");
        REQUIRE(entry != nullptr);

        entry->xt(&ctx);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("Execute multiple words") {
        const DictionaryEntry* push42 = dict.find_word("PUSH42");
        const DictionaryEntry* push100 = dict.find_word("PUSH100");
        const DictionaryEntry* add = dict.find_word("ADD");

        push42->xt(&ctx);
        push100->xt(&ctx);
        add->xt(&ctx);

        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 142);
    }
}

TEST_CASE("Dictionary get all words", "[dictionary][listing]") {
    Dictionary dict;
    dict.add_word("ZEBRA", test_word_1);
    dict.add_word("ALPHA", test_word_2);
    dict.add_word("BETA", test_word_add);
    dict.add_word("HIDDEN", test_word_1, WORD_HIDDEN);

    auto words = dict.get_all_words();

    SECTION("Returns all non-hidden words") {
        REQUIRE(words.size() == 3);
        // Words should be sorted alphabetically
        REQUIRE(words[0] == "ALPHA");
        REQUIRE(words[1] == "BETA");
        REQUIRE(words[2] == "ZEBRA");
    }

    SECTION("Hidden words not included") {
        bool found_hidden = false;
        for (const auto& word : words) {
            if (word == "HIDDEN") {
                found_hidden = true;
            }
        }
        REQUIRE(!found_hidden);
    }
}

TEST_CASE("Global dictionary", "[dictionary][global]") {
    // Clear global dictionary before test
    global_dictionary.clear();

    SECTION("Global dictionary is accessible") {
        global_dictionary.add_word("TEST", test_word_1);

        REQUIRE(global_dictionary.has_word("TEST"));
        REQUIRE(global_dictionary.size() == 1);
    }

    SECTION("Global dictionary persists") {
        global_dictionary.add_word("PERSISTENT", test_word_2);

        // Access in different scope
        {
            REQUIRE(global_dictionary.has_word("PERSISTENT"));
        }

        REQUIRE(global_dictionary.has_word("PERSISTENT"));
    }

    // Clean up
    global_dictionary.clear();
}

TEST_CASE("Dictionary word replacement", "[dictionary][replacement]") {
    Dictionary dict;
    dict.add_word("TEST", test_word_1);

    SECTION("Replace word with same name") {
        const DictionaryEntry* entry1 = dict.find_word("TEST");
        REQUIRE(entry1->xt == test_word_1);

        // Adding with same name replaces
        dict.add_word("TEST", test_word_2);

        const DictionaryEntry* entry2 = dict.find_word("TEST");
        REQUIRE(entry2->xt == test_word_2);
        REQUIRE(dict.size() == 1); // Still only one entry
    }

    SECTION("Replace word flags") {
        dict.add_word("TEST", test_word_1, WORD_IMMEDIATE);

        const DictionaryEntry* entry = dict.find_word("TEST");
        REQUIRE(entry->is_immediate());
        REQUIRE(entry->xt == test_word_1);
    }
}
