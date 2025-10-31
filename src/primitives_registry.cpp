#include "primitives_registry.h"
#include "primitives.h"

namespace anvil {

// Global primitives registry instance
PrimitivesRegistry global_primitives;

// Initialize all built-in primitives
void initialize_primitives() {
    // Clear any existing primitives
    global_primitives.clear();

    // Arithmetic operations
    global_primitives.register_primitive("+", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_add(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("-", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_sub(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("*", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_mul(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("/MOD", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_divmod(builder, data_stack, dsp);
    });

    // Stack operations
    global_primitives.register_primitive("DUP", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_dup(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("DROP", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_drop(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("SWAP", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_swap(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("OVER", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_over(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("ROT", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_rot(builder, data_stack, dsp);
    });

    // Bitwise operations
    global_primitives.register_primitive("AND", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_and(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("OR", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_or(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("XOR", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_xor(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("INVERT", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_invert(builder, data_stack, dsp);
    });

    // Comparison
    global_primitives.register_primitive("<", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_lt(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("=", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_eq(builder, data_stack, dsp);
    });

    // Memory operations
    global_primitives.register_primitive("@", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_fetch(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("!", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_store(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("C@", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_cfetch(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("C!", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_cstore(builder, data_stack, dsp);
    });

    // Return stack operations (require all 4 pointers)
    global_primitives.register_primitive(">R", [](auto& builder, auto data_stack, auto return_stack, auto data_space, auto dsp, auto rsp, auto) {
        emit_to_r(builder, data_stack, return_stack, dsp, rsp);
    });

    global_primitives.register_primitive("R>", [](auto& builder, auto data_stack, auto return_stack, auto data_space, auto dsp, auto rsp, auto) {
        emit_from_r(builder, data_stack, return_stack, dsp, rsp);
    });

    global_primitives.register_primitive("R@", [](auto& builder, auto data_stack, auto return_stack, auto data_space, auto dsp, auto rsp, auto) {
        emit_r_fetch(builder, data_stack, return_stack, dsp, rsp);
    });

    global_primitives.register_primitive("2>R", [](auto& builder, auto data_stack, auto return_stack, auto data_space, auto dsp, auto rsp, auto) {
        emit_two_to_r(builder, data_stack, return_stack, dsp, rsp);
    });

    global_primitives.register_primitive("2R>", [](auto& builder, auto data_stack, auto return_stack, auto data_space, auto dsp, auto rsp, auto) {
        emit_two_from_r(builder, data_stack, return_stack, dsp, rsp);
    });

    global_primitives.register_primitive("2R@", [](auto& builder, auto data_stack, auto return_stack, auto data_space, auto dsp, auto rsp, auto) {
        emit_two_r_fetch(builder, data_stack, return_stack, dsp, rsp);
    });

    // String/IO operations
    global_primitives.register_primitive("TYPE", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_type(builder, data_stack, dsp);
    });

    // Loop control operations
    global_primitives.register_primitive("I", [](auto& builder, auto data_stack, auto return_stack, auto data_space, auto dsp, auto rsp, auto) {
        emit_i(builder, data_stack, return_stack, dsp, rsp);
    });

    global_primitives.register_primitive("J", [](auto& builder, auto data_stack, auto return_stack, auto data_space, auto dsp, auto rsp, auto) {
        emit_j(builder, data_stack, return_stack, dsp, rsp);
    });

    // I/O operations
    global_primitives.register_primitive(".", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto) {
        emit_dot(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("EMIT", [](auto& builder, auto data_stack, auto, auto, auto dsp, auto, auto) {
        emit_emit(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("CR", [](auto& builder, auto, auto, auto, auto, auto, auto) {
        emit_cr(builder);
    });

    // Terminal I/O primitives
    global_primitives.register_primitive("KEY", [](auto& builder, auto data_stack, auto, auto, auto dsp, auto, auto) {
        emit_key(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("KEY?", [](auto& builder, auto data_stack, auto, auto, auto dsp, auto, auto) {
        emit_key_question(builder, data_stack, dsp);
    });

    global_primitives.register_primitive("RAW-MODE", [](auto& builder, auto, auto, auto, auto, auto, auto) {
        emit_raw_mode(builder);
    });

    global_primitives.register_primitive("COOKED-MODE", [](auto& builder, auto, auto, auto, auto, auto, auto) {
        emit_cooked_mode(builder);
    });

    global_primitives.register_primitive("EMIT-ESC", [](auto& builder, auto data_stack, auto, auto, auto dsp, auto, auto) {
        emit_emit_esc(builder, data_stack, dsp);
    });

    // Memory allocation primitives
    global_primitives.register_primitive("HERE", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto here) {
        emit_here(builder, data_stack, dsp, data_space, here);
    });

    global_primitives.register_primitive("ALLOT", [](auto& builder, auto data_stack, auto, auto, auto dsp, auto, auto here) {
        emit_allot(builder, data_stack, dsp, here);
    });

    global_primitives.register_primitive(",", [](auto& builder, auto data_stack, auto, auto data_space, auto dsp, auto, auto here) {
        emit_comma(builder, data_stack, dsp, data_space, here);
    });
}

} // namespace anvil
