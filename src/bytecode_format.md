# Memory-Based Bytecode Format

## Format

Each instruction is stored as bytes in memory:

```
| OPCODE (1 byte) | DATA (variable) |
```

## Opcodes

```
0x01 - Primitive(prim_id)    [1 byte opcode, 1 byte prim_id]
0x02 - PushLiteral(i64)      [1 byte opcode, 8 bytes value]
0x03 - Jump(addr)            [1 byte opcode, 8 bytes addr]
0x04 - JumpIfZero(addr)      [1 byte opcode, 8 bytes addr]
0x05 - JumpIfNotZero(addr)   [1 byte opcode, 8 bytes addr]
0x06 - Call(addr)            [1 byte opcode, 8 bytes addr]
0x07 - Return                [1 byte opcode]
0x08 - PushVariable(offset)  [1 byte opcode, 8 bytes offset]
0x09 - PushConstant(i64)     [1 byte opcode, 8 bytes value]
0x0A - DoSetup               [1 byte opcode]
0x0B - QuestionDoSetup(addr) [1 byte opcode, 8 bytes addr]
0x0C - LoopCheck(addr)       [1 byte opcode, 8 bytes addr]
0x0D - PlusLoopCheck(addr)   [1 byte opcode, 8 bytes addr]
0x0E - LoopEnd               [1 byte opcode]
```

## Primitives Mapping

Each primitive gets a numeric ID (0-255):
- 0x00 = @ (Fetch)
- 0x01 = ! (Store)
- 0x02 = C@ (CFetch)
- ... etc

Primitives can provide their ID via a `Primitive::id()` method.
