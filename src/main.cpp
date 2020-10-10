#include <vector>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>


enum class OpCode: uint8_t {
    Constant,
    Return,
};

typedef double Value;

struct Chunk {
    std::vector<uint8_t> code;
    std::vector<Value> constants;
    std::vector<int> lines;

    void write(uint8_t byte, int line) {
        code.push_back(byte);
        lines.push_back(line);
    }
    void write(OpCode opcode, int line) { 
        write(static_cast<uint8_t>(opcode), line);
    }
    long addConstant(Value value) {
        constants.push_back(value);
        return constants.size() - 1;
    }
};

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

static void printValue(Value value) {
  printf("%g", value);
}

static int constantInstruction(const char* name, const Chunk &chunk, int offset) {
    uint8_t constant = chunk.code[offset + 1];
    printf("%-16s %4d '", name, constant);
    printValue(chunk.constants[constant]);
    printf("'\n");
    return offset + 2;
}


int disassembleInstruction(const Chunk &chunk, int offset) {
    printf("%04d ", offset);

    if (offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1]) {
        printf("   | ");
    } else {
        printf("%4d ", chunk.lines[offset]);
    }

    auto instruction = OpCode(chunk.code[offset]);
    switch (instruction) {
        case OpCode::Constant:
            return constantInstruction("Constant", chunk, offset);
        case OpCode::Return:
            return simpleInstruction("Return", offset);
        default:
            printf("Unknown opcode %hhu\n", instruction);
            return offset + 1;
    }
}

void disassembleChunk(const Chunk &chunk, const char* name) {
    printf("== %s ==\n", name);

    for (unsigned long offset = 0; offset < chunk.code.size();) {
        offset = disassembleInstruction(chunk, offset);
    }
}



int main(int argc, const char* argv[]) {
    Chunk chunk;

    int constant = chunk.addConstant(1.2);
    chunk.write(OpCode::Constant, 123);
    chunk.write(constant, 123);


    chunk.write(OpCode::Return, 123);
    disassembleChunk(chunk, "test chunk");

    return 0;
}
