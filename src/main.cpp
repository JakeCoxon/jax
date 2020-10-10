#include <vector>
#include <stdio.h>

#define DEBUG_TRACE_EXECUTION
#define STACK_MAX 256

enum class OpCode: uint8_t {
    Constant,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
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

enum class InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
};

static void printValue(Value value) {
    printf("%g", value);
}

int disassembleInstruction(const Chunk &chunk, int offset);

struct VM {
    Chunk *chunk;
    unsigned long ip;

    std::vector<Value> stack;

    InterpretResult interpret(Chunk *chunk);
    InterpretResult run();
    void binaryOperation(OpCode instruction);

    void resetStack() { stack.clear(); };
    void push(Value value) { stack.push_back(value); }
    Value pop() {
        auto value = stack.back();
        stack.pop_back();
        return value;
    }
};

InterpretResult VM::interpret(Chunk *chunk) {
    this->chunk = chunk;
    ip = 0;
    return run();
}

InterpretResult VM::run() {
    auto readByte = [&]() -> uint8_t {
        return chunk->code[ip++];
    };
    auto readConstant = [&]() -> Value { 
        return chunk->constants[readByte()];
    };

    while (true) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (auto value: stack) {
            printf("[ ");
            printValue(value);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(*chunk, ip);
#endif

        auto instruction = OpCode(readByte());
        switch (instruction) {
            case OpCode::Constant: {
                Value constant = readConstant();
                push(constant);
                break;
            }
            case OpCode::Add:
            case OpCode::Subtract:
            case OpCode::Multiply:
            case OpCode::Divide: {
                binaryOperation(instruction);
                break;
            }
            case OpCode::Negate: push(-pop()); break;
            case OpCode::Return: {
                printValue(pop());
                printf("\n");
                return InterpretResult::Ok;
            }
        }
    }
}

void VM::binaryOperation(OpCode instruction) {
    double b = pop();
    double a = pop();

    switch (instruction) {
        case OpCode::Add:       { push(a + b); break; }
        case OpCode::Subtract:  { push(a - b); break; }
        case OpCode::Multiply:  { push(a * b); break; }
        case OpCode::Divide:    { push(a / b); break; }
        default: {}
    }
}

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
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
        case OpCode::Add:
            return simpleInstruction("Add", offset);
        case OpCode::Subtract:
            return simpleInstruction("Subtract", offset);
        case OpCode::Multiply:
            return simpleInstruction("Multiply", offset);
        case OpCode::Divide:
            return simpleInstruction("Divide", offset);
        case OpCode::Negate:
            return simpleInstruction("Negate", offset);
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
    VM vm;
    Chunk chunk;

    int constant = chunk.addConstant(1.2);
    chunk.write(OpCode::Constant, 123);
    chunk.write(constant, 123);

    constant = chunk.addConstant(3.4);
    chunk.write(OpCode::Constant, 123);
    chunk.write(constant, 123);

    chunk.write(OpCode::Add, 123);

    constant = chunk.addConstant(5.6);
    chunk.write(OpCode::Constant, 123);
    chunk.write(constant, 123);

    chunk.write(OpCode::Divide, 123);
    chunk.write(OpCode::Negate, 123);


    chunk.write(OpCode::Return, 123);
    disassembleChunk(chunk, "test chunk");

    vm.interpret(&chunk);

    return 0;
}
