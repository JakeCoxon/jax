#include <vector>
#include <string>

#define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION
#define STACK_MAX 256

enum class OpCode: uint8_t {
    Constant,
    Nil,
    True,
    False,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
};

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

int disassembleInstruction(const Chunk &chunk, int offset);
bool compile(const std::string &source, Chunk &chunk); // compiler.cpp

struct VM {
    Chunk *chunk;
    unsigned long ip;

    std::vector<Value> stack;

    InterpretResult interpret(const std::string &string);
    InterpretResult run();
    void binaryOperation(OpCode instruction);
    void unaryOperation(OpCode instruction);

    std::ostream &runtimeError();

    void resetStack() { stack.clear(); };
    void push(Value value) { stack.push_back(value); }
    Value peek(int distance) { return stack[stack.size() - distance - 1]; }
    Value pop() {
        auto value = stack.back();
        stack.pop_back();
        return value;
    }

    
};

InterpretResult VM::interpret(const std::string &source) {
    Chunk chunk;
    if (!compile(source, chunk)) {
        return InterpretResult::CompileError;
    }

    this->chunk = &chunk;
    this->ip = 0;

    InterpretResult result = run();
    return result;
}

std::ostream &VM::runtimeError() {
    int line = chunk->lines[ip - 1];
    resetStack();
    std::cerr << "[line " << line << " in script] ";
    return std::cerr;
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
        std::cout << "          ";
        for (auto value: stack) {
            std::cout << "[ ";
            std::cout << value;
            std::cout << " ]";
        }
        std::cout << std::endl;
        disassembleInstruction(*chunk, ip);
#endif

        auto instruction = OpCode(readByte());
        switch (instruction) {
            case OpCode::Constant: {
                Value constant = readConstant();
                push(constant);
                break;
            }
            case OpCode::Nil: push(Value::Nil()); break;
            case OpCode::True: push(true); break;
            case OpCode::False: push(false); break;
            case OpCode::Add:
            case OpCode::Subtract:
            case OpCode::Multiply:
            case OpCode::Divide: {
                binaryOperation(instruction);
                if (stack.empty()) return InterpretResult::RuntimeError;
                break;
            }
            case OpCode::Negate:
                unaryOperation(instruction);
                if (stack.empty()) return InterpretResult::RuntimeError;
                break;
            case OpCode::Return: {
                std::cout << pop();
                std::cout << std::endl;
                return InterpretResult::Ok;
            }
        }
    }
}

void VM::binaryOperation(OpCode instruction) {
    if (!peek(0).isNumber() || !peek(1).isNumber()) {
        runtimeError() << "Operands must be numbers." << std::endl;
        return;
    }
    double b = pop().asNumber();
    double a = pop().asNumber();

    switch (instruction) {
        case OpCode::Add:       { push(a + b); break; }
        case OpCode::Subtract:  { push(a - b); break; }
        case OpCode::Multiply:  { push(a * b); break; }
        case OpCode::Divide:    { push(a / b); break; }
        default: return;
    }
}

void VM::unaryOperation(OpCode instruction) {
    switch (instruction) {
        case OpCode::Negate: {
            if (!peek(0).isNumber()) {
                runtimeError() << "Operand must be a number." << std::endl;
                return;
            }
            push(-pop().asNumber());
            break;
        }
        default: return;
    }
}

static int simpleInstruction(const char* name, int offset) {
    std::cout << name << std::endl;
    return offset + 1;
}

static int constantInstruction(const char* name, const Chunk &chunk, int offset) {
    uint8_t constant = chunk.code[offset + 1];
    printf("%-16s %4d '", name, constant);
    std::cout << chunk.constants[constant];
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
        case OpCode::Nil:
            return simpleInstruction("Nil", offset);
        case OpCode::True:
            return simpleInstruction("True", offset);
        case OpCode::False:
            return simpleInstruction("False", offset);
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
