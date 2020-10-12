#include <vector>
#include <string>
#include <unordered_map>

#define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION
#define STACK_MAX 256

enum class OpCode: uint8_t {
    Constant,
    Nil,
    True,
    False,
    Pop,
    DefineGlobal,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
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
    std::unordered_map<std::string, Value> globals;

    InterpretResult interpret(const std::string &string);
    InterpretResult run();
    ObjString *allocateString(std::string string);
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
    auto readString = [&]() -> ObjString& { 
        return readConstant().asString();
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
            case OpCode::Pop: pop(); break;
            case OpCode::DefineGlobal: {
                auto name = readString();
                globals[name.text] = peek(0);
                pop();
                break;
            }
            case OpCode::Equal:
            case OpCode::Greater:
            case OpCode::Less:
            case OpCode::Add:
            case OpCode::Subtract:
            case OpCode::Multiply:
            case OpCode::Divide: {
                binaryOperation(instruction);
                if (stack.empty()) return InterpretResult::RuntimeError;
                break;
            }
            case OpCode::Not:
            case OpCode::Negate: {
                unaryOperation(instruction);
                if (stack.empty()) return InterpretResult::RuntimeError;
                break;
            }
            case OpCode::Print: {
                std::cout << pop() << std::endl;
                break;
            }
            case OpCode::Return: {
                std::cout << std::endl;
                return InterpretResult::Ok;
            }
        }
    }
}

ObjString *VM::allocateString(std::string text) {
    // TODO: Garbage collection
    auto objStr = new ObjString { {}, text };
    return objStr;
}


class OpEqual {};
class OpGreater {};
class OpLess {};
class OpAdd {};
class OpSubtract {};
class OpMultiply {};
class OpDivide {};

using BinaryOperation = mpark::variant<
    OpEqual, OpGreater, OpLess, OpAdd, OpSubtract, OpMultiply, OpDivide
>;

template <class Variant, std::size_t I = 0>
Variant variant_from_index(std::size_t index) {
    if constexpr(I >= mpark::variant_size_v<Variant>)
        throw std::runtime_error{"Variant index " + std::to_string(I + index) + " out of bounds"};
    else
        return index == 0
            ? Variant{mpark::in_place_index<I>}
            : variant_from_index<Variant, I + 1>(index - 1);
}

struct BinaryOperatorVisitor {
    VM &vm;

    // Arithmetic on doubles only
    Value operator()(double a, double b, OpGreater _) {    return a > b; }
    Value operator()(double a, double b, OpLess _) {       return a < b; }
    Value operator()(double a, double b, OpSubtract _) {   return a - b; }
    Value operator()(double a, double b, OpMultiply _) {   return a * b; }
    Value operator()(double a, double b, OpDivide _) {     return a / b; }

    // Add
    Value operator()(double a, double b, OpAdd _) { return a + b; }
    Value operator()(ObjString *a, ObjString *b, OpAdd _) {
        return vm.allocateString(a->text + b->text);
    }
    
    // Equal
    Value operator()(ObjString *a, ObjString *b, OpEqual _) { return a->text == b->text; }

    template<class T>
    Value operator()(T a, T b, OpEqual _) { return a == b; }

    template<class T, class U>
    Value operator()(T a, U b, OpEqual _) { return false; }

    // Default case 
    template<class T, class U, class V>
    Value operator()(T a, U b, V _) {
        vm.runtimeError() << "Invalid operands for operator." << std::endl;
        return Value::Nil();
    }
};


void VM::binaryOperation(OpCode instruction) {

    Value b = peek(0);
    Value a = peek(1);
    auto binaryOpIndex = variant_from_index<BinaryOperation>(
        static_cast<size_t>(instruction) - static_cast<size_t>(OpCode::Equal)
    );

    Value result = rollbear::visit(BinaryOperatorVisitor{*this}, a.variant, b.variant, binaryOpIndex);
    if (stack.empty()) return;

    pop(); pop(); push(result);

}

void VM::unaryOperation(OpCode instruction) {
    switch (instruction) {
        case OpCode::Not:
            push(pop().isFalsey());
            break;
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
        case OpCode::Pop:
            return simpleInstruction("Pop", offset);
        case OpCode::DefineGlobal:
            return simpleInstruction("DefineGlobal", offset);
        case OpCode::Equal:
            return simpleInstruction("Equal", offset);
        case OpCode::Less:
            return simpleInstruction("Less", offset);
        case OpCode::Greater:
            return simpleInstruction("Greater", offset);
        case OpCode::Add:
            return simpleInstruction("Add", offset);
        case OpCode::Subtract:
            return simpleInstruction("Subtract", offset);
        case OpCode::Multiply:
            return simpleInstruction("Multiply", offset);
        case OpCode::Divide:
            return simpleInstruction("Divide", offset);
        case OpCode::Not:
            return simpleInstruction("Not", offset);
        case OpCode::Negate:
            return simpleInstruction("Negate", offset);
        case OpCode::Print:
            return simpleInstruction("Print", offset);
        case OpCode::Return:
            return simpleInstruction("Return", offset);
    }
}

void disassembleChunk(const Chunk &chunk, const char* name) {
    printf("== %s ==\n", name);

    for (unsigned long offset = 0; offset < chunk.code.size();) {
        offset = disassembleInstruction(chunk, offset);
    }
}
