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
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
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
    Jump,
    JumpIfFalse,
    Loop,
    Return,
};

struct Chunk {
    std::vector<uint8_t> code;
    std::vector<Value> constants;
    std::vector<int> lines;

    Chunk() {}

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

struct ObjFunction: Obj {
    int arity = 0;
    Chunk chunk {};
    ObjString *name;

    ObjFunction() {}
};

enum class InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
};

int disassembleInstruction(const Chunk &chunk, int offset);
ObjFunction *compile(const std::string &source); // compiler.cpp

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
    ObjFunction *function = compile(source);
    if (!function) {
        return InterpretResult::CompileError;
    }

    this->chunk = &function->chunk;
    this->ip = 0;
    push(function);

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
    auto readShort = [&]() -> uint16_t { 
        ip += 2;
        return (chunk->code[ip - 2] << 8) | chunk->code[ip - 1];
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
            case OpCode::GetGlobal: {
                auto name = readString();
                auto found = globals.find(name.text);
                if (found == globals.end()) {
                    runtimeError() << "Undefined variable '" << name.text << "'" << std::endl;
                    return InterpretResult::RuntimeError;
                }
                push(found->second);
                break;
            }
            case OpCode::SetGlobal: {
                auto name = readString();
                auto found = globals.find(name.text);
                if (found == globals.end()) {
                    runtimeError() << "Undefined variable '" << name.text << "'" << std::endl;
                    return InterpretResult::RuntimeError;
                }
                found->second = peek(0);
                break;
            }
            case OpCode::GetLocal: {
                uint8_t slot = readByte();
                push(stack[slot]);
                break;
            }
            case OpCode::SetLocal: {
                uint8_t slot = readByte();
                stack[slot] = peek(0);
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
            case OpCode::Jump: {
                int offset = readShort();
                ip += offset;
                break;
            }
            case OpCode::JumpIfFalse: {
                int offset = readShort();
                if (peek(0).isFalsey()) ip += offset;
                break;
            }
            case OpCode::Loop: {
                int offset = readShort();
                ip -= offset;
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

void VM::binaryOperation(OpCode instruction) {

    Value b = peek(0);
    Value a = peek(1);

    Value result = rollbear::visit([this](auto &a, auto &b, OpCode op) -> Value {
        using A = std::decay_t<decltype(a)>;
        using B = std::decay_t<decltype(b)>;

        if constexpr (std::is_same_v<A, double> && std::is_same_v<B, double>) {
            if (op == OpCode::Greater)  return a > b;
            if (op == OpCode::Less)     return a < b;
            if (op == OpCode::Subtract) return a - b;
            if (op == OpCode::Multiply) return a * b;
            if (op == OpCode::Divide)   return a / b;
        }

        if (op == OpCode::Equal) { 
            if constexpr (std::is_same_v<A, ObjString*> && std::is_same_v<B, ObjString*>) { 
                return a->text == b->text;
            } else if constexpr (std::is_same_v<A, B>) { return a == b; }
            else { return false; }
        }

        if (op == OpCode::Add) { 
            if constexpr (std::is_same_v<A, ObjString*> && std::is_same_v<B, ObjString*>) {
                return allocateString(a->text + b->text);
            }
            else if constexpr (std::is_same_v<A, double> && std::is_same_v<B, double>) {
                return a + b;
            }
        }

        runtimeError() << "Invalid operands for operator." << std::endl;
        return Value::Nil();
    }, a.variant, b.variant, instruction);

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

struct OutputVisitor {
    std::ostream &os;
    void operator()(mpark::monostate n) { os << "nil"; }
    void operator()(double d) { os << d; }
    void operator()(bool b) { os << (b ? "true" : "false"); }
    void operator()(ObjString *s) { os << s->text; }
    void operator()(ObjFunction *f) { 
        if (!f->name)
            os << "<script>";
        else os << "<fn " << f->name->text << ">";
    }

    template <typename T> bool operator()(T b) = delete; // Catch non-explicit conversions
};

inline std::ostream &operator<<(std::ostream &os, const Value &v) {
    v.visit(OutputVisitor { os });
    return os;
}

struct ToStringVisitor {
    std::string operator()(mpark::monostate n) { return "nil"; }
    std::string operator()(double d) { return std::to_string(d); }
    std::string operator()(bool b) { return (b ? "true" : "false"); }
    std::string operator()(ObjString *s) { return s->text; }
    std::string operator()(ObjFunction *f) { 
        if (!f->name)
            return "<script>";
        return "<fn " + f->name->text + ">";
    }

    template <typename T> bool operator()(T b) = delete; // Catch non-explicit conversions
};

std::string toString(const Value &value) {
    return value.visit(ToStringVisitor());
}


struct IsFalseyVisitor {
    bool operator()(mpark::monostate n) { return true; }
    bool operator()(double d) { return false; }
    bool operator()(bool b) { return !b; }
    bool operator()(ObjString *s) { return false; }
    bool operator()(ObjFunction *f) { return false; }

    template <typename T> bool operator()(T b) = delete; // Catch non-explicit conversions
};

bool Value::isFalsey() const {
    return visit(IsFalseyVisitor());
}

struct IsEqualVisitor {
    template<class T> bool operator()(const T a, const T b) const { return a == b; }
    template<class T, class U> bool operator()(const T a, const U b) const { return false; }
};

bool Value::operator==(Value &rhs) const {
    return rollbear::visit(IsEqualVisitor{}, variant, rhs.variant);
}
