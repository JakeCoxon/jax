#include <vector>
#include <string>
#include <unordered_map>

#define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION
#define STACK_MAX 256
#define FRAMES_MAX 64

// Overloaded helper for visit
template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

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
    Call,
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
    ObjString *name = nullptr;
    int type = -1;

    ObjFunction() {}
};

struct CallFrame {
    ObjFunction *function;
    size_t ip;
    size_t firstSlot;
};

enum class InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
};

int disassembleInstruction(const Chunk &chunk, int offset);
ObjFunction *compile(const std::string &source); // compiler.cpp

struct VM {
    std::vector<CallFrame> frames;

    std::vector<Value> stack;
    std::unordered_map<std::string, Value> globals;

    InterpretResult interpret(const std::string &string);
    InterpretResult run();
    ObjString *allocateString(std::string string);
    bool beginCall(ObjFunction* function, int argCount);
    bool callValue(Value callee, int argCount);
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

    push(function);
    frames.push_back(CallFrame { function, 0, 0 });

    InterpretResult result = run();
    return result;
}

std::ostream &VM::runtimeError() {
    for (size_t i = 0; i < frames.size(); i++) {
        CallFrame *frame = &frames[i];
        ObjFunction *function = frame->function;
        // -1 because the IP is sitting on the next instruction to be
        // executed.
        size_t instruction = frame->ip - 1;
        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->text.c_str());
        }
    }
    resetStack();
    return std::cerr;
}

InterpretResult VM::run() {
    CallFrame *frame = &frames.back();

    auto readByte = [&]() -> uint8_t {
        return frame->function->chunk.code[frame->ip++];
    };
    auto readConstant = [&]() -> Value { 
        return frame->function->chunk.constants[readByte()];
    };
    auto readShort = [&]() -> uint16_t { 
        frame->ip += 2;
        return (frame->function->chunk.code[frame->ip - 2] << 8) | 
            frame->function->chunk.code[frame->ip - 1];
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
        disassembleInstruction(frame->function->chunk, frame->ip);
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
                push(stack[frame->firstSlot + slot]);
                break;
            }
            case OpCode::SetLocal: {
                uint8_t slot = readByte();
                stack[frame->firstSlot + slot] = peek(0);
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
                frame->ip += offset;
                break;
            }
            case OpCode::JumpIfFalse: {
                int offset = readShort();
                if (peek(0).isFalsey()) frame->ip += offset;
                break;
            }
            case OpCode::Loop: {
                int offset = readShort();
                frame->ip -= offset;
                break;
            }
            case OpCode::Call: {
                int argCount = readByte();
                if (!callValue(peek(argCount), argCount)) {
                    return InterpretResult::RuntimeError;
                }
                frame = &frames.back();
                break;
            }
            case OpCode::Return: {
                Value result = pop();
                frames.pop_back();
                if (frames.size() == 0) {
                    pop();
                    return InterpretResult::Ok;    
                }
                int numToPop = stack.size() - frame->firstSlot;
                for (int i = 0; i < numToPop; i++) {
                    pop();
                }
                push(result);
                frame = &frames.back();
            }
        }
    }
}

ObjString *VM::allocateString(std::string text) {
    // TODO: Garbage collection
    auto objStr = new ObjString(text);
    return objStr;
}

bool VM::beginCall(ObjFunction* function, int argCount) {
    if (argCount != function->arity) {
        runtimeError() << "Expected " << function->arity << 
            " arguments but got " << argCount << "." << std::endl;
        return false;
    }

    if (frames.size() == FRAMES_MAX) {
        runtimeError() << "Stack overflow." << std::endl;
        return false;
    }
    frames.push_back(CallFrame {
        function, 0, stack.size() - argCount - 1
    });
    return true;
}



bool VM::callValue(Value callee, int argCount) {
    return callee.visit(overloaded {
        [&](ObjFunction *function) -> bool {
            return beginCall(function, argCount);
        },
        [&](auto value) -> bool {
            runtimeError() << "Cannot call value: " << value << std::endl;
            return false;
        }
    });
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
