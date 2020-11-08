#include <tinyformat.hpp>
#include <vector>
#include <array>
#include <string>
#include <iterator>
#include <unordered_map>

#define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION
#define STACK_MAX 256
#define FRAMES_MAX 64
#define VALUE_SIZE_BYTES (sizeof(Value) / sizeof(uint8_t))

// Overloaded helper for visit
template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

enum class OpCode: uint8_t {
    Constant,
    Nil,
    True,
    False,
    Pop,
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
    std::vector<uint8_t> constants;
    std::vector<int> lines;

    Chunk() {}

    void write(uint8_t byte, int line) {
        code.push_back(byte);
        lines.push_back(line);
    }
    void write(OpCode opcode, int line) { 
        write(static_cast<uint8_t>(opcode), line);
    }
    Value getConstant(size_t index) const {
        return *reinterpret_cast<const Value*>(&constants[index]);
    }
    long addConstant(Value value) {
        long new_index = constants.size();
        constants.resize(constants.size() + VALUE_SIZE_BYTES);
        Value *ptr = reinterpret_cast<Value*>(&constants[new_index]);
        *ptr = value;
        // constants.push_back(value);
        return new_index;
    }
};

struct VM;

struct ObjFunction: Obj {
    int arity = 0;
    Chunk chunk {};
    ObjString *name = nullptr;
    int type = -1;

    ObjFunction() {}
};

using NativeFn = std::function<Value(VM *vm, int argCount, Value *args)>;

struct ObjNative: Obj {
    int type;
    NativeFn function;
    ObjNative(int type, NativeFn function): type(type), function(function) {}
};

struct ObjResource: Obj {
    void *pointer;
    ObjResource(void *pointer): pointer(pointer) {}
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

struct Parser;
int disassembleInstruction(const Chunk &chunk, int offset);
ObjFunction *compile(const std::string &source); // compiler.cpp

struct VM {
    std::vector<CallFrame> frames;
    std::vector<Value> stack;

    InterpretResult interpret(const std::string &string);
    InterpretResult run();
    ObjString *allocateString(std::string string);
    bool beginCall(ObjFunction* function, int argCount);
    bool callValue(Value callee, int argCount);
    void printOperation(int argCount);
    void binaryOperation(OpCode instruction);
    void unaryOperation(OpCode instruction);

    template<typename... Args>
    void runtimeError(const char* formatString, const Args&... args);

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

template<typename... Args>
void VM::runtimeError(const char* formatString, const Args&... args) {
    for (size_t i = 0; i < frames.size(); i++) {
        CallFrame *frame = &frames[i];
        ObjFunction *function = frame->function;
        // -1 because the IP is sitting on the next instruction to be
        // executed.
        size_t instruction = frame->ip - 1;
        tfm::format(std::cerr, "[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL) {
            tfm::format(std::cerr, "script\n");
        } else {
            tfm::format(std::cerr, "%s()\n", function->name->text);
        }
    }
    tfm::format(std::cerr, formatString, args...);
    resetStack();
}

InterpretResult VM::run() {
    CallFrame *frame = &frames.back();

    auto readByte = [&]() -> uint8_t {
        return frame->function->chunk.code[frame->ip++];
    };
    auto readShort = [&]() -> uint16_t { 
        frame->ip += 2;
        return (frame->function->chunk.code[frame->ip - 2] << 8) | 
            frame->function->chunk.code[frame->ip - 1];
    };
    auto readConstant = [&]() -> Value { 
        return frame->function->chunk.getConstant(readShort());
    };
    auto readString = [&]() -> ObjString& { 
        return readConstant().asString();
    };

    while (true) {
#ifdef DEBUG_TRACE_EXECUTION
        tfm::printf("          ");
        for (auto value: stack) {
            tfm::printf("[ %s ]", value);
        }
        tfm::printf("\n");
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
                printOperation(readByte());
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
        runtimeError("Expected %i arguments but got %i\n", function->arity, argCount);
        return false;
    }

    if (frames.size() == FRAMES_MAX) {
        runtimeError("Stack overflow.\n");
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
        [&](ObjNative *native) -> bool {
            Value result = native->function(this, argCount, &stack[stack.size() - argCount]);
            for (int i = 0; i < argCount; i++) { pop(); }
            pop();
            push(result);
            return true;
        },
        [&](auto value) -> bool {
            runtimeError("Cannot call value: %s\n", value);
            return false;
        }
    });
}

void VM::printOperation(int argCount) {
    ObjString &string = peek(argCount - 1).asString();

    int numArg = 0;
    for (size_t i = 0; i < string.text.size(); i++) {
        if (string.text[i] == '{' && i < string.text.size()) {
            i ++;
            tfm::printf("%s", peek(argCount - 2 - numArg));
            numArg++;
        } else {
            tfm::printf("%s", string.text[i]);
        }
    }
    tfm::printf("\n");
    for (int i = 0; i < argCount; i++) {
        pop();
    }
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

        runtimeError("Invalid operands for operator.\n");
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
                runtimeError("Operand must be a number.\n");
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
    void operator()(ObjNative *f) { os << "<native>"; }
    void operator()(ObjResource *r) { os << "<resource>"; }

    template <typename T> bool operator()(T b) = delete; // Catch non-explicit conversions
};

std::ostream &operator<<(std::ostream &os, const Value &v) {
    v.visit(OutputVisitor { os });
    return os;
}

std::string Value::toString() {
    return tfm::format("%s", *this);
}


struct IsFalseyVisitor {
    bool operator()(mpark::monostate n) { return true; }
    bool operator()(double d) { return false; }
    bool operator()(bool b) { return !b; }
    bool operator()(ObjString *s) { return false; }
    bool operator()(ObjFunction *f) { return false; }
    bool operator()(ObjNative *f) { return false; }
    bool operator()(ObjResource *r) { return false; }

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
