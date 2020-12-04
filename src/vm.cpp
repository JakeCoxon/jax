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
#define VALUE_SIZE_SLOTS (sizeof(Value) / sizeof(uint32_t))

// Overloaded helper for visit
template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

enum class OpCode: uint8_t {
    Constant,
    ConstantDouble,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    GetLocalDouble,
    SetLocal,
    SetLocalDouble,
    Equal,
    EqualDouble,
    Greater,
    Less,
    Add,
    AddDouble,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    PrintDouble,
    ToStringDouble,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Return,
};

struct Chunk {
    std::vector<uint8_t> code;
    std::vector<uint32_t> constants;
    std::vector<double> doubleConstants;
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
        // printf("VALUE_SIZE_SLOTS: %lu\n", VALUE_SIZE_SLOTS);
        size_t new_index = constants.size();
        constants.resize(constants.size() + VALUE_SIZE_SLOTS);
        Value *ptr = reinterpret_cast<Value*>(&constants[new_index]);
        *ptr = value;
        // constants.push_back(value);
        return new_index;
    }
    long addDoubleConstant(double value) {
        doubleConstants.push_back(value);
        return doubleConstants.size() - 1;
    }
    double getDoubleConstant(size_t index) const {
        return doubleConstants[index];
    }
};

struct VM;

struct TypeData;

struct PrimitiveTypeData {
    std::string name;
};
struct FunctionTypeData {
    std::vector<TypeData*> parameterTypes;
    TypeData* returnType;
};
struct StructMember {
    std::string name;
    TypeData* type;
};
struct StructTypeData {
    std::string name;
    std::vector<StructMember> members;
};
struct ArrayTypeData {
    TypeData* elementType;
};

struct TypeData {
    const size_t type_id;
    mpark::variant<PrimitiveTypeData, FunctionTypeData, StructTypeData, ArrayTypeData> variant;

    PrimitiveTypeData* primitiveTypeData() { return &mpark::get<PrimitiveTypeData>(variant); }
    FunctionTypeData* functionTypeData() { return &mpark::get<FunctionTypeData>(variant); }
    StructTypeData* structTypeData() { return &mpark::get<StructTypeData>(variant); }
    ArrayTypeData* arrayTypeData() { return &mpark::get<ArrayTypeData>(variant); }

    bool isPrimitive() { return mpark::holds_alternative<PrimitiveTypeData>(variant); }
    bool isFunction() { return mpark::holds_alternative<FunctionTypeData>(variant); }
    bool isStruct() { return mpark::holds_alternative<StructTypeData>(variant); }
    bool isArray() { return mpark::holds_alternative<ArrayTypeData>(variant); }

};
using Type = TypeData*;

int slotSizeOfType(Type type);

namespace types {
    const Type Void     = new TypeData{0, PrimitiveTypeData{"void"}};
    const Type Number   = new TypeData{1, PrimitiveTypeData{"number"}};
    const Type Bool     = new TypeData{2, PrimitiveTypeData{"bool"}};
    const Type String   = new TypeData{3, PrimitiveTypeData{"string"}};
    const Type Dynamic  = new TypeData{4, PrimitiveTypeData{"dynamic"}};
    const Type Unknown  = new TypeData{5, PrimitiveTypeData{"unknown"}};
    const Type Function = new TypeData{6, PrimitiveTypeData{"function"}};
    const Type VoidPtr  = new TypeData{7, PrimitiveTypeData{"voidptr"}};
    const Type Array    = new TypeData{8, PrimitiveTypeData{"array"}};
}


struct ObjFunction: Obj {
    int arity = 0;
    Chunk chunk {};
    ObjString *name = nullptr;
    Type type = types::Void;
    int argSlots = -1;
    int returnSlots = -1;

    ObjFunction() {}
};

using NativeFn = std::function<void(VM *vm, int argCount)>;

struct ObjNative: Obj {
    Type type;
    NativeFn function;
    ObjNative(Type type, NativeFn function): type(type), function(function) {}
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
    std::vector<uint32_t> stack;

    InterpretResult interpret(const std::string &string);
    InterpretResult run();
    ObjString *allocateString(std::string string);
    bool beginCall(ObjFunction* function, int argCount);
    bool callValue(Value callee);
    void printOperation(int argCount);

    void binaryOperation(OpCode instruction);
    void binaryOperationDouble(OpCode instruction);
    void unaryOperation(OpCode instruction);

    template<typename... Args>
    void runtimeError(const char* formatString, const Args&... args);

    void resetStack() { stack.clear(); };

    template <typename T>
    void push(T value) { 
        size_t new_index = stack.size();
        const int num_slots = sizeof(T) / sizeof(uint32_t);
        stack.resize(stack.size() + num_slots);
        T *ptr = reinterpret_cast<T*>(&stack[new_index]);
        *ptr = value;
    }
    
    // void push(Value value) { stack.push_back(value); }

    
    // Value peek(int distance) { return stack[stack.size() - distance - 1]; }
    // Value pop() {
    //     auto value = stack.back();
    //     stack.pop_back();
    //     return value;
    // }

    // template <typename T>
    // T peek(int distance) { 
    //     size_t index = stack.size() - distance - 1
    //     reinterpret_cast<T*>(&stack[index]);
    //     return stack[stack.size() - distance - 1];
    // }

    

    template <typename T>
    T peek() {
        const int num_slots = sizeof(T) / sizeof(uint32_t);
        size_t index = stack.size() - num_slots;
        return *reinterpret_cast<T*>(&stack[index]);
    }


    template <typename T>
    T &stack_as(size_t index) {
        return *reinterpret_cast<T*>(&stack[index]);
    }

    template <typename T>
    T pop() {
        const int num_slots = sizeof(T) / sizeof(uint32_t);
        size_t index = stack.size() - num_slots;
        auto value = reinterpret_cast<T*>(&stack[index]);
        // auto value = stack.back();
        assert(stack.size() >= num_slots);
        for (int i = 0; i < num_slots; i++) {
            stack.pop_back();
        }
        return *value;
    }

    
};

InterpretResult VM::interpret(const std::string &source) {
    ObjFunction *function = compile(source);
    if (!function) {
        return InterpretResult::CompileError;
    }

    // push(function);
    // push<double>(0);
    frames.push_back(CallFrame { function, 0, 0 });

    // InterpretResult result = run();
    InterpretResult result = InterpretResult::Ok;
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
        return frame->function->chunk.getConstant(readByte());
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
#endif

        if (frame->ip >= frame->function->chunk.code.size()) {
            return InterpretResult::Ok; // We got to the end
        }

#ifdef DEBUG_TRACE_EXECUTION
        disassembleInstruction(frame->function->chunk, frame->ip);
#endif
        

        auto instruction = OpCode(readByte());
        switch (instruction) {
            case OpCode::Constant: {
                Value constant = readConstant();
                push<Value>(constant);
                break;
            }
            case OpCode::ConstantDouble: {
                // Value constant = readConstant();
                double constant = frame->function->chunk.getDoubleConstant(readByte());
                // getDoubleConstant
                push<double>(constant);
                break;
            }
            case OpCode::Nil: 
                // assert(false);
                push<double>(0.0);
                // push<double>(Value::Nil());
                break;
            case OpCode::True: push<double>(true); break;
            case OpCode::False: push<double>(false); break;
            case OpCode::Pop: {
                int slots = readByte();
                while (slots) {
                    pop<uint32_t>(); slots --;
                }
                break;
            }
            case OpCode::GetLocal: {
                uint8_t slot = readByte();
                push(stack_as<Value>(frame->firstSlot + slot));
                break;
            }
            case OpCode::GetLocalDouble: {
                uint8_t slot = readByte();
                push(stack_as<double>(frame->firstSlot + slot));
                break;
            }
            case OpCode::SetLocal: {
                uint8_t slot = readByte();
                stack_as<Value>(frame->firstSlot + slot) = peek<Value>();
                break;
            }
            case OpCode::SetLocalDouble: {
                uint8_t slot = readByte();
                stack_as<double>(frame->firstSlot + slot) = peek<double>();
                break;
            }
            case OpCode::Add:
            case OpCode::Equal: {
                binaryOperation(instruction);
                if (stack.empty()) return InterpretResult::RuntimeError;
                break;
            }
            case OpCode::EqualDouble:
            case OpCode::Greater:
            case OpCode::Less:
            case OpCode::AddDouble:
            case OpCode::Subtract:
            case OpCode::Multiply:
            case OpCode::Divide: {
                binaryOperationDouble(instruction);
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
                readByte(); // discard
                tfm::printf("%s\n", pop<Value>());
                break;
            }
            case OpCode::PrintDouble: {
                readByte(); // discard
                tfm::printf("%s\n", pop<double>());
                break;
            }
            case OpCode::ToStringDouble: {
                auto str = allocateString(tfm::format("%s", pop<double>()));
                push<Value>(str);
                break;
            }
            case OpCode::Jump: {
                int offset = readShort();
                frame->ip += offset;
                break;
            }
            case OpCode::JumpIfFalse: {
                int offset = readShort();
                if (peek<double>() == 0.0) frame->ip += offset;
                break;
            }
            case OpCode::Loop: {
                int offset = readShort();
                frame->ip -= offset;
                break;
            }
            case OpCode::Call: {
                // assert(false);
                // int argCount = readByte();
                Value function = readConstant();

                if (!callValue(function)) {
                    return InterpretResult::RuntimeError;
                }
                frame = &frames.back();
                break;
            }
            case OpCode::Return: {
                // assert(false);
                double result = pop<double>();
                frames.pop_back();
                if (frames.size() == 0) {
                    return InterpretResult::Ok;    
                }
                int slots = stack.size() - frame->firstSlot;
                while (slots) {
                    pop<uint32_t>(); slots --;
                }
                push(result);
                frame = &frames.back();
                break;
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
    // if (argCount != function->arity) {
    //     runtimeError("Expected %i arguments but got %i\n", function->arity, argCount);
    //     return false;
    // }

    if (frames.size() == FRAMES_MAX) {
        runtimeError("Stack overflow.\n");
        return false;
    }
    frames.push_back(CallFrame {
        function, 0, stack.size() - function->argSlots
    });
    return true;
}



bool VM::callValue(Value callee) {
    return callee.visit(overloaded {
        [&](ObjFunction *function) -> bool {
            return beginCall(function, function->arity);
        },
        [&](ObjNative *native) -> bool {
            // assert(false);
            auto functionTypeData = native->type->functionTypeData();
            auto parameterTypes = functionTypeData->parameterTypes;
            int argCount = parameterTypes.size();
            native->function(this, argCount);
            // for (int i = 0; i < argCount; i++) { pop(); }
            int slots = 0;
            for (Type paramType : parameterTypes) {
                slots += slotSizeOfType(paramType);
            }
            auto returnSlots = slotSizeOfType(functionTypeData->returnType);

            auto stackSize = stack.size();
            for (int i = 0; i < returnSlots; i++) {
                // |    |    |    |    |    |    |
                //           ^- slots- ^- return-^
                //                               ^-- size
                stack[stackSize - slots - returnSlots + i] = stack[stackSize - returnSlots + i];
            }

            auto slotsDiff = slots - returnSlots;

            while (slotsDiff > 0) {
                pop<uint32_t>(); slotsDiff --;
            }
            // pop();
            // push(result);
            return true;
        },
        [&](auto value) -> bool {
            runtimeError("Cannot call value: %s\n", value);
            return false;
        }
    });
}

void VM::printOperation(int argCount) {
    // TODO: Fix this
    assert(false);
    // auto value = peek<double>();
    // tfm::printf("%s\n", value);
    // pop<double>();

    // ObjString &string = peek(argCount - 1).asString();

    // int numArg = 0;
    // for (size_t i = 0; i < string.text.size(); i++) {
    //     if (string.text[i] == '{' && i < string.text.size()) {
    //         i ++;
    //         tfm::printf("%s", peek(argCount - 2 - numArg));
    //         numArg++;
    //     } else {
    //         tfm::printf("%s", string.text[i]);
    //     }
    // }
    // tfm::printf("\n");
    // for (int i = 0; i < argCount; i++) {
    //     pop();
    // }
}

void VM::binaryOperationDouble(OpCode instruction) {

    double b = pop<double>();
    double a = pop<double>();

    auto func = [this](auto &a, auto &b, OpCode op) -> double {

        if (op == OpCode::Greater)   return a > b;
        if (op == OpCode::Less)      return a < b;
        if (op == OpCode::AddDouble) return a + b;
        if (op == OpCode::Subtract)  return a - b;
        if (op == OpCode::Multiply)  return a * b;
        if (op == OpCode::Divide)    return a / b;
        if (op == OpCode::EqualDouble) return a == b;

        runtimeError("Invalid operator for numbers.\n");
        push(a); push(b); // Keep the stack as it was
        return 0.0;
    };
    auto result = func(a, b, instruction);

    // if (stack.empty()) return;
    push(result);
}

void VM::binaryOperation(OpCode instruction) {

    Value b = pop<Value>();
    Value a = pop<Value>();

    rollbear::visit([this](auto &a, auto &b, OpCode op) -> void {
        using A = std::decay_t<decltype(a)>;
        using B = std::decay_t<decltype(b)>;

        if (op == OpCode::Equal) { 
            if constexpr (std::is_same_v<A, ObjString*> && std::is_same_v<B, ObjString*>) {
                push<double>(a->text == b->text ? 1.0 : 0.0);
                return;
            }
            else { push<double>(0.0); return; }
        }

        if (op == OpCode::Add) { 
            if constexpr (std::is_same_v<A, ObjString*> && std::is_same_v<B, ObjString*>) {
                push<Value>(allocateString(a->text + b->text));
                return;
            }
        }

        runtimeError("Invalid operands for operator.\n");
        push(a); push(b); // Keep the stack as it was
    }, a.variant, b.variant, instruction);
    
}

void VM::unaryOperation(OpCode instruction) {
    switch (instruction) {
        case OpCode::Not:
            push<double>(!pop<double>());
            break;
        case OpCode::Negate: {
            push<double>(-pop<double>());
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
