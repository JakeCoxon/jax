
int slotSizeOfType(Type type) {
    if (type == types::Number || type == types::Void || type == types::Bool) {
        return 2;
    } else {
        return 4;
    }
}

Type typeByName(Parser *parser, const std::string_view &name) {
    Type type = types::Void;
    if (name == "void") {
        type = types::Void;
    } else if (name == "number") {
        type = types::Number;
    } else if (name == "bool") {
        type = types::Bool;
    } else if (name == "string") {
        type = types::String;
    } else if (name == "dynamic") {
        type = types::Dynamic;
    } else {
        std::string err = "Unknown type: ";
        err += name;
        parser->error(err);
    }
    return type;
}

int addNewType(Parser *parser, PrimitiveTypeData primData) {
    parser->types.push_back(new TypeData{ parser->types.size(), PrimitiveTypeData(primData)});
    return parser->types.size() - 1;
}
int addNewType(Parser *parser, FunctionTypeData funcData) {
    parser->types.push_back(new TypeData{ parser->types.size(), FunctionTypeData(funcData)});
    return parser->types.size() - 1;
}

void typecheckInit(Parser *parser) {
    parser->types.push_back(types::Void); // Void
    parser->types.push_back(types::Number); // Number
    parser->types.push_back(types::Bool); // Bool
    parser->types.push_back(types::String); // String
    parser->types.push_back(types::Dynamic); // Dynamic
    parser->types.push_back(types::Unknown); // Unknown
    parser->types.push_back(types::Function); // Function placeholder
}

void typecheckPop(Parser *parser) {
    if (parser->compiler->expressionTypeStack.size() == 0) {
        parser->error("Type stack was 0.");
    } else {
        parser->compiler->expressionTypeStack.pop_back();
    }
}

bool typecheckIsAssignable(Parser *parser, Type typeA, Type typeB) {
    if (typeA == types::Dynamic) {
        return true;    
    }
    if (typeB == types::Void) {
        return (typeA != types::Number && typeA != types::Bool);
    }
    return typeA == typeB;
}

void typecheckEndStatement(Parser *parser) {
    typecheckPop(parser);
    if (parser->compiler->expressionTypeStack.size() != 0) {
        std::string err = "Programmer error. There was this many types on the stack: ";
        err += std::to_string(parser->compiler->expressionTypeStack.size());
        parser->error(err);
    }
}

void typecheckIfCondition(Parser *parser) {
    // Anything can work here?
    typecheckPop(parser);
}

void typecheckVarDeclaration(Parser *parser, Type type) {
    Type backType = parser->compiler->expressionTypeStack.back();
    parser->compiler->expressionTypeStack.pop_back();
    if (type == types::Void) {
        type = backType;
    }
    if (!typecheckIsAssignable(parser, type, backType)) {
        parser->error("Cannot declare a variable with a different type.");
    }
    Local &local = parser->compiler->locals.back();
    local.type = type;
    local.stackOffset = parser->compiler->nextStackSlot;
    parser->compiler->nextStackSlot += slotSizeOfType(type);
}

void typecheckNil(Parser *parser, Type type) {
    if (type == types::Void) {
        return;
    }
    if (typecheckIsAssignable(parser, type, types::Void)) {
        parser->error("Cannot assign nil to this type.");
    }
}

void typecheckNumber(Parser *parser) {
    parser->compiler->expressionTypeStack.push_back(types::Number);
}

void typecheckBinary(Parser *parser, TokenType operatorType) {
    Compiler *compiler = parser->compiler;
    Type typeB = compiler->expressionTypeStack.back();
    compiler->expressionTypeStack.pop_back();
    Type typeA = compiler->expressionTypeStack.back();
    compiler->expressionTypeStack.pop_back();

    auto assertNumbers = [&]() {
        if (typeA != types::Number || typeB != types::Number) { 
            parser->error("Operator expects two numbers.");
        }
    };

    auto assertEqual = [&]() {
        if (typeA != typeB) { 
            parser->error("Types don't match");
        }
    };

    Type resultType;
    switch (operatorType) {
        case TokenType::BangEqual:    assertEqual();   resultType = types::Bool; break;
        case TokenType::EqualEqual:   assertEqual();   resultType = types::Bool; break;
        case TokenType::Greater:      assertNumbers(); resultType = types::Bool; break;
        case TokenType::GreaterEqual: assertNumbers(); resultType = types::Bool; break;
        case TokenType::Less:         assertNumbers(); resultType = types::Bool; break;
        case TokenType::LessEqual:    assertNumbers(); resultType = types::Bool; break;
        case TokenType::Plus:         assertEqual();   resultType = typeA; break;
        case TokenType::Minus:        assertEqual();   resultType = typeA; break;
        case TokenType::Star:         assertEqual();   resultType = typeA; break;
        case TokenType::Slash:        assertEqual();   resultType = typeA; break;
        default:
            parser->error("Unrecognised operator.");
            return;
    }
    compiler->expressionTypeStack.push_back(resultType);
}

void typecheckLiteral(Parser *parser) {
    switch (parser->previous.type) {
        case TokenType::False:
            parser->compiler->expressionTypeStack.push_back(types::Bool);
            break;
        case TokenType::True:
            parser->compiler->expressionTypeStack.push_back(types::Bool);
            break;
        case TokenType::Nil:
            parser->compiler->expressionTypeStack.push_back(types::Void);
            break;
        default: return;
    }
}

void typecheckString(Parser *parser) {
    parser->compiler->expressionTypeStack.push_back(types::String);
}

void typecheckAnd(Parser *parser) {
    typecheckPop(parser);
    typecheckPop(parser);
    parser->compiler->expressionTypeStack.push_back(types::Bool);
}

void typecheckOr(Parser *parser) {
    typecheckPop(parser);
    typecheckPop(parser);
    parser->compiler->expressionTypeStack.push_back(types::Bool);
}

void typecheckAssign(Parser *parser, int local) {
    Type a = parser->compiler->locals[local].type;
    Type b = parser->compiler->expressionTypeStack.back();
    if (!typecheckIsAssignable(parser, a, b)) {
        parser->error("Cannot assign a different type.");
    }
}

void typecheckVariable(Parser *parser, int local) {
    Type type = parser->compiler->locals[local].type;
    parser->compiler->expressionTypeStack.push_back(type);
}


void typecheckParameter(Parser *parser, ObjFunction *function, Type functionType, Type argumentType) {
    Local &local = parser->compiler->locals.back();
    local.type = argumentType;
    local.stackOffset = parser->compiler->nextStackSlot;
    int slotSize = slotSizeOfType(argumentType);
    parser->compiler->nextStackSlot += slotSize;
    function->argSlots += slotSize;

    auto functionTypeObj = functionType->functionTypeData();
    functionTypeObj->parameterTypes.push_back(argumentType);

}

Type typecheckFunctionDeclaration(Parser *parser, ObjFunction *function) {
    addNewType(parser, FunctionTypeData{});
    // parser->types.push_back(new FunctionTypeData {});
    // int functionType = 
    // parser->compiler->locals.back().type = functionType;
    return parser->types.back();
}

void typecheckFunctionDeclarationReturn(Parser *parser, ObjFunction *function, Type functionType, Type returnType) {
    auto functionTypeObj = functionType->functionTypeData();
    function->type = functionType;
    functionTypeObj->returnType = returnType;
    // if (returnType != TypeId::Void) {
        function->returnSlots = 2; // :EverythingDouble
    // } else {
    //     function->returnSlots = 0;
    // }
}

Type getFunctionType(Parser *parser) {
    return parser->compiler->expressionTypeStack.back();
}

void typecheckFunctionArgument(Parser *parser, FunctionDeclaration *functionDeclaration, int argIndex) {
    Type argumentType = parser->compiler->expressionTypeStack.back();
    if (functionDeclaration->parameters[argIndex].type == types::Unknown) {
        // will be type checked later
        return;
    }
    if (!typecheckIsAssignable(parser, functionDeclaration->parameters[argIndex].type, argumentType)) {
        parser->error("Type mismatch");
    }
}

void typecheckBeginFunctionCall(Parser *parser, ObjFunction *function) {
    parser->compiler->expressionTypeStack.push_back(types::Void);
}
void typecheckEndFunctionCall(Parser *parser, Value function, int argCount) {
    for (int i = 0; i < argCount; i++) {
        typecheckPop(parser);
    }
    Type functionType = function.visit(overloaded {
        [&](ObjFunction *f) -> Type { return f->type; },
        [&](ObjNative *f) -> Type { return f->type; },
        [&](auto value) -> Type {
            parser->error("Invalid function type.");
            return types::Void;
        }
    });
    parser->compiler->expressionTypeStack.pop_back();
    auto functionTypeObj = functionType->functionTypeData();
    parser->compiler->expressionTypeStack.push_back(functionTypeObj->returnType);
}

void typecheckUpdateFunctionInstantiation(Parser *parser, Type functionType, int argCount) {
    auto &arg = parser->compiler->expressionTypeStack[parser->compiler->expressionTypeStack.size() - 1 - argCount];
    arg = functionType;
}

void typecheckReturn(Parser *parser, ObjFunction *function) {
    Type returnStatementType = parser->compiler->expressionTypeStack.back();
    auto functionTypeObj = function->type->functionTypeData();

    if (!typecheckIsAssignable(parser, functionTypeObj->returnType, returnStatementType)) {
        parser->error("Type mismatch");
    }
    typecheckEndStatement(parser);
}
void typecheckReturnNil(Parser *parser, ObjFunction *function) {
    Type returnStatementType = parser->compiler->expressionTypeStack.back();
    auto functionTypeObj = returnStatementType->functionTypeData();
    if (!typecheckIsAssignable(parser, functionTypeObj->returnType, types::Void)) {
        parser->error("Type mismatch");
    }
}

struct PrintState {
    int initialConstantIndex;
    int expectedVarargs = 0;
    bool formatted = false;
};

PrintState typecheckPrintBegin(Parser *parser) {
    PrintState ps;
    ps.initialConstantIndex = parser->currentChunk().constants.size() - 1 * VALUE_SIZE_SLOTS;
    return ps;
}

void typecheckPrintArgument(Parser *parser, PrintState *printState, int argIndex) {
    if (argIndex > 0) {
        // All types can be printed
        return;
    }

    if (!printState->formatted) {
        return;
    }

    // int firstArgType = parser->compiler->expressionTypeStack.back();
    // if (firstArgType != TypeId::String) {
    //     parser->error("Print expects a constant string as the first argument.");
    //     return;
    // }

    // if ((int)parser->currentChunk().constants.size() != printState->initialConstantIndex + 2 * VALUE_SIZE_SLOTS) {
    //     parser->error("Print expects a constant string as the first argument.");
    //     return;
    // }

    // // TODO: This is not typechecked
    // auto ptr = &parser->currentChunk().constants[parser->currentChunk().constants.size() - VALUE_SIZE_SLOTS];
    // Value stringConstant = *reinterpret_cast<Value*>(ptr);
    // if (!stringConstant.isString()) {
    //     parser->error("Print expects a constant string as the first argument.");
    //     return;
    // }

    // ObjString s = stringConstant.asString();
    // for (size_t i = 0; i < s.text.size(); i++) {
    //     if (s.text[i] == '{') {
    //         i ++;
    //         if (i >= s.text.size() || s.text[i] != '}') {
    //             parser->error("Invalid syntax in print string");
    //             return;
    //         }
    //         printState->expectedVarargs ++;
    //     }
    // }
}

void typecheckPrintEnd(Parser *parser, PrintState *printState, int argCount) {
    // if (printState->expectedVarargs != argCount - 1) {
    //     parser->error("Invalid number of arguments to print statement.");
    // }
    for (int i = 0; i < argCount - 1; i++) {
        typecheckPop(parser);
    }
    typecheckEndStatement(parser);
}