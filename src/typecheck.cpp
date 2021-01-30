
int slotSizeOfType(Type type) {
    if (type == types::Void) return 0;
    if (type == types::Number || type == types::Void || type == types::Bool) {
        return 2;
    } else {
        return 4;
    }
}

Type typeByName(Parser *parser, const std::string_view &name) {
    auto it = parser->typesByName.find(std::string(name));
    if (it == parser->typesByName.end()) {
        std::string err = "Unknown type: ";
        err += name;
        parser->error(err);
    }
    return it->second;
}

template<typename T>
Type addNewType(Parser *parser, T typeData) {
    auto type = new TypeData{ parser->types.size(), T(typeData)};
    parser->types.push_back(type);
    return type;
}

template<typename T>
Type addNamedType(Parser *parser, const std::string &name, T typeData) {
    auto type = addNewType(parser, typeData);
    parser->typesByName[name] = type;
    return type;
}

void typecheckInit(Parser *parser) {
    parser->types.push_back(types::Void);
    parser->typesByName["void"] = parser->types.back();
    parser->types.push_back(types::Number);
    parser->typesByName["number"] = parser->types.back();
    parser->types.push_back(types::Bool);
    parser->typesByName["bool"] = parser->types.back();
    parser->types.push_back(types::String);
    parser->typesByName["string"] = parser->types.back();
    parser->types.push_back(types::Dynamic);
    parser->typesByName["dynamic"] = parser->types.back();
    parser->types.push_back(types::Unknown);
    parser->typesByName["unknown"] = parser->types.back();
    parser->types.push_back(types::Function);
    
    parser->types.push_back(types::VoidPtr);
    parser->typesByName["voidptr"] = parser->types.back();
    parser->types.push_back(types::Array);
    parser->typesByName["array"] = parser->types.back();
    parser->types.push_back(types::Lambda);
    parser->typesByName["lambda"] = parser->types.back();
}

void typecheckPop(Parser *parser) {
    size_t s = parser->compiler->expressionTypeStack.size();
    if (s == 0) {
        parser->error("Expected at least 1 type on the stack.");
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
    int s = parser->compiler->expressionTypeStack.size();
    if (s != 0) {
        parser->error("There was " + std::to_string(s) + " types at the end of the statement but there should be just 1. This is indicative of a compiler error mishandling the type stack");
        parser->compiler->expressionTypeStack.clear();
    }
}

void typecheckReverseLastNumTypes(Parser *parser, size_t num) {
    // :ReverseStack
    std::reverse(parser->compiler->expressionTypeStack.end() - num, parser->compiler->expressionTypeStack.end());
}

void typecheckIfCondition(Parser *parser) {
    // Anything can work here?
    typecheckPop(parser);
}

void typecheckVarDeclarationInitializer(Parser *parser, Local &local) {
    Type initializeType = parser->compiler->expressionTypeStack.back();
    parser->compiler->expressionTypeStack.pop_back();
    if (local.type == types::Void || local.type == types::Unknown) { // TODO: Why void?
        local.type = initializeType;
    }
    if (local.type == types::Lambda) {
        local.isStatic = true;
    }
    if (!typecheckIsAssignable(parser, local.type, initializeType)) {
        parser->error("Cannot declare a variable with a different type.");
    }
}

void typecheckLambda(Parser *parser) {
    parser->compiler->expressionTypeStack.push_back(types::Lambda);
}
void typecheckCallLambda(Parser *parser) {
    assert(parser->compiler->expressionTypeStack.size());
    Type backType = parser->compiler->expressionTypeStack.back();

    // types::Lambda is effectively a function that hasn't been
    // instantiated yet. Let's see if this type should extend to
    // regular functions too.
    if (backType != types::Lambda) {
        parser->error("Can't call this type of expression.");
    }
}

void typecheckNil(Parser *parser, Type type) {
    if (type == types::Void) {
        return;
    }
    if (typecheckIsAssignable(parser, type, types::Void)) {
        parser->error("Cannot assign nil to this type.");
    }
}

void typecheckPropertyAccess(Parser *parser, const std::string_view& propertyName) {
    Type mainType = parser->compiler->expressionTypeStack.back();
    parser->compiler->expressionTypeStack.pop_back();

    if (mainType->isArray() && propertyName == "len") {
        parser->compiler->expressionTypeStack.push_back(types::Number);
        return;
    }
    if (!mainType->isStruct()) {
        parser->error("Cannot access a non-struct object.");
        return;
    }

    StructMember *foundMember = nullptr;
    for (auto &member : mainType->structTypeData()->members) {
        if (member.name == propertyName) {
            foundMember = &member;
            break;
        }
    }

    if (!foundMember) {
        parser->error("Property does not exist on type.");
        return;
    }

    parser->compiler->expressionTypeStack.push_back(foundMember->type);
}
void typecheckAssignExpression(Parser *parser) {
    Type b = parser->compiler->expressionTypeStack.back();
    parser->compiler->expressionTypeStack.pop_back();
    Type a = parser->compiler->expressionTypeStack.back();
    if (!typecheckIsAssignable(parser, a, b)) {
        parser->error("Cannot assign a different type.");
    }
    // Leaves a on the stack
}

void typecheckNumber(Parser *parser) {
    parser->compiler->expressionTypeStack.push_back(types::Number);
}

void typecheckArrayAccess(Parser *parser) {
    Type indexType = parser->compiler->expressionTypeStack.back();
    parser->compiler->expressionTypeStack.pop_back();
    if (!typecheckIsAssignable(parser, indexType, types::Number)) {
        parser->error("Can only index with a number.");
        return;
    }

    Type arrayType = parser->compiler->expressionTypeStack.back();
    parser->compiler->expressionTypeStack.pop_back();
    if (!arrayType->isArray()) {
        parser->error("Can only index an array.");
        return;
    }


    parser->compiler->expressionTypeStack.push_back(arrayType->arrayTypeData()->elementType);
}

bool typecheckIsBinaryStrings(Parser *parser) {
    Compiler *compiler = parser->compiler;
    Type typeB = compiler->expressionTypeStack.back();
    Type typeA = compiler->expressionTypeStack.back();
    
    return typeA == types::String && typeB == types::String;
}

void typecheckBinary(Parser *parser, TokenType operatorType) {
    assert(parser->compiler->expressionTypeStack.size() >= 2);
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
        // TODO: highlight the proper token here
        if (typeA == types::Void) {
            parser->error("Type cannot be void of left-hand side of operator.");
        }
        if (typeB == types::Void) {
            parser->error("Type cannot be void of right-hand side of operator.");
        }
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
    switch (parser->previous().type) {
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

void typecheckVariable(Parser *parser, Local *local) {
    parser->compiler->expressionTypeStack.push_back(local->type);
}


void typecheckParameter(Parser *parser, Type functionType, Type argumentType) {
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

void typecheckFunctionDeclarationReturnValue(Parser *parser, ObjFunction *function, Type functionType, Type returnType) {
    auto functionTypeObj = functionType->functionTypeData();
    function->type = functionType;
    functionTypeObj->returnType = returnType;
    // if (returnType != TypeId::Void) {
    function->returnSlots = slotSizeOfType(returnType);
    // } else {
    //     function->returnSlots = 0;
    // }
}

void typecheckInstantiationFromArgumentList(Parser *parser, FunctionInstantiation *functionInst) {
    assert(!functionInst->declaration->polymorphic);

    for (size_t i = 0; i < functionInst->declaration->parameters.size(); i++) {
        Type argumentType = functionInst->declaration->parameters[i].type;
        typecheckParameter(parser, functionInst->type, argumentType);
    }
}

void typecheckInstantiationAgainstStack(Parser *parser, FunctionInstantiation *functionInst, int argCount) {
    size_t end = parser->compiler->expressionTypeStack.size();
    for (size_t i = 0; i < functionInst->declaration->parameters.size(); i++) {
        Type argumentType = parser->compiler->expressionTypeStack[end - argCount + i];
        typecheckParameter(parser, functionInst->type, argumentType);
    }
}

Type getFunctionType(Parser *parser) {
    return parser->compiler->expressionTypeStack.back();
}

void typecheckFunctionArgument(Parser *parser, FunctionDeclaration *functionDeclaration, int argIndex) {
    if ((size_t)argIndex >= functionDeclaration->parameters.size()) {
        parser->error("This function takes up to " + 
            std::to_string(functionDeclaration->parameters.size()) +
            " parameters");
        return;
    }
    Type argumentType = parser->compiler->expressionTypeStack.back();
    if (functionDeclaration->parameters[argIndex].type == types::Unknown) {
        // will be type checked later
        return;
    }
    if (!typecheckIsAssignable(parser, functionDeclaration->parameters[argIndex].type, argumentType)) {
        parser->error("Type mismatch");
    }
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
    auto functionTypeObj = functionType->functionTypeData();
    parser->compiler->expressionTypeStack.push_back(functionTypeObj->returnType);
}

void typecheckReturn(Parser *parser, Compiler *functionCompiler) {
    Type returnStatementType = parser->compiler->expressionTypeStack.back();
    if (!typecheckIsAssignable(parser, functionCompiler->returnType, returnStatementType)) {
        parser->error("Type mismatch");
    }
    typecheckEndStatement(parser);
}
void typecheckReturnNil(Parser *parser, Compiler *functionCompiler) {
    if (!typecheckIsAssignable(parser, functionCompiler->returnType, types::Void)) {
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