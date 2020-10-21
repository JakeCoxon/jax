namespace TypeId {
    int Void = 0;
    int Number = 1;
    int Bool = 2;
    int String = 3;
    int Dynamic = 4;
    int Unknown = 5;
    int Function = 6;
}

int typeByName(Parser *parser, const std::string_view &name) {
    int type = TypeId::Void;
    if (name == "void") {
        type = TypeId::Void;
    } else if (name == "number") {
        type = TypeId::Number;
    } else if (name == "bool") {
        type = TypeId::Bool;
    } else if (name == "string") {
        type = TypeId::String;
    } else if (name == "dynamic") {
        type = TypeId::Dynamic;
    } else {
        std::string err = "Unknown type: ";
        err += name;
        parser->error(err);
    }
    return type;
}

int addNewType(Parser *parser, GenericType genericType) {
    parser->types.push_back(genericType);
    return parser->types.size() - 1;
}

void typecheckInit(Parser *parser) {
    parser->types.push_back(GenericType{"void"}); // Void
    parser->types.push_back(GenericType{"number"}); // Number
    parser->types.push_back(GenericType{"bool"}); // Bool
    parser->types.push_back(GenericType{"string"}); // String
    parser->types.push_back(GenericType{"dynamic"}); // Dynamic
    parser->types.push_back(GenericType{"unknown"}); // Unknown
    parser->types.push_back(GenericType{"function"}); // Function placeholder
}

void typecheckPop(Parser *parser) {
    if (parser->compiler->expressionTypeStack.size() == 0) {
        parser->error("Type stack was 0.");
    } else {
        parser->compiler->expressionTypeStack.pop_back();
    }
}

bool typecheckIsAssignable(Parser *parser, int typeA, int typeB) {
    if (typeA == TypeId::Dynamic) {
        return true;    
    }
    if (typeB == TypeId::Void) {
        return (typeA != TypeId::Number && typeA != TypeId::Bool);
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

void typecheckVarDeclaration(Parser *parser, int type) {
    int backType = parser->compiler->expressionTypeStack.back();
    parser->compiler->expressionTypeStack.pop_back();
    if (type == -1) {
        type = backType;
    }
    if (!typecheckIsAssignable(parser, type, backType)) {
        parser->error("Cannot declare a variable with a different type.");
    }
    parser->compiler->locals.back().type = type;
}

void typecheckNil(Parser *parser, int type) {
    if (type == -1) {
        return;
    }
    if (typecheckIsAssignable(parser, type, TypeId::Void)) {
        parser->error("Cannot assign nil to this type.");
    }
}

void typecheckNumber(Parser *parser) {
    parser->compiler->expressionTypeStack.push_back(TypeId::Number);
}

void typecheckBinary(Parser *parser, TokenType operatorType) {
    Compiler *compiler = parser->compiler;
    int typeB = compiler->expressionTypeStack.back();
    compiler->expressionTypeStack.pop_back();
    int typeA = compiler->expressionTypeStack.back();
    compiler->expressionTypeStack.pop_back();

    auto assertNumbers = [&]() {
        if (typeA != TypeId::Number || typeB != TypeId::Number) { 
            parser->error("Operator expects two numbers.");
        }
    };

    auto assertEqual = [&]() {
        if (typeA != typeB) { parser->error("Types don't match"); }
    };

    int resultType;
    switch (operatorType) {
        case TokenType::BangEqual:    assertEqual();   resultType = TypeId::Bool; break;
        case TokenType::EqualEqual:   assertEqual();   resultType = TypeId::Bool; break;
        case TokenType::Greater:      assertNumbers(); resultType = TypeId::Bool; break;
        case TokenType::GreaterEqual: assertNumbers(); resultType = TypeId::Bool; break;
        case TokenType::Less:         assertNumbers(); resultType = TypeId::Bool; break;
        case TokenType::LessEqual:    assertNumbers(); resultType = TypeId::Bool; break;
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
            parser->compiler->expressionTypeStack.push_back(TypeId::Bool);
            break;
        case TokenType::True:
            parser->compiler->expressionTypeStack.push_back(TypeId::Bool);
            break;
        case TokenType::Nil:
            parser->compiler->expressionTypeStack.push_back(TypeId::Void);
            break;
        default: return;
    }
}

void typecheckString(Parser *parser) {
    parser->compiler->expressionTypeStack.push_back(TypeId::String);
}

void typecheckAnd(Parser *parser) {
    typecheckPop(parser);
    typecheckPop(parser);
    parser->compiler->expressionTypeStack.push_back(TypeId::Bool);
}

void typecheckOr(Parser *parser) {
    typecheckPop(parser);
    typecheckPop(parser);
    parser->compiler->expressionTypeStack.push_back(TypeId::Bool);
}

void typecheckAssign(Parser *parser, int local) {
    int a = parser->compiler->locals[local].type;
    int b = parser->compiler->expressionTypeStack.back();
    if (!typecheckIsAssignable(parser, a, b)) {
        parser->error("Cannot assign a different type.");
    }
}

void typecheckVariable(Parser *parser, int local) {
    int type = parser->compiler->locals[local].type;
    parser->compiler->expressionTypeStack.push_back(type);
}


void typecheckParameter(Parser *parser, ObjFunction *function, int functionType, int argumentType) {
    parser->compiler->locals.back().type = argumentType;
    auto functionTypeObj = &mpark::get<FunctionTypeObj>(parser->types[functionType]);
    functionTypeObj->parameterTypes.push_back(argumentType);
}

int typecheckFunctionDeclaration(Parser *parser, ObjFunction *function) {
    parser->types.push_back(FunctionTypeObj {});
    int functionType = parser->types.size() - 1;
    // parser->compiler->locals.back().type = functionType;
    return functionType;
}

void typecheckFunctionDeclarationReturn(Parser *parser, ObjFunction *function, int functionType, int returnType) {
    auto functionTypeObj = &mpark::get<FunctionTypeObj>(parser->types[functionType]);
    function->type = functionType;
    functionTypeObj->returnType = returnType;
}

int getFunctionType(Parser *parser) {
    return parser->compiler->expressionTypeStack.back();
}

void typecheckFunctionArgument(Parser *parser, FunctionDeclaration *functionDeclaration, int argIndex) {
    int argumentType = parser->compiler->expressionTypeStack.back();
    if (functionDeclaration->parameters[argIndex].type == TypeId::Unknown) {
        // will be type checked later
        return;
    }
    if (!typecheckIsAssignable(parser, functionDeclaration->parameters[argIndex].type, argumentType)) {
        parser->error("Type mismatch");
    }
}

void typecheckBeginFunctionCall(Parser *parser, ObjFunction *function) {
    parser->compiler->expressionTypeStack.push_back(0);
}
void typecheckEndFunctionCall(Parser *parser, Value function, int argCount) {
    for (int i = 0; i < argCount; i++) {
        typecheckPop(parser);
    }
    int functionType = function.visit(overloaded {
        [&](ObjFunction *f) -> int { return f->type; },
        [&](ObjNative *f) -> int { return f->type; },
        [&](auto value) -> int {
            parser->error("Invalid function type.");
            return 0;
        }
    });
    parser->compiler->expressionTypeStack.pop_back();
    auto functionTypeObj = &mpark::get<FunctionTypeObj>(parser->types[functionType]);
    parser->compiler->expressionTypeStack.push_back(functionTypeObj->returnType);
}

void typecheckUpdateFunctionInstantiation(Parser *parser, int functionType, int argCount) {
    auto &arg = parser->compiler->expressionTypeStack[parser->compiler->expressionTypeStack.size() - 1 - argCount];
    arg = functionType;
}

void typecheckReturn(Parser *parser, ObjFunction *function) {
    int returnStatementType = parser->compiler->expressionTypeStack.back();
    auto functionTypeObj = &mpark::get<FunctionTypeObj>(parser->types[function->type]);
    if (!typecheckIsAssignable(parser, functionTypeObj->returnType, returnStatementType)) {
        parser->error("Type mismatch");
    }
    typecheckEndStatement(parser);
}
void typecheckReturnNil(Parser *parser, ObjFunction *function) {
    int returnStatementType = parser->compiler->expressionTypeStack.back();
    auto functionTypeObj = &mpark::get<FunctionTypeObj>(parser->types[function->type]);
    if (!typecheckIsAssignable(parser, functionTypeObj->returnType, TypeId::Void)) {
        parser->error("Type mismatch");
    }
}

struct PrintState {
    int initialConstantIndex;
    int expectedVarargs = 0;
};

PrintState typecheckPrintBegin(Parser *parser) {
    PrintState ps;
    ps.initialConstantIndex = parser->currentChunk().constants.size() - 1;
    return ps;
}

void typecheckPrintArgument(Parser *parser, PrintState *printState, int argIndex) {
    if (argIndex > 0) {
        // All types can be printed
        return;
    }

    int firstArgType = parser->compiler->expressionTypeStack.back();
    if (firstArgType != TypeId::String) {
        parser->error("Print expects a constant string as the first argument.");
        return;
    }

    if ((int)parser->currentChunk().constants.size() != printState->initialConstantIndex + 2) {
        parser->error("Print expects a constant string as the first argument.");
        return;
    }

    Value stringConstant = parser->currentChunk().constants.back();
    if (!stringConstant.isString()) {
        parser->error("Print expects a constant string as the first argument.");
        return;
    }

    ObjString s = stringConstant.asString();
    for (size_t i = 0; i < s.text.size(); i++) {
        if (s.text[i] == '{') {
            i ++;
            if (i >= s.text.size() || s.text[i] != '}') {
                parser->error("Invalid syntax in print string");
                return;
            }
            printState->expectedVarargs ++;
        }
    }
}

void typecheckPrintEnd(Parser *parser, PrintState *printState, int argCount) {
    if (printState->expectedVarargs != argCount - 1) {
        parser->error("Invalid number of arguments to print statement.");
    }
    for (int i = 0; i < argCount - 1; i++) {
        typecheckPop(parser);
    }
    typecheckEndStatement(parser);
}