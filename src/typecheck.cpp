namespace Type {
    int Nil = 0;
    int Number = 1;
    int Bool = 2;
    int String = 3;
    int Function = 4;
}

int typeByName(Parser *parser, const std::string_view &name) {
    int type = Type::Nil;
    if (name == "number") {
        type = Type::Number;
    } else if (name == "bool") {
        type = Type::Bool;
    } else if (name == "string") {
        type = Type::String;
    } else if (name == "function") {
        type = Type::Function;
    } else {
        parser->error("Unknown type");
    }
    return type;
}

void typecheckVarDeclaration(Parser *parser, int type) {
    int backType = parser->compiler->expressionTypeStack.back();
    if (type == -1) {
        type = backType;
    }
    if (backType != type) {
        parser->error("Cannot declare a variable with a different type.");
    }
    parser->compiler->locals.back().type = type;
}

void typecheckNumber(Parser *parser) {
    parser->compiler->expressionTypeStack.push_back(Type::Number);
}

void typecheckBinary(Parser *parser, TokenType operatorType) {
    Compiler *compiler = parser->compiler;
    int typeA = compiler->expressionTypeStack.back();
    compiler->expressionTypeStack.pop_back();
    int typeB = compiler->expressionTypeStack.back();
    compiler->expressionTypeStack.pop_back();

    auto assertNumbers = [&]() -> void {
        if (typeA != Type::Number && typeB != Type::Number) { 
            parser->error("Types don't match");
        }
    };

    switch (operatorType) {
        case TokenType::BangEqual:
            if (typeA != typeB) { parser->error("Types don't match"); }
            compiler->expressionTypeStack.push_back(Type::Bool);
            break;
        case TokenType::EqualEqual:
            if (typeA != typeB) { parser->error("Types don't match"); }
            compiler->expressionTypeStack.push_back(Type::Bool);
            break;
        case TokenType::Greater:
            assertNumbers();
            compiler->expressionTypeStack.push_back(Type::Bool);
            break;
        case TokenType::GreaterEqual:
            assertNumbers();
            compiler->expressionTypeStack.push_back(Type::Bool);
            break;
        case TokenType::Less:
            assertNumbers();
            compiler->expressionTypeStack.push_back(Type::Bool);
            break;
        case TokenType::LessEqual:
            assertNumbers();
            compiler->expressionTypeStack.push_back(Type::Bool);
            break;
        case TokenType::Plus:
            if (typeA != typeB) { parser->error("Types don't match"); }
            compiler->expressionTypeStack.push_back(typeA);
            break;
        case TokenType::Minus:
            if (typeA != typeB) { parser->error("Types don't match"); }
            compiler->expressionTypeStack.push_back(typeA);
            break;
        case TokenType::Star:
            if (typeA != typeB) { parser->error("Types don't match"); }
            compiler->expressionTypeStack.push_back(typeA);
            break;
        case TokenType::Slash:
            if (typeA != typeB) { parser->error("Types don't match"); }
            compiler->expressionTypeStack.push_back(typeA);
            break;
        default: break;
    }
}

void typecheckLiteral(Parser *parser) {
    switch (parser->previous.type) {
        case TokenType::False:
            parser->compiler->expressionTypeStack.push_back(Type::Bool);
            break;
        case TokenType::True:
            parser->compiler->expressionTypeStack.push_back(Type::Bool);
            break;
        case TokenType::Nil:
            parser->compiler->expressionTypeStack.push_back(Type::Nil);
            break;
        default: return;
    }
}

void typecheckString(Parser *parser) {
    parser->compiler->expressionTypeStack.push_back(Type::String);
}

void typecheckAnd(Parser *parser) {
    parser->compiler->expressionTypeStack.pop_back();
    parser->compiler->expressionTypeStack.pop_back();
    parser->compiler->expressionTypeStack.push_back(Type::Bool);
}

void typecheckOr(Parser *parser) {
    parser->compiler->expressionTypeStack.pop_back();
    parser->compiler->expressionTypeStack.pop_back();
    parser->compiler->expressionTypeStack.push_back(Type::Bool);
}

void typecheckAssign(Parser *parser, int local) {
    int a = parser->compiler->locals[local].type;
    int b = parser->compiler->expressionTypeStack.back();
    if (a != b) {
        parser->error("Cannot assign a different type.");
    }
}

void typecheckVariable(Parser *parser, int local) {
    int type = parser->compiler->locals[local].type;
    parser->compiler->expressionTypeStack.push_back(type);
}