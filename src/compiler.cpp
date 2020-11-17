#include <vector>
#include <string>

#define UINT8_COUNT (UINT8_MAX + 1)

enum class Precedence {
    None,
    Assignment,  // =
    Or,          // or
    And,         // and
    Equality,    // == !=
    Comparison,  // < > <= >=
    Term,        // + -
    Factor,      // * /
    Unary,       // ! -
    Call,        // . ()
    Primary
};

enum class FunctionType {
    Function, Script,
};

struct Local {
    std::string_view name;
    int type = 0;
    int depth;

    Local(const std::string_view &name, int depth):
        name(name), depth(depth) {};
};

struct Compiler;

struct FunctionParameter {
    std::string_view name;
    int type;
};

struct FunctionInstantiation {
    int type;
    Value function = mpark::monostate();
    Compiler *compiler = nullptr;
};

struct FunctionDeclaration {
    std::string_view name;
    std::vector<FunctionParameter> parameters;
    int returnType;
    bool polymorphic;

    Compiler *enclosingCompiler;
    std::vector<FunctionInstantiation> overloads;
    
    size_t constant;
    size_t blockStart;
    int blockLine;
};

struct Parser;

struct Compiler {
    ObjFunction *function;
    FunctionType type;
    Compiler *enclosing;
    std::vector<Local> locals;
    std::vector<int> expressionTypeStack;
    std::vector<FunctionDeclaration> functionDeclarations;

    int scopeDepth = 0;
    bool compiled = false;

    Compiler(ObjFunction *function, FunctionType type, Compiler *enclosing)
            : function(function), type(type), enclosing(enclosing) {
    }

    int resolveLocal(const std::string_view &name);
    FunctionDeclaration *resolveFunctionDeclaration(const std::string_view &name);
    void declareVariable(Parser* parser, const std::string_view& name);
    void markInitialized();
};

struct ExpressionState {
    bool canAssign;
};

struct GenericType {
    std::string name;
};
struct FunctionTypeObj {
    std::vector<int> parameterTypes;
    int returnType = -1;
};
using Type = mpark::variant<
    GenericType, FunctionTypeObj
>;

struct Parser {
    Token current;
    Token previous;
    Scanner *scanner;
    Compiler *compiler;

    std::vector<Type> types;

    bool hadError = false;
    bool panicMode = false;

    Parser(Scanner *scanner, Compiler *compiler):
            scanner(scanner), compiler(compiler) {
        initCompiler(compiler);
    };

    void advance();
    bool match(TokenType type);
    bool check(TokenType type);
    void consume(TokenType type, const std::string &message);
    void consumeEndStatement(const std::string &message);
    void errorAt(Token &token, const std::string &message);
    uint16_t makeConstant(Value value);
    void parsePrecedence(Precedence precedence);
    void parseVariable(const std::string &errorMessage);
    uint16_t identifierConstant(const std::string_view &name);
    void synchronize();
    void initCompiler(Compiler *compiler);
    ObjFunction *endCompiler();
    Value maybeCompileFunctionInstantiation(FunctionDeclaration *functionDeclaration, int argCount);
    void beginScope();
    void endScope();

    void emitReturn();
    void emitConstant(Value value);
    int emitJump(OpCode instruction);
    void patchJump(int offset);
    void emitLoop(int loopStart);

    void declaration();
    void statement();
    void printStatement();
    void returnStatement();
    void expressionStatement();
    void ifStatement();
    void whileStatement();
    void varDeclaration();
    void expression();
    void block();
    void funDeclaration();

    void number(ExpressionState es);
    void grouping(ExpressionState es);
    void unary(ExpressionState es);
    void binary(ExpressionState es);
    void literal(ExpressionState es);
    void string(ExpressionState es);
    void namedVariable(const std::string_view &name, ExpressionState es);
    void variable(ExpressionState es);
    void and_(ExpressionState es);
    void or_(ExpressionState es);
    uint8_t argumentList(FunctionDeclaration *functionDeclaration);
    void callFunction(FunctionDeclaration *functionDeclaration);

    Chunk &currentChunk() { 
        return compiler->function->chunk;
    }
    void emitByte(uint8_t byte) {
        currentChunk().write(byte, previous.line);
    }
    void emitByte(OpCode opcode) {
        emitByte(static_cast<uint8_t>(opcode));
    }
    void emitTwoBytes(uint16_t bytes) {
        currentChunk().write((bytes << 8) & 0xff, previous.line);
        currentChunk().write(bytes & 0xff, previous.line);
    }

    void errorAtCurrent(const std::string &message) {
        errorAt(current, message);
    }
    void error(const std::string &message) {
        errorAt(previous, message);
    }
};

#include "typecheck.cpp"

using ParseFn = void (Parser::*)(ExpressionState);

struct ParseRule {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
};

static ParseRule &getRule(TokenType type);

void registry(Parser *parser);

void registerNative(
        Parser *parser, std::string name, int returnType,
        std::vector<FunctionParameter> parameters, NativeFn nativeFn) {
    FunctionTypeObj type{};
    for (size_t i = 0; i < parameters.size(); i++) {
        type.parameterTypes.push_back(parameters[i].type);
    }
    type.returnType = returnType;
    parser->types.push_back(type);
    int typeId = parser->types.size() - 1;

    // TODO: garbage collection
    auto native = new ObjNative{typeId, nativeFn};
    // TODO: garbage collection
    auto functionName = new ObjString{name};
    auto inst = FunctionInstantiation{typeId, native};
    parser->compiler->functionDeclarations.push_back({
        functionName->text, parameters, returnType, false, nullptr, {inst}, 0, 0, 0
    });
}

ObjFunction *compile(const std::string &source) {
    Scanner scanner { source };
    auto function = new ObjFunction();
    Compiler compiler(function, FunctionType::Script, nullptr);
    Parser parser { &scanner, &compiler };
    typecheckInit(&parser);

    registry(&parser);


    parser.advance();

    while (!parser.match(TokenType::EOF_)) {
        while (parser.match(TokenType::Newline)) {}
        parser.declaration();
        while (parser.match(TokenType::Newline)) {}
    }

    parser.endCompiler();
    return parser.hadError ? nullptr : function;

}

int Compiler::resolveLocal(const std::string_view &name) {
    for (int i = locals.size() - 1; i >= 0; i--) {
        Local* local = &locals[i];
        if (name == local->name) {
            if (local->depth == -1) {
                return -2;
            }
            return i;
        }
    }

    return -1;
}

FunctionDeclaration *Compiler::resolveFunctionDeclaration(const std::string_view &name) {
    for (size_t i = 0; i < functionDeclarations.size(); i++) {
        if (functionDeclarations[i].name == name) {
            return &functionDeclarations[i];
        }
    }
    if (enclosing) { return enclosing->resolveFunctionDeclaration(name); }
    return nullptr;
}

void Compiler::declareVariable(Parser *parser, const std::string_view& name) {
    for (int i = locals.size() - 1; i >= 0; i--) {
        Local* local = &locals[i];
        if (local->depth != -1 && local->depth < scopeDepth) {
            break; 
        }

        if (name == local->name) {
            parser->error("Already variable with this name in this scope.");
        }
    }
    if (locals.size() == UINT8_COUNT) {
        parser->error("Too many local variables on the stack.");
    }
    locals.push_back(Local { name, -1 });
}

void Compiler::markInitialized() {
    locals.back().depth = scopeDepth;
}

void Parser::advance() {
    previous = current;
    while (true) {
        current = scanner->scanToken();
        if (current.type == TokenType::Error) {
            errorAtCurrent(std::string(current.text));
            continue;
        }
        break;
    }
}


bool Parser::match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}
bool Parser::check(TokenType type) {
    return current.type == type;
}

void Parser::consume(TokenType type, const std::string &message) {
    if (current.type != type) {
        errorAtCurrent(message);
        return;
    }
    advance();
}

void Parser::consumeEndStatement(const std::string &message) {
    if (current.type == TokenType::Semicolon || current.type == TokenType::Newline) {
        advance();
        return;
    }
    if (current.type == TokenType::EOF_) {
        return;
    }
    
    errorAtCurrent(message);
}

void Parser::errorAt(Token &token, const std::string &message) {
    if (panicMode) return;
    panicMode = true;
    hadError = true;
    std::cerr << "[line " << token.line << "] Error";

    if (token.type == TokenType::EOF_) {
        std::cerr << " at end: " << message << std::endl;
    } else if (token.type == TokenType::Error) {
        // Nothing
    } else {
        std::cerr << ": " << message << std::endl;

        // Use a cursor to work backwards to find the line that
        // the token is on and then print the line
        std::cerr << std::endl;
        size_t cursor = token.start;
        while (cursor == 1 || (cursor > 1 && scanner->source[cursor - 1] != '\n')) {
            cursor--;
        }
        size_t offsetInLine = token.start - cursor;
        while (cursor < scanner->source.size()  && scanner->source[cursor] != '\n') {
            std::cerr << scanner->source[cursor];
            cursor++;
        }
        std::cerr << std::endl;
        for (size_t i = 0; i < offsetInLine; i++) std::cerr << ' ';
        for (size_t i = 0; i < fmax(token.text.size(), 1); i++) std::cerr << '^';
        std::cerr << std::endl;
        std::cerr << std::endl;
    }
}

uint16_t Parser::makeConstant(Value value) {
    int constant = currentChunk().addConstant(value);
    // if (constant > UINT8_MAX) {
    //     error("Too many constants in one chunk.");
    //     return 0;
    // }
    return (uint16_t)constant;
}

void Parser::synchronize() {
    panicMode = false;

    while (current.type != TokenType::EOF_) {
        if (previous.type == TokenType::Semicolon ||
            previous.type == TokenType::Newline) return;

        switch (current.type) {
            case TokenType::Class:
            case TokenType::Fun:
            case TokenType::Var:
            case TokenType::For:
            case TokenType::If:
            case TokenType::While:
            case TokenType::Print:
            case TokenType::Return:
                return;
            default: break;
        }
        advance();
    }
}

void Parser::initCompiler(Compiler *compiler) {
    this->compiler = compiler;
    // TODO: Garbage collection
    if (compiler->type != FunctionType::Script) {
        compiler->function->name = new ObjString(std::string(previous.text));
    }
    compiler->locals.push_back(Local("", 0));

}


ObjFunction *Parser::endCompiler() {
    emitReturn();

    ObjFunction *function = compiler->function;
#ifdef DEBUG_PRINT_CODE
    if (!hadError) {
        disassembleChunk(currentChunk(), function->name != nullptr
            ? function->name->text : "<script>");
    }
#endif
    compiler->compiled = true;
    compiler = compiler->enclosing;

    return function;
}


Value Parser::maybeCompileFunctionInstantiation(FunctionDeclaration *functionDeclaration, int argCount) {
    Compiler *initialCompiler = this->compiler;
    Scanner *initialScanner = this->scanner;

    // slow as hell
    for (size_t j = 0; j < functionDeclaration->overloads.size(); j++) {
        auto inst = &functionDeclaration->overloads[j];
        auto functionTypeObj = &mpark::get<FunctionTypeObj>(types[inst->type]);

        bool isMatched = true;
        for (size_t i = 0; i < functionDeclaration->parameters.size(); i++) {
            size_t end = initialCompiler->expressionTypeStack.size();
            int argumentType = initialCompiler->expressionTypeStack[end - argCount + i];
            if (!typecheckIsAssignable(this, functionTypeObj->parameterTypes[i], argumentType)) {
                isMatched = false; break;
            }
        }
        if (isMatched) return inst->function;
    }

    Scanner tempScanner { initialScanner->source };
    scanner = &tempScanner;
    
    Token _previous = previous;
    Token _current = current;

    scanner->current = functionDeclaration->blockStart;
    scanner->start = functionDeclaration->blockStart;
    scanner->line = functionDeclaration->blockLine;
    scanner->parens = 0;
    advance();

    // TODO: Garbage collection
    auto newFunction = new ObjFunction();
    Compiler *compiler = new Compiler(newFunction, FunctionType::Function, functionDeclaration->enclosingCompiler);
    auto functionType = typecheckFunctionDeclaration(this, newFunction);

    this->compiler = compiler;
    compiler->compiled = true;

    functionDeclaration->overloads.push_back({
        functionType, newFunction, compiler
    });

    beginScope();
    // TODO: Garbage collection
    compiler->function->name = new ObjString(std::string(functionDeclaration->name));
    compiler->function->arity = functionDeclaration->parameters.size();
    compiler->locals.push_back(Local("", 0)); // Function object

    
    for (size_t i = 0; i < functionDeclaration->parameters.size(); i++) {
        compiler->declareVariable(this, functionDeclaration->parameters[i].name);
        compiler->markInitialized();
        size_t end = initialCompiler->expressionTypeStack.size();
        int argumentType = initialCompiler->expressionTypeStack[end - argCount + i];
        typecheckParameter(this, newFunction, functionType, argumentType);
    }

    typecheckFunctionDeclarationReturn(this, newFunction, functionType, functionDeclaration->returnType);

    block();
    endCompiler();
    typecheckUpdateFunctionInstantiation(this, newFunction->type, argCount);

    // Reset back
    previous = _previous;
    current = _current;
    this->scanner = initialScanner;
    this->compiler = initialCompiler;
    return newFunction;
}

void Parser::beginScope() {
    compiler->scopeDepth ++;
}

void Parser::endScope() {
    compiler->scopeDepth --;
    while (compiler->locals.size() > 0 &&
            compiler->locals.back().depth >
            compiler->scopeDepth) {
        emitByte(OpCode::Pop);
        compiler->locals.pop_back();
    }
}

void Parser::emitReturn() {
    emitByte(OpCode::Nil);
    emitByte(OpCode::Return);
}

void Parser::emitConstant(Value value) {
    uint16_t constantIndex = makeConstant(value);
    if (constantIndex < 256) {
        emitByte(OpCode::Constant); emitByte(constantIndex);
    } else {
        assert(false); // Not implemented yet. Use a wide opcode
    }
    // emitByte(OpCode::Constant); emitTwoBytes(constantIndex);
}


int Parser::emitJump(OpCode instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk().code.size() - 2;
}

void Parser::patchJump(int offset) {
    int jump = currentChunk().code.size() - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
        return;
    }

    currentChunk().code[offset] = (jump >> 8) & 0xff;
    currentChunk().code[offset + 1] = jump & 0xff;
}

void Parser::emitLoop(int loopStart) {
    emitByte(OpCode::Loop);

    int offset = currentChunk().code.size() - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

void Parser::parsePrecedence(Precedence precedence) {
    advance();

    ParseFn prefixRule = getRule(previous.type).prefix;
    if (!prefixRule) {
        error("Expect expression.");
        return;
    }

    ExpressionState es;
    es.canAssign = precedence <= Precedence::Assignment;
    
    (this->*prefixRule)(es);

    while (precedence <= getRule(current.type).precedence) {
        advance();
        ParseFn infixRule = getRule(previous.type).infix;
        (this->*infixRule)(es);
    }

    if (es.canAssign && match(TokenType::Equal)) {
        error("Invalid assignment target.");
    }
}


void Parser::parseVariable(const std::string &errorMessage) {
    consume(TokenType::Identifier, errorMessage);
    compiler->declareVariable(this, previous.text);
}

uint16_t Parser::identifierConstant(const std::string_view &name) {
    // TODO: Garbage collection
    auto objStr = new ObjString(std::string(name));
    return makeConstant(objStr);
}

void Parser::declaration() {
    if (match(TokenType::Fun)) {
        funDeclaration();
    } else if (match(TokenType::Var)) {
        varDeclaration();
    } else {
        statement();
    }

    if (panicMode) synchronize();
}

void Parser::statement() {
    if (match(TokenType::Print)) {
        printStatement();
    } else if (match(TokenType::If)) {
        ifStatement();
    } else if (match(TokenType::Return)) {
        returnStatement();
    } else if (match(TokenType::While)) {
        whileStatement();
    } else if (match(TokenType::LeftBrace)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

void Parser::printStatement() {

    auto printState = typecheckPrintBegin(this);
    uint8_t argCount = 0;
    consume(TokenType::LeftParen, "Expect '(' after print.");
    if (!check(TokenType::RightParen)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't hve more than 255 arguments.");
            }
            typecheckPrintArgument(this, &printState, argCount);
            argCount ++;
        } while (match(TokenType::Comma));
    }
    consume(TokenType::RightParen, "Expect ')' after arguments.");
    consumeEndStatement("Expect ';' or newline after value.");
    
    typecheckPrintEnd(this, &printState, argCount);
    emitByte(OpCode::Print);
    emitByte(argCount);
}

void Parser::returnStatement() {
    if (compiler->type == FunctionType::Script) {
        error("Can't return from top-level code.");
    }
    if (match(TokenType::Semicolon) || match(TokenType::Newline)) {
        emitReturn();
        typecheckReturnNil(this, compiler->function);
    } else {
        expression();
        consumeEndStatement("Expect ';' or newline after return value");
        emitByte(OpCode::Return);
        typecheckReturn(this, compiler->function);
    }

}

void Parser::expressionStatement() {
    expression();
    consumeEndStatement("Expect ';' or newline after expression.");
    emitByte(OpCode::Pop);
    typecheckEndStatement(this);
}

void Parser::ifStatement() {
    expression();
    typecheckIfCondition(this);
    consume(TokenType::LeftBrace, "Expect '{' after if.");

    int thenJump = emitJump(OpCode::JumpIfFalse);
    emitByte(OpCode::Pop);
    beginScope();
    block();
    endScope();

    int elseJump = emitJump(OpCode::Jump);

    patchJump(thenJump);
    emitByte(OpCode::Pop);

    if (match(TokenType::Else)) {
        beginScope();
        block();
        endScope();
    }
    patchJump(elseJump);
    consumeEndStatement("Expect ';' or newline after block.");
}

void Parser::whileStatement() {
    int loopStart = currentChunk().code.size();
    expression();
    typecheckIfCondition(this);
    consume(TokenType::LeftBrace, "Expect '{' after condition.");

    int exitJump = emitJump(OpCode::JumpIfFalse);

    emitByte(OpCode::Pop);

    beginScope();
    block();
    endScope();

    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OpCode::Pop);
    consumeEndStatement("Expect ';' or newline after block.");
}

void Parser::varDeclaration() {
    parseVariable("Expect variable name.");

    int type = -1;
    if (match(TokenType::Colon)) {
        consume(TokenType::Identifier, "Expect type name after ':'.");
        type = typeByName(this, previous.text);
    }

    if (match(TokenType::Equal)) {
        expression();
        typecheckVarDeclaration(this, type);
    } else {
        emitByte(OpCode::Nil);
        typecheckNil(this, type);
    }
    consumeEndStatement("Expect ';' or newline after variable declaration.");
    compiler->markInitialized();
}

void Parser::expression() {
    parsePrecedence(Precedence::Assignment);
}

void Parser::block() {
    while (!check(TokenType::RightBrace) && !check(TokenType::EOF_)) {
        while (match(TokenType::Newline)) {}
        declaration();
        while (match(TokenType::Newline)) {}
    }
    consume(TokenType::RightBrace, "Expect '}' after block");
}

void Parser::funDeclaration() {
    consume(TokenType::Identifier, "Expect function name.");
    auto name = previous.text;
    auto function = new ObjFunction();
    Compiler *enclosingCompiler = compiler;
    Compiler *functionCompiler = new Compiler(function, FunctionType::Function, compiler);
    uint16_t constant = makeConstant(function);
    // int functionType = typecheckFunctionDeclaration(this, function);
    // initCompiler(functionCompiler);
    // beginScope();

    std::vector<FunctionParameter> parameters;
    bool polymorphic = false;

    consume(TokenType::LeftParen, "Expect '(' after function name.");
    if (!check(TokenType::RightParen)) {
        do {
            function->arity++;
            if (function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }

            consume(TokenType::Identifier, "Expect parameter name.");
            auto parameterName = previous.text;

            int argumentType = TypeId::Unknown;
            if (match(TokenType::Colon)) {
                consume(TokenType::Identifier, "Expect type name after ':'.");
                argumentType = typeByName(this, previous.text);
            }
            // typecheckParameter(this, function, functionType, argumentType);

            parameters.push_back({ parameterName, argumentType });
            if (argumentType == TypeId::Unknown) {
                polymorphic = true;
            }
        } while (match(TokenType::Comma));
    }
    consume(TokenType::RightParen, "Expect ')' after after parameters.");

    int returnType = TypeId::Void;
    if (match(TokenType::Colon)) {
        consume(TokenType::Identifier, "Expect type name after ':'.");
        returnType = typeByName(this, previous.text);
    }
    // typecheckFunctionDeclarationReturn(this, function, functionType, returnType);

    consume(TokenType::LeftBrace, "Expect '{' before function body.");
    enclosingCompiler->functionDeclarations.push_back(FunctionDeclaration{ 
        name, parameters, returnType, polymorphic, enclosingCompiler, {}, constant, scanner->start, scanner->line
    });

    int braces = 0;
    while ((braces > 0 || !check(TokenType::RightBrace)) && !check(TokenType::EOF_)) {
        if (match(TokenType::LeftBrace)) braces ++;
        else if (match(TokenType::RightBrace)) braces --;
        else advance();
    }
    consume(TokenType::RightBrace, "Expect '}' after block");

    // compiler = compiler->enclosing;
}


void Parser::number(ExpressionState es) {
    // https://stackoverflow.com/questions/11752705/does-stdstring-contain-null-terminator
    // Don't want to risk it - just convert to a new string instead
    double value = strtod(std::string(previous.text).c_str(), nullptr);
    emitConstant(value);
    typecheckNumber(this);
}

void Parser::grouping(ExpressionState es) {
    expression();
    consume(TokenType::RightParen, "Expect ')' after expression.");
}

void Parser::unary(ExpressionState es) {
    TokenType operatorType = previous.type;
    
    expression();

    switch (operatorType) {
        case TokenType::Bang:  emitByte(OpCode::Not); break;
        case TokenType::Minus: emitByte(OpCode::Negate); break;
        default:
            return;
    }
}

void Parser::binary(ExpressionState es) {
    TokenType operatorType = previous.type;
    
    ParseRule &rule = getRule(operatorType);
    int prec = static_cast<int>(rule.precedence);
    parsePrecedence(Precedence(prec + 1)); // +1 because of left associativity

    typecheckBinary(this, operatorType);

    switch (operatorType) {
        case TokenType::BangEqual:     emitByte(OpCode::Equal); emitByte(OpCode::Not); break;
        case TokenType::EqualEqual:    emitByte(OpCode::Equal); break;
        case TokenType::Greater:       emitByte(OpCode::Greater); break;
        case TokenType::GreaterEqual:  emitByte(OpCode::Less); emitByte(OpCode::Not); break;
        case TokenType::Less:          emitByte(OpCode::Less); break;
        case TokenType::LessEqual:     emitByte(OpCode::Greater); emitByte(OpCode::Not); break;
        case TokenType::Plus:          emitByte(OpCode::Add); break;
        case TokenType::Minus:         emitByte(OpCode::Subtract); break;
        case TokenType::Star:          emitByte(OpCode::Multiply); break;
        case TokenType::Slash:         emitByte(OpCode::Divide); break;
        default: break;
    }

}

void Parser::literal(ExpressionState es) {
    typecheckLiteral(this);
    switch (previous.type) {
        case TokenType::False: emitByte(OpCode::False); break;
        case TokenType::True: emitByte(OpCode::True); break;
        case TokenType::Nil: emitByte(OpCode::Nil); break;
        default: return;
    }
}

void Parser::string(ExpressionState es) {
    typecheckString(this);

    auto str = previous.text;
    str.remove_prefix(1);
    str.remove_suffix(1);
    // TODO: Garbage collection
    auto objStr = new ObjString(std::string(str));
    emitConstant(objStr);
}

void Parser::and_(ExpressionState es) {
    int endJump = emitJump(OpCode::JumpIfFalse);

    emitByte(OpCode::Pop);
    parsePrecedence(Precedence::And);

    patchJump(endJump);
    typecheckAnd(this);
}

void Parser::or_(ExpressionState es) {
    int elseJump = emitJump(OpCode::JumpIfFalse);
    int endJump = emitJump(OpCode::Jump);

    patchJump(elseJump);
    emitByte(OpCode::Pop);

    parsePrecedence(Precedence::Or);
    patchJump(endJump);
    typecheckOr(this);
}

uint8_t Parser::argumentList(FunctionDeclaration *functionDeclaration) {
    uint8_t argCount = 0;
    if (!check(TokenType::RightParen)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }
            typecheckFunctionArgument(this, functionDeclaration, argCount);
            argCount ++;
        } while (match(TokenType::Comma));
    }
    consume(TokenType::RightParen, "Expect ')' after arguments.");
    return argCount;
}

void Parser::callFunction(FunctionDeclaration *functionDeclaration) {
    
    
    typecheckBeginFunctionCall(this, nullptr);

    emitByte(OpCode::Constant);
    emitByte(0xFF);
    // emitByte(0xFF);
    size_t patchIndex = currentChunk().code.size() - 1;
    uint8_t argCount = argumentList(functionDeclaration);
    Value function = maybeCompileFunctionInstantiation(functionDeclaration, argCount);

    uint16_t constant = makeConstant(function);
    assert(constant < 256);
    // currentChunk().code[patchIndex] = (constant >> 8) & 0xff;
    currentChunk().code[patchIndex] = constant & 0xff;
    
    typecheckEndFunctionCall(this, function, argCount);
    emitByte(OpCode::Call);
    emitByte(argCount);
}

void Parser::variable(ExpressionState es) {
    namedVariable(previous.text, es);
}

void Parser::namedVariable(const std::string_view &name, ExpressionState es) {
    FunctionDeclaration *functionDeclaration = compiler->resolveFunctionDeclaration(name);

    if (functionDeclaration != nullptr) {
        
        if (es.canAssign && match(TokenType::Equal)) {
            error("Cannot assign to a function.");
            return;
        }

        if (match(TokenType::LeftParen)) {
            callFunction(functionDeclaration);
        } else {
            error("Cannot reference function without calling it (yet).");
        }
    } else {
        
        OpCode getOp, setOp;
        int arg = compiler->resolveLocal(name);
        if (arg == -2) {
            error("Can't read local variable in its own initializer.");
        }
        if (arg != -1) {
            getOp = OpCode::GetLocal;
            setOp = OpCode::SetLocal;

            if (es.canAssign && match(TokenType::Equal)) {
                expression();
                emitByte(setOp); emitByte((uint8_t)arg);
                typecheckAssign(this, arg);
            } else {
                emitByte(getOp); emitByte((uint8_t)arg);
                typecheckVariable(this, arg);
            }
        } else {
            error("No variable found.");
        }
    }
}

ParseRule rules[] = {
    {&Parser::grouping, nullptr,           Precedence::None},       // LeftParen
    {nullptr,           nullptr,           Precedence::None},       // RightParen
    {nullptr,           nullptr,           Precedence::None},       // LeftBrace
    {nullptr,           nullptr,           Precedence::None},       // RightBrace
    {nullptr,           nullptr,           Precedence::None},       // Comma
    {nullptr,           nullptr,           Precedence::None},       // Dot
    {&Parser::unary,    &Parser::binary,   Precedence::Term},       // Minus
    {nullptr,           &Parser::binary,   Precedence::Term},       // Plus
    {nullptr,           nullptr,           Precedence::None},       // Semicolon
    {nullptr,           &Parser::binary,   Precedence::Factor},     // Slash
    {nullptr,           &Parser::binary,   Precedence::Factor},     // Star
    {nullptr,           nullptr,           Precedence::None},       // Colon
    {&Parser::unary,    nullptr,           Precedence::None},       // Bang
    {nullptr,           &Parser::binary,   Precedence::Equality},   // BangEqual
    {nullptr,           nullptr,           Precedence::None},       // Equal
    {nullptr,           &Parser::binary,   Precedence::Equality},   // EqualEqual
    {nullptr,           &Parser::binary,   Precedence::Comparison}, // Greater
    {nullptr,           &Parser::binary,   Precedence::Comparison}, // GreaterEqual
    {nullptr,           &Parser::binary,   Precedence::Comparison}, // Less
    {nullptr,           &Parser::binary,   Precedence::Comparison}, // LessEqual
    {&Parser::variable, nullptr,           Precedence::None},       // Identifier
    {&Parser::string,   nullptr,           Precedence::None},       // String
    {&Parser::number,   nullptr,           Precedence::None},       // Number
    {nullptr,           &Parser::and_,     Precedence::And},        // And
    {nullptr,           nullptr,           Precedence::None},       // Class
    {nullptr,           nullptr,           Precedence::None},       // Else
    {&Parser::literal,  nullptr,           Precedence::None},       // False
    {nullptr,           nullptr,           Precedence::None},       // For
    {nullptr,           nullptr,           Precedence::None},       // Fun
    {nullptr,           nullptr,           Precedence::None},       // If
    {&Parser::literal,  nullptr,           Precedence::None},       // Nil
    {nullptr,           &Parser::and_,     Precedence::Or},         // Or
    {nullptr,           nullptr,           Precedence::None},       // Print
    {nullptr,           nullptr,           Precedence::None},       // Return
    {nullptr,           nullptr,           Precedence::None},       // Super
    {nullptr,           nullptr,           Precedence::None},       // This
    {&Parser::literal,  nullptr,           Precedence::None},       // True
    {nullptr,           nullptr,           Precedence::None},       // Var
    {nullptr,           nullptr,           Precedence::None},       // While
    {nullptr,           nullptr,           Precedence::None},       // Newline
    {nullptr,           nullptr,           Precedence::None},       // Error
    {nullptr,           nullptr,           Precedence::None},       // Eof
};

static ParseRule &getRule(TokenType type) {
    return rules[static_cast<int>(type)];
}