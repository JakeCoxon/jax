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

enum class VariableStatus {
    Ok,
    AlreadyExists,
    TooMany
};

enum class FunctionType {
    Function, Script,
};

struct Local {
    std::string_view name;
    int depth;

    Local(const std::string_view &name, int depth):
        name(name), depth(depth) {};
};

struct Compiler {
    ObjFunction *function;
    FunctionType type;
    Compiler *enclosing;
    std::vector<Local> locals;
    int scopeDepth = 0;

    Compiler(ObjFunction *function, FunctionType type, Compiler *enclosing)
            : function(function), type(type), enclosing(enclosing) {
    }

    int resolveLocal(const std::string_view &name);
    VariableStatus declareVariable(const std::string_view& name);
};

struct ExpressionState {
    bool canAssign;
};


struct Parser {
    Token current;
    Token previous;
    Scanner &scanner;
    Compiler *compiler;

    bool hadError = false;
    bool panicMode = false;

    Parser(Scanner &scanner, Compiler *compiler):
            scanner(scanner), compiler(compiler) {
        initCompiler(compiler, FunctionType::Script);
    };

    void advance();
    bool match(TokenType type);
    bool check(TokenType type);
    void consume(TokenType type, const std::string &message);
    void consumeEndStatement(const std::string &message);
    void errorAt(Token &token, const std::string &message);
    uint8_t makeConstant(Value value);
    void parsePrecedence(Precedence precedence);
    uint8_t parseVariable(const std::string &errorMessage);
    void defineVariable(uint8_t global);
    void markInitialized();
    uint8_t identifierConstant(const std::string_view &name);
    void synchronize();
    void initCompiler(Compiler *compiler, FunctionType type);
    ObjFunction *endCompiler();
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
    void expressionStatement();
    void ifStatement();
    void whileStatement();
    void varDeclaration();
    void expression();
    void block();
    void function(FunctionType type);
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
    uint8_t argumentList();
    void call(ExpressionState es);

    Chunk &currentChunk() { 
        return compiler->function->chunk;
    }
    void emitByte(uint8_t byte) {
        currentChunk().write(byte, previous.line);
    }
    void emitByte(OpCode opcode) {
        emitByte(static_cast<uint8_t>(opcode));
    }

    void errorAtCurrent(const std::string &message) {
        errorAt(current, message);
    }
    void error(const std::string &message) {
        errorAt(previous, message);
    }
};

using ParseFn = void (Parser::*)(ExpressionState);

struct ParseRule {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
};

static ParseRule &getRule(TokenType type);

ObjFunction *compile(const std::string &source) {
    Scanner scanner { source };
    auto function = new ObjFunction();
    Compiler compiler(function, FunctionType::Script, nullptr);
    Parser parser { scanner, &compiler };
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

VariableStatus Compiler::declareVariable(const std::string_view& name) {
    if (scopeDepth == 0) return VariableStatus::Ok;

    for (int i = locals.size() - 1; i >= 0; i--) {
        Local* local = &locals[i];
        if (local->depth != -1 && local->depth < scopeDepth) {
            break; 
        }

        if (name == local->name) {
            return VariableStatus::AlreadyExists;
        }
    }
    if (locals.size() == UINT8_COUNT) {
        return VariableStatus::TooMany;
    }
    locals.push_back(Local { name, -1 });
    return VariableStatus::Ok;
}

void Parser::advance() {
    previous = current;
    while (true) {
        current = scanner.scanToken();
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
        while (cursor == 1 || (cursor > 1 && scanner.source[cursor - 1] != '\n')) {
            cursor--;
        }
        size_t offsetInLine = token.start - cursor;
        while (cursor < scanner.source.size()  && scanner.source[cursor] != '\n') {
            std::cerr << scanner.source[cursor];
            cursor++;
        }
        std::cerr << std::endl;
        for (size_t i = 0; i < offsetInLine; i++) std::cerr << ' ';
        for (size_t i = 0; i < fmax(token.text.size(), 1); i++) std::cerr << '^';
        std::cerr << std::endl;
        std::cerr << std::endl;
    }
}

uint8_t Parser::makeConstant(Value value) {
    int constant = currentChunk().addConstant(value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }
    return (uint8_t)constant;
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

void Parser::initCompiler(Compiler *compiler, FunctionType type) {
    this->compiler = compiler;
    // TODO: Garbage collection
    if (type != FunctionType::Script) {
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
    compiler = compiler->enclosing;
    return function;
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
    emitByte(OpCode::Return);
}

void Parser::emitConstant(Value value) {
    emitByte(OpCode::Constant); emitByte(makeConstant(value));
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


uint8_t Parser::parseVariable(const std::string &errorMessage) {
    consume(TokenType::Identifier, errorMessage);
    auto status = compiler->declareVariable(previous.text);
    switch (status) {
        case VariableStatus::AlreadyExists:
            error("Already variable with this name in this scope.");
        case VariableStatus::TooMany:
            error("Too many local variables on the stack.");
        case VariableStatus::Ok:
            break;
    }
    if (compiler->scopeDepth > 0) return 0;
    return identifierConstant(previous.text);
}

void Parser::defineVariable(uint8_t global) {
    if (compiler->scopeDepth > 0) {
        markInitialized();
        return;
    }
    emitByte(OpCode::DefineGlobal); emitByte(global);
}

void Parser::markInitialized() {
    if (compiler->scopeDepth == 0) return;
    compiler->locals.back().depth = compiler->scopeDepth;
}

uint8_t Parser::identifierConstant(const std::string_view &name) {
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
    expression();
    consumeEndStatement("Expect ';' or newline after value.");
    emitByte(OpCode::Print);
}

void Parser::expressionStatement() {
    expression();
    consumeEndStatement("Expect ';' or newline after expression.");
    emitByte(OpCode::Pop);
}

void Parser::ifStatement() {
    expression();
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
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TokenType::Equal)) {
        expression();
    } else {
        emitByte(OpCode::Nil);
    }
    consumeEndStatement("Expect ';' or newline after variable declaration.");
    defineVariable(global);
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

void Parser::function(FunctionType type) {
    auto function = new ObjFunction();
    Compiler functionCompiler(function, type, compiler);
    initCompiler(&functionCompiler, type);
    beginScope();
    consume(TokenType::LeftParen, "Expect '(' after function name.");
    if (!check(TokenType::RightParen)) {
        do {
            function->arity++;
            if (function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }

            uint8_t paramConstant = parseVariable("Expect parameter name.");
            defineVariable(paramConstant);
        } while (match(TokenType::Comma));
    }
    consume(TokenType::RightParen, "Expect ')' after after parameters.");

    consume(TokenType::LeftBrace, "Expect '{' before function body.");
    block();

    endCompiler();
    emitByte(OpCode::Constant);
    emitByte(makeConstant(function));
}

void Parser::funDeclaration() {
    uint8_t global = parseVariable("Expect function name.");
    markInitialized();
    function(FunctionType::Function);
    defineVariable(global);
}

void Parser::number(ExpressionState es) {
    // https://stackoverflow.com/questions/11752705/does-stdstring-contain-null-terminator
    // Don't want to risk it - just convert to a new string instead
    double value = strtod(std::string(previous.text).c_str(), nullptr);
    emitConstant(value);
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
        default: return;
    }
}

void Parser::literal(ExpressionState es) {
    switch (previous.type) {
        case TokenType::False: emitByte(OpCode::False); break;
        case TokenType::True: emitByte(OpCode::True); break;
        case TokenType::Nil: emitByte(OpCode::Nil); break;
        default: return;
    }
}

void Parser::string(ExpressionState es) {
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
}

void Parser::or_(ExpressionState es) {
    int elseJump = emitJump(OpCode::JumpIfFalse);
    int endJump = emitJump(OpCode::Jump);

    patchJump(elseJump);
    emitByte(OpCode::Pop);

    parsePrecedence(Precedence::Or);
    patchJump(endJump);
}

uint8_t Parser::argumentList() {
    uint8_t argCount = 0;
    if (!check(TokenType::RightParen)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't hve more than 255 arguments.");
            }
            argCount ++;
        } while (match(TokenType::Comma));
    }
    consume(TokenType::RightParen, "Expect ')' after arguments.");
    return argCount;
}

void Parser::call(ExpressionState es) {
    uint8_t argCount = argumentList();
    emitByte(OpCode::Call);
    emitByte(argCount);
}

void Parser::variable(ExpressionState es) {
    namedVariable(previous.text, es);
}

void Parser::namedVariable(const std::string_view &name, ExpressionState es) {
    OpCode getOp, setOp;
    int arg = compiler->resolveLocal(name);
    if (arg == -2) {
        error("Can't read local variable in its own initializer.");
    }
    if (arg != -1) {
        getOp = OpCode::GetLocal;
        setOp = OpCode::SetLocal;
    } else {
        arg = identifierConstant(name);
        getOp = OpCode::GetGlobal;
        setOp = OpCode::SetGlobal;
    }

    if (es.canAssign && match(TokenType::Equal)) {
        expression();
        emitByte(setOp); emitByte((uint8_t)arg);
    } else {
        emitByte(getOp); emitByte((uint8_t)arg);
    }
}

ParseRule rules[] = {
    {&Parser::grouping, &Parser::call,     Precedence::Call},       // LeftParen
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