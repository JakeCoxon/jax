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

struct Local {
    std::string_view name;
    int depth;
};

struct Compiler {
    std::vector<Local> locals;
    int scopeDepth = 0;

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
    Chunk *chunk;
    Compiler *compiler;

    bool hadError = false;
    bool panicMode = false;

    Parser(Scanner &scanner, Chunk *chunk, Compiler *compiler):
        scanner(scanner), chunk(chunk), compiler(compiler) {};

    void advance();
    bool match(TokenType type);
    bool check(TokenType type);
    void consume(TokenType type, const std::string &messsage);
    void errorAt(Token &token, const std::string &message);
    uint8_t makeConstant(Value value);
    void parsePrecedence(Precedence precedence);
    uint8_t parseVariable(const std::string &errorMessage);
    void defineVariable(uint8_t global);
    uint8_t identifierConstant(const std::string_view &name);
    void synchronize();
    void endCompiler();
    void beginScope();
    void endScope();

    void emitReturn();
    void emitConstant(Value value);
    int emitJump(OpCode instruction);
    void patchJump(int offset);

    void declaration();
    void statement();
    void printStatement();
    void expressionStatement();
    void ifStatement();
    void varDeclaration();
    void expression();
    void block();

    void number(ExpressionState es);
    void grouping(ExpressionState es);
    void unary(ExpressionState es);
    void binary(ExpressionState es);
    void literal(ExpressionState es);
    void string(ExpressionState es);
    void variable(ExpressionState es);
    void namedVariable(const std::string_view &name, ExpressionState es);

    Chunk &currentChunk() { return *chunk; }
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

bool compile(const std::string &source, Chunk &chunk) {
    Scanner scanner { source };
    Compiler compiler;
    Parser parser { scanner, &chunk, &compiler };
    parser.advance();

    while (!parser.match(TokenType::EOF_)) {
        parser.declaration();
    }

    parser.endCompiler();

    return !parser.hadError;
}

int Compiler::resolveLocal(const std::string_view &name) {
    for (int i = locals.size() - 1; i >= 0; i--) {
        Local* local = &locals[i];
        if (name == local->name) {
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
    locals.push_back(Local { name, scopeDepth });
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
        for (size_t i = 0; i < token.text.size(); i++) std::cerr << '^';
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
        if  (previous.type == TokenType::Semicolon) return;

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

void Parser::endCompiler() {
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    if (!hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
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
        return;
    }
    emitByte(OpCode::DefineGlobal); emitByte(global);
}

uint8_t Parser::identifierConstant(const std::string_view &name) {
    // TODO: Garbage collection
    auto objStr = new ObjString { {}, std::string(name) };
    return makeConstant(objStr);
}

void Parser::declaration() {
    if (match(TokenType::Var)) {
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
    consume(TokenType::Semicolon, "Expect ';' after value.");
    emitByte(OpCode::Print);
}

void Parser::expressionStatement() {
    expression();
    consume(TokenType::Semicolon, "Expect ';' after expression.");
    emitByte(OpCode::Pop);
}

void Parser::ifStatement() {
    expression();
    consume(TokenType::LeftBrace, "Expect '{' after if");

    int thenJump = emitJump(OpCode::JumpIfFalse);
    beginScope();
    block();
    endScope();

    patchJump(thenJump);
}

void Parser::varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TokenType::Equal)) {
        expression();
    } else {
        emitByte(OpCode::Nil);
    }
    consume(TokenType::Semicolon, "Expect ';' after variable declaration.");
    defineVariable(global);
}

void Parser::expression() {
    parsePrecedence(Precedence::Assignment);
}

void Parser::block() {
    while (!check(TokenType::RightBrace) && !check(TokenType::EOF_)) {
        declaration();
    }
    consume(TokenType::RightBrace, "Expect '}' after block");
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
    auto objStr = new ObjString { {}, std::string(str) };
    emitConstant(objStr);
}

void Parser::variable(ExpressionState es) {
    namedVariable(previous.text, es);
}

void Parser::namedVariable(const std::string_view &name, ExpressionState es) {
    OpCode getOp, setOp;
    int arg = compiler->resolveLocal(name);
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
    {nullptr,           nullptr,           Precedence::None},       // And
    {nullptr,           nullptr,           Precedence::None},       // Class
    {nullptr,           nullptr,           Precedence::None},       // Else
    {&Parser::literal,  nullptr,           Precedence::None},       // False
    {nullptr,           nullptr,           Precedence::None},       // For
    {nullptr,           nullptr,           Precedence::None},       // Fun
    {nullptr,           nullptr,           Precedence::None},       // If
    {&Parser::literal,  nullptr,           Precedence::None},       // Nil
    {nullptr,           nullptr,           Precedence::None},       // Or
    {nullptr,           nullptr,           Precedence::None},       // Print
    {nullptr,           nullptr,           Precedence::None},       // Return
    {nullptr,           nullptr,           Precedence::None},       // Super
    {nullptr,           nullptr,           Precedence::None},       // This
    {&Parser::literal,  nullptr,           Precedence::None},       // True
    {nullptr,           nullptr,           Precedence::None},       // Var
    {nullptr,           nullptr,           Precedence::None},       // While
    {nullptr,           nullptr,           Precedence::None},       // Error
    {nullptr,           nullptr,           Precedence::None},       // Eof
};

static ParseRule &getRule(TokenType type) {
    return rules[static_cast<int>(type)];
}