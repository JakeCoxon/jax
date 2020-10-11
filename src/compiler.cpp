#include <vector>
#include <string>

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

struct Parser {
    Token current;
    Token previous;
    Scanner &scanner;
    Chunk *chunk;

    bool hadError = false;
    bool panicMode = false;

    Parser(Scanner &scanner, Chunk *chunk):
        scanner(scanner), chunk(chunk) {};

    void advance();
    void consume(TokenType type, const std::string &messsage);
    void errorAt(Token &token, const std::string &message);
    uint8_t makeConstant(Value value);
    void parsePrecedence(Precedence precedence);
    void endCompiler();

    void emitReturn();
    void emitConstant(Value value);

    void expression();
    void number();
    void grouping();
    void unary();
    void binary();

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

using ParseFn = void (Parser::*)();

struct ParseRule {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
};

static ParseRule &getRule(TokenType type);

bool compile(const std::string &source, Chunk &chunk) {
    Scanner scanner { source };
    Parser parser { scanner, &chunk };
    parser.advance();
    parser.expression();
    parser.consume(TokenType::EOF_, "Expect end of expression.");

    parser.endCompiler();

    // int line = -1;
    // while (true) {
    //     Token token = scanner.scanToken();
    //     if (token.line != line) {
    //         printf("%4d ", token.line);
    //         line = token.line;
    //     } else {
    //         printf("   | ");
    //     }
    //     printf("%2zu ", token.start);
    //     printf("%2d ", token.type);
    //     std::cout << "'" << token.text << "'" << std::endl;
    //     parser.errorAt(token, "Testing");

    //     if (token.type == TokenType::EOF_) break;
    // }
    return !parser.hadError;
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

void Parser::consume(TokenType type, const std::string &message) {
    if (current.type == type) {
        advance();
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

void Parser::endCompiler() {
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    if (!hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

void Parser::emitReturn() {
    emitByte(OpCode::Return);
}

void Parser::emitConstant(Value value) {
    emitByte(OpCode::Constant); emitByte(makeConstant(value));
}

void Parser::parsePrecedence(Precedence precedence) {
    advance();

    ParseFn prefixRule = getRule(previous.type).prefix;
    if (!prefixRule) {
        error("Expect expression.");
        return;
    }

    (this->*prefixRule)();

    while (precedence <= getRule(current.type).precedence) {
        advance();
        ParseFn infixRule = getRule(previous.type).infix;
        (this->*infixRule)();
    }
}

void Parser::expression() {
    parsePrecedence(Precedence::Assignment);
}

void Parser::number() {
    // https://stackoverflow.com/questions/11752705/does-stdstring-contain-null-terminator
    // Don't want to risk it - just convert to a new string instead
    double value = strtod(std::string(previous.text).c_str(), nullptr);
    emitConstant(value);
}

void Parser::grouping() {
    expression();
    consume(TokenType::RightParen, "Expect ')' after expression.");
}

void Parser::unary() {
    TokenType operatorType = previous.type;
    
    expression();

    switch (operatorType) {
        case TokenType::Minus: emitByte(OpCode::Negate); break;
        default:
            return;
    }
}

void Parser::binary() {
    TokenType operatorType = previous.type;
    
    ParseRule &rule = getRule(operatorType);
    int prec = static_cast<int>(rule.precedence);
    parsePrecedence(Precedence(prec + 1)); // +1 because of left associativity

    switch (operatorType) {
        case TokenType::Plus:  emitByte(OpCode::Add); break;
        case TokenType::Minus: emitByte(OpCode::Subtract); break;
        case TokenType::Star:  emitByte(OpCode::Multiply); break;
        case TokenType::Slash: emitByte(OpCode::Divide); break;
        default:
            return;
    }
}

ParseRule rules[] = {
    {&Parser::grouping, nullptr,           Precedence::None},     // LeftParen
    {nullptr,           nullptr,           Precedence::None},     // RightParen
    {nullptr,           nullptr,           Precedence::None},     // LeftBrace
    {nullptr,           nullptr,           Precedence::None},     // RightBrace
    {nullptr,           nullptr,           Precedence::None},     // Comma
    {nullptr,           nullptr,           Precedence::None},     // Dot
    {&Parser::unary,    &Parser::binary,   Precedence::Term},     // Minus
    {nullptr,           &Parser::binary,   Precedence::Term},     // Plus
    {nullptr,           nullptr,           Precedence::None},     // Semicolon
    {nullptr,           &Parser::binary,   Precedence::Factor},   // Slash
    {nullptr,           &Parser::binary,   Precedence::Factor},   // Star
    {nullptr,           nullptr,           Precedence::None},     // Bang
    {nullptr,           nullptr,           Precedence::None},     // BangEqual
    {nullptr,           nullptr,           Precedence::None},     // Equal
    {nullptr,           nullptr,           Precedence::None},     // EqualEqual
    {nullptr,           nullptr,           Precedence::None},     // Greater
    {nullptr,           nullptr,           Precedence::None},     // GreaterEqual
    {nullptr,           nullptr,           Precedence::None},     // Less
    {nullptr,           nullptr,           Precedence::None},     // LessEqual
    {nullptr,           nullptr,           Precedence::None},     // Identifier
    {nullptr,           nullptr,           Precedence::None},     // String
    {&Parser::number,   nullptr,           Precedence::None},     // Number
    {nullptr,           nullptr,           Precedence::None},     // And
    {nullptr,           nullptr,           Precedence::None},     // Class
    {nullptr,           nullptr,           Precedence::None},     // Else
    {nullptr,           nullptr,           Precedence::None},     // False
    {nullptr,           nullptr,           Precedence::None},     // For
    {nullptr,           nullptr,           Precedence::None},     // Fun
    {nullptr,           nullptr,           Precedence::None},     // If
    {nullptr,           nullptr,           Precedence::None},     // Nil
    {nullptr,           nullptr,           Precedence::None},     // Or
    {nullptr,           nullptr,           Precedence::None},     // Print
    {nullptr,           nullptr,           Precedence::None},     // Return
    {nullptr,           nullptr,           Precedence::None},     // Super
    {nullptr,           nullptr,           Precedence::None},     // This
    {nullptr,           nullptr,           Precedence::None},     // True
    {nullptr,           nullptr,           Precedence::None},     // Var
    {nullptr,           nullptr,           Precedence::None},     // While
    {nullptr,           nullptr,           Precedence::None},     // Error
    {nullptr,           nullptr,           Precedence::None},     // Eof
};

static ParseRule &getRule(TokenType type) {
    return rules[static_cast<int>(type)];
}