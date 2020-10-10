enum class TokenType {
  // Single-character tokens.
  LeftParen, RightParen,
  LeftBrace, RightBrace,
  Comma, Dot, Minus, Plus,
  Semicolon, Slash, Star,

  // One or two character tokens.
  Bang, BangEqual,
  Equal, EqualEqual,
  Greater, GreaterEqual,
  Less, LessEqual,

  // Literals.
  Identifier, String, Number,

  // Keywords.
  And, Class, Else, False,
  For, Fun, If, Nil, Or,
  Print, Return, Super, This,
  True, Var, While,

  Error,
  EOF_
};


struct Token {
    TokenType type;
    std::string_view text;
    int line;
    int indexInLine;
};

struct Scanner {
    std::string source;
    size_t start = 0;
    size_t current = 0;
    size_t lineStart = 0;
    int indexInLine = 0;
    int line = 1;

    Token scanToken();
    void skipWhitespace();
    bool match(char expected);
    TokenType identifierType();
    TokenType checkKeyword(size_t offset, size_t length, const std::string &rest, TokenType type);

    Token string();
    Token number();
    Token identifier();

    char peek() { return source[current]; }
    bool isAtEnd() { return current == source.size(); }

    char advance() {
        current ++;
        return source[current - 1];
    }
    char peekNext() {
        if (isAtEnd()) return '\0';
        return source[current + 1];
    }

    Token makeToken(TokenType type) {
        auto text = std::string_view(&source[start], current - start);
        return Token { type, text, line, indexInLine };
    }

    Token errorToken(const std::string& message) {
        return Token { TokenType::Error, message, line, indexInLine };
    }
};


void compile(const std::string &source) {
    Scanner scanner { source };
    int line = -1;
    while (true) {
        Token token = scanner.scanToken();
        if (token.line != line) {
            printf("%4d ", token.line);
            line = token.line;
        } else {
            printf("   | ");
        }
        printf("%2d ", token.indexInLine);
        printf("%2d ", token.type);
        std::cout << "'" << token.text << "'" << std::endl;

        if (token.type == TokenType::EOF_) break;
    }

}

bool Scanner::match(char expected) {
    if (isAtEnd()) return false;
    if (source[current] != expected) return false;
    current ++;
    return true;
}

void Scanner::skipWhitespace() {
    while (true) {
        char c = peek();
        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                advance();
                break;
            case '\n':
                line ++; lineStart = current + 1;
                advance();
                break;
            case '/':
                if (peekNext() == '/') {
                    while (peek() != '\n' && !isAtEnd()) advance();
                } else {
                    return;
                }
            default:
                return;
        }
    }
}

static bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

static bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c == '_');
}

Token Scanner::scanToken() {
    skipWhitespace();

    start = current;
    indexInLine = (int) (start - lineStart);

    if (isAtEnd()) return makeToken(TokenType::EOF_);

    char c = advance();

    if (isAlpha(c)) return identifier();
    if (isDigit(c)) return number();

    switch (c) {
        case '(': return makeToken(TokenType::LeftParen);
        case ')': return makeToken(TokenType::RightParen);
        case '{': return makeToken(TokenType::LeftBrace);
        case '}': return makeToken(TokenType::RightBrace);
        case ';': return makeToken(TokenType::Semicolon);
        case ',': return makeToken(TokenType::Comma);
        case '.': return makeToken(TokenType::Dot);
        case '-': return makeToken(TokenType::Minus);
        case '+': return makeToken(TokenType::Plus);
        case '/': return makeToken(TokenType::Slash);
        case '*': return makeToken(TokenType::Star);
        case '!':
            return makeToken(
                match('=') ? TokenType::BangEqual : TokenType::Bang);
        case '=':
            return makeToken(
                match('=') ? TokenType::EqualEqual : TokenType::Equal);
        case '<':
            return makeToken(
                match('=') ? TokenType::LessEqual : TokenType::Less);
        case '>':
            return makeToken(
                match('=') ? TokenType::GreaterEqual : TokenType::Greater);
        case '"': return string();
    }

    return errorToken("Unexpected character.");
}

TokenType Scanner::identifierType() {
    switch (source[start]) {
        case 'a': return checkKeyword(1, 2, "nd", TokenType::And);
        case 'c': return checkKeyword(1, 4, "lass", TokenType::Class);
        case 'e': return checkKeyword(1, 3, "lse", TokenType::Else);
        case 'i': return checkKeyword(1, 1, "f", TokenType::If);
        case 'n': return checkKeyword(1, 2, "il", TokenType::Nil);
        case 'o': return checkKeyword(1, 1, "r", TokenType::Or);
        case 'p': return checkKeyword(1, 4, "rint", TokenType::Print);
        case 'r': return checkKeyword(1, 5, "eturn", TokenType::Return);
        case 's': return checkKeyword(1, 4, "uper", TokenType::Super);
        case 'v': return checkKeyword(1, 2, "ar", TokenType::Var);
        case 'w': return checkKeyword(1, 4, "hile", TokenType::While);
    }
    return TokenType::Identifier;
}

TokenType Scanner::checkKeyword(size_t offset, size_t length, const std::string &rest, TokenType type) {
    if (current == start + offset + length && 
            source.compare(start + offset, length, rest) == 0) {
        return type;
    }
    return TokenType::Identifier;
}

Token Scanner::string() {
    while (peek() != '"' && !isAtEnd()) {
        if (peek() == '\n') { 
            line ++; lineStart = current + 1;
        }
        advance();
    }

    if (isAtEnd()) return errorToken("Unterminated string");
    advance(); // Closing quote
    return makeToken(TokenType::String);
}

Token Scanner::number() {
    while (isDigit(peek())) advance();

    if (peek() == '.' && isDigit(peekNext())) {
        advance();

        while (isDigit(peek())) advance();
    }
    return makeToken(TokenType::Number);
}

Token Scanner::identifier() {
    while (isAlpha(peek()) || isDigit(peek())) advance();
    return makeToken(identifierType());
}
