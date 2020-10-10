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
    int index;
};

struct Scanner {
    std::string source;
    size_t start = 0;
    size_t current = 0;
    size_t lineStart = 0;
    int line = 1;

    Token scanToken();
    void skipWhitespace();
    bool match(char expected);

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
        return Token { type, text, line, (int) (start - lineStart) };
    }

    Token errorToken(const std::string& message) {
        return Token { TokenType::Error, message, line, (int) (start - lineStart) };
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
        printf("%2d ", token.index);
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
                line ++;
                advance();
                lineStart = current;
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

Token Scanner::scanToken() {
    skipWhitespace();

    start = current;

    if (isAtEnd()) return makeToken(TokenType::EOF_);

    char c = advance();

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

    }

    return errorToken("Unexpected character.");
}