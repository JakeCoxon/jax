enum class TokenType {
    // Single-character tokens.
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    LeftSquare, RightSquare,
    Comma, Dot, Minus, Plus,
    Semicolon, Slash, Star,
    Colon, At, Dollar, Pipe,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    PlusEqual, MinusEqual,
    SlashEqual, StarEqual,

    // Literals.
    Identifier, String, Number,

    // Keywords.
    And, Struct, Else, False,
    For, Fun, If, Nil, Or,
    Print, Return, Super, This,
    True, Var, While, Block,
    Static, In,

    Newline,
    Error,
    EOF_
};


struct Token {
    TokenType type;
    std::string_view text;
    int line;
    size_t start;
};

struct Scanner {
    const std::string &source;
    size_t start = 0;
    size_t current = 0;
    size_t lineStart = 0;
    int line = 1;
    int parens = 0;
    bool isString = false;
    bool isStringInterpolation = false;

    Token currentToken = {};
    Token previousToken = {};

    void advanceToken();
    Token scanToken();
    void skipWhitespace();
    bool match(char expected);
    TokenType checkKeyword(size_t offset, const std::string &rest, TokenType type);
    TokenType identifierType();

    Token scanTokenString();

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
        return Token { type, text, line, start };
    }

    Token errorToken(const std::string& message) {
        return Token { TokenType::Error, message, line, start };
    }
};

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
                if (parens == 0) return;
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
    if (isString) return scanTokenString();
    
    skipWhitespace();

    start = current;

    if (peek() == '\n') {
        // Construct token before we advance the line
        Token newline = makeToken(TokenType::Newline);
        line ++; lineStart = current + 1;
        advance();
        return newline;
    }

    if (isAtEnd()) return makeToken(TokenType::EOF_);

    char c = advance();

    if (isAlpha(c)) return identifier();
    if (isDigit(c)) return number();

    switch (c) {
        case '(': parens ++; return makeToken(TokenType::LeftParen);
        case ')': parens --; return makeToken(TokenType::RightParen);
        case '{': return makeToken(TokenType::LeftBrace);
        case '}': return makeToken(TokenType::RightBrace);
        case '[': return makeToken(TokenType::LeftSquare);
        case ']': return makeToken(TokenType::RightSquare);
        case ';': return makeToken(TokenType::Semicolon);
        case ',': return makeToken(TokenType::Comma);
        case '.': return makeToken(TokenType::Dot);
        case ':': return makeToken(TokenType::Colon);
        case '@': return makeToken(TokenType::At);
        case '$': return makeToken(TokenType::Dollar);
        case '|': return makeToken(TokenType::Pipe);
        case '-': 
            return makeToken(match('=') ? TokenType::MinusEqual : TokenType::Minus);
        case '+':
            return makeToken(match('=') ? TokenType::PlusEqual : TokenType::Plus);
        case '/':
            return makeToken(match('=') ? TokenType::SlashEqual : TokenType::Slash);
        case '*':
            return makeToken(match('=') ? TokenType::StarEqual : TokenType::Star);
        case '!':
            return makeToken(match('=') ? TokenType::BangEqual : TokenType::Bang);
        case '=':
            return makeToken(match('=') ? TokenType::EqualEqual : TokenType::Equal);
        case '<':
            return makeToken(match('=') ? TokenType::LessEqual : TokenType::Less);
        case '>':
            return makeToken(match('=') ? TokenType::GreaterEqual : TokenType::Greater);
        case '"': return string();
    }

    return errorToken("Unexpected character.");
}

void Scanner::advanceToken() {
    previousToken = currentToken;
    currentToken = scanToken();
}


TokenType Scanner::checkKeyword(size_t offset, const std::string &rest, TokenType type) {
    if (current == start + offset + rest.size() && 
            source.compare(start + offset, rest.size(), rest) == 0) {
        return type;
    }
    return TokenType::Identifier;
}

TokenType Scanner::identifierType() {
    switch (source[start]) {
        case 'a': return checkKeyword(1, "nd", TokenType::And);
        case 'b': return checkKeyword(1, "lock", TokenType::Block);
        case 'e': return checkKeyword(1, "lse", TokenType::Else);
        case 'f': 
            if (current - start > 1) {
                switch (source[start + 1]) {
                    case 'a': return checkKeyword(2, "lse", TokenType::False);
                    case 'o': return checkKeyword(2, "r", TokenType::For);
                    case 'u': return checkKeyword(2, "n", TokenType::Fun);
                }
            }
            break;
        case 'i':
            if (current - start > 1) {
                switch (source[start + 1]) {
                    case 'f': return checkKeyword(2, "", TokenType::If);
                    case 'n': return checkKeyword(2, "", TokenType::In);
                }
            }
            break;
        case 'n': return checkKeyword(1, "il", TokenType::Nil);
        case 'o': return checkKeyword(1, "r", TokenType::Or);
        case 'p': return checkKeyword(1, "rint", TokenType::Print);
        case 'r': return checkKeyword(1, "eturn", TokenType::Return);
        case 's': 
            if (current - start > 1) {
                switch (source[start + 1]) {
                    case 'u': return checkKeyword(2, "per", TokenType::Super);
                    case 't': 
                        if (current - start > 2) {
                            switch (source[start + 2]) {
                                case 'a': return checkKeyword(3, "tic", TokenType::Static);
                                case 'r': return checkKeyword(3, "uct", TokenType::Struct);
                            }
                        }
                        break;
                }
            }
            break;
        case 't': 
            if (current - start > 1) {
                switch (source[start + 1]) {
                    case 'h': return checkKeyword(2, "is", TokenType::This);
                    case 'r': return checkKeyword(2, "ue", TokenType::True);
                }
            }
            break;
        case 'v': return checkKeyword(1, "ar", TokenType::Var);
        case 'w': return checkKeyword(1, "hile", TokenType::While);
    }
    return TokenType::Identifier;
}

Token Scanner::string() {
    isString = true;
    while (peek() != '"' && !isAtEnd()) {
        if (peek() == '\n') { 
            return makeToken(TokenType::String);
        }
        else if (peek() == '{' || peek() == '\\') {
            return makeToken(TokenType::String);
        }
        advance();
    }
    isString = false;
    if (isAtEnd()) return errorToken("Unterminated string");
    advance(); // Closing quote
    return makeToken(TokenType::String);
}

Token Scanner::scanTokenString() {

    start = current;
    if (isAtEnd()) return makeToken(TokenType::EOF_);

    if (isStringInterpolation) {
        if (peek() == '"') {
            advance();
            return errorToken("Strings cannot be nested.");
        } else if (peek() == '}') {
            advance();
            isStringInterpolation = false;
            return makeToken(TokenType::RightBrace);
        }

        isString = false; // bit hacky
        Token result = scanToken();
        isString = true;
        return result;
    }

    char c = advance();
    if (c == '\n') {
        line ++; lineStart = current + 1;
        return makeToken(TokenType::String);
    }
    if (c == '{') {
        isStringInterpolation = true;
        return makeToken(TokenType::LeftBrace);
    }

    if (c == '\\') {
        c = advance();
        
        if (c == '\\') {}
        else if (c == '"') {}
        else if (c == '{') {}
        else if (c == 'n') {}
        else { return errorToken("Invalid escape character."); }
        return makeToken(TokenType::String);
    }
    else if (c == '"') {
        isString = false;
        return makeToken(TokenType::String);
    }


    return string();
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
