#include <vector>
#include <string>

struct Parser {
    Token current;
    Token previous;
    Scanner &scanner;
    bool hadError = false;
    bool panicMode = false;

    Parser(Scanner &scanner): scanner(scanner) {};

    void advance();
    void consume(TokenType type, const std::string &messsage);
    void errorAt(Token &token, const std::string &message);

    void errorAtCurrent(const std::string &message) {
        errorAt(current, message);
    }
    void error(const std::string &message) {
        errorAt(previous, message);
    }
};

bool compile(const std::string &source, Chunk &chunk) {
    Scanner scanner { source };
    Parser parser { scanner };
    parser.hadError = true;
    parser.advance();

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
        while (cursor > 1 && scanner.source[cursor - 1] != '\n') cursor--;
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