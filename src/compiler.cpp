#include <vector>
#include <string>

bool compile(const std::string &source, Chunk &chunk) {
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
    return true;
}