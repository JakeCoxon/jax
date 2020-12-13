#include <vector>
#include <string>
#include <map>

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

enum class CompilerType {
    Function, Script,
};

struct Local {
    std::string_view name;
    Type type = types::Void;
    int depth;
    int stackOffset = -1;
    bool isStatic = false;
    std::string_view renamedTo;
    int renamedFromNum = 0;

    Local(const std::string_view &name, int depth, int stackOffset):
        name(name), depth(depth), stackOffset(stackOffset) {};
};

struct Compiler;

struct FunctionParameter {
    std::string_view name;
    Type type;
    bool isStatic = false;
};

struct FunctionDeclaration;

struct FunctionInstantiation {
    Type type;
    Value function;
    Compiler *compiler = nullptr;
    FunctionDeclaration *declaration = nullptr;
};

struct FunctionDeclaration {
    std::string_view name;
    std::vector<FunctionParameter> parameters;
    Type returnType;
    bool polymorphic;

    Compiler *enclosingCompiler;
    std::vector<FunctionInstantiation> overloads;

    bool isExtern = false;
    bool isInline = false;
    
    size_t constant;
    size_t blockStart;
    int blockLine;
};

struct VmWriter;
struct AstGen;

struct ExpressionState {
    bool canAssign;
};
struct StringParseFlags {
    bool indented;
};

struct Parser {
    Scanner *scanner;
    Compiler *compiler;

    std::vector<Type> types;
    std::map<std::string, Type> typesByName;

    AstGen *ast;
    VmWriter *vmWriter;

    bool isBytecode = false;
    std::string generatedCodeBuffer;

    bool hadError = false;
    bool panicMode = false;

    Parser(Scanner *scanner, Compiler *compiler, AstGen *ast, VmWriter *vmWriter):
            scanner(scanner), compiler(compiler), ast(ast), vmWriter(vmWriter) {
        initCompiler(compiler);
    };

    Token current();
    Token previous();
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
    Type inlineFunction(ObjFunction *function, FunctionDeclaration *funDecl);
    FunctionInstantiation *getInstantiationByStackArguments(FunctionDeclaration *functionDeclaration, int argCount);
    void compileFunctionInstantiation(FunctionInstantiation &functionDeclaration);
    FunctionInstantiation *createInstantiation(FunctionDeclaration *functionDeclaration);

    void beginScope();
    void endScope();


    Chunk &currentChunk();


    void declaration();
    void staticDeclaration();
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
    void structDeclaration();

    void number(ExpressionState es);
    void grouping(ExpressionState es);
    void arraylit(ExpressionState es);
    void arrayget(ExpressionState es);
    void unary(ExpressionState es);
    void binary(ExpressionState es);
    void literal(ExpressionState es);
    void string(ExpressionState es);
    void stringAdvanced(StringParseFlags spf);
    void at(ExpressionState es);
    void namedVariable(const std::string_view &name, ExpressionState es);
    void variable(ExpressionState es);
    void and_(ExpressionState es);
    void or_(ExpressionState es);
    void dot(ExpressionState es);
    void lambda(ExpressionState es);
    void callLambda(ExpressionState es);
    uint8_t argumentList(FunctionDeclaration *functionDeclaration);
    void callFunction(FunctionDeclaration *functionDeclaration);


    void errorAtCurrent(const std::string &message) {
        errorAt(scanner->currentToken, message);
    }
    void error(const std::string &message) {
        errorAt(scanner->previousToken, message);
    }
};

// Compiler
// Holds the state for compiling functions-like blocks, including
// lambdas and the main program. Has its own local stack, expressionType
// stack and function declarations.
struct Compiler {
    ObjFunction *function;
    CompilerType type;
    Compiler *enclosing = nullptr;
    Compiler *inlinedFrom = nullptr;
    std::vector<Local> locals;
    std::vector<Type> expressionTypeStack;
    std::vector<FunctionDeclaration*> functionDeclarations;

    int scopeDepth = 0;
    int nextStackOffset = 0;
    bool compiled = false;

    Type implicitReturnType = nullptr;

    Compiler(ObjFunction *function, CompilerType type, Compiler *enclosing)
            : function(function), type(type), enclosing(enclosing) {
    }

    int resolveLocal(const std::string_view &name);
    FunctionDeclaration *resolveFunctionDeclaration(const std::string_view &name);
    void declareVariable(Parser* parser, const std::string_view& name);
    void handleRenames(Parser *parser, Local &newLocal);
    void markInitialized();
};

#include "typecheck.cpp"
#include "ast.cpp"
#include "vmwriter.cpp"
#include "compiler_functions.cpp"

using ParseFn = void (Parser::*)(ExpressionState);

struct ParseRule {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
};

static ParseRule &getRule(TokenType type);

void registry(Parser *parser);

void registerNative(
        Parser *parser, std::string name, Type returnType,
        std::vector<FunctionParameter> parameters, NativeFn nativeFn) {
    FunctionTypeData typeData {};
    for (size_t i = 0; i < parameters.size(); i++) {
        typeData.parameterTypes.push_back(parameters[i].type);
    }
    typeData.returnType = returnType;
    Type type = addNewType(parser, typeData);

    // TODO: garbage collection
    auto native = new ObjNative{type, nativeFn};
    // TODO: garbage collection
    auto functionName = new ObjString{name};
    FunctionInstantiation inst = {type, native};
    auto constant = parser->makeConstant(native);

    auto decl = new FunctionDeclaration;
    inst.declaration = decl;

    decl->name = functionName->text;
    decl->parameters = parameters;
    decl->returnType = returnType;
    decl->polymorphic = false;
    decl->enclosingCompiler = nullptr;
    decl->isExtern = true;
    decl->constant = constant;
    decl->overloads = {inst};
    parser->compiler->functionDeclarations.push_back(decl);
}

bool compileToString(const std::string &source, std::ostringstream &stream) {
    Scanner scanner { source };
    auto function = new ObjFunction();
    AstGen ast;
    VmWriter vmWriter;
    Compiler compiler(function, CompilerType::Script, nullptr);
    Parser parser { &scanner, &compiler, &ast, &vmWriter };
    vmWriter.parser = &parser;
    ast.parser = &parser;
    typecheckInit(&parser);

    registry(&parser);


    parser.advance();

    while (!parser.match(TokenType::EOF_)) {
        while (parser.match(TokenType::Newline)) {}
        parser.declaration();
        while (parser.match(TokenType::Newline)) {}
    }

    parser.endCompiler();

    if (!parser.hadError) {
        generateCodeC(&ast, stream);
    }
    return !parser.hadError;
    
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
        if (functionDeclarations[i]->name == name) {
            return functionDeclarations[i];
        }
    }
    if (enclosing) { return enclosing->resolveFunctionDeclaration(name); }
    return nullptr;
}

void Compiler::handleRenames(Parser *parser, Local &newLocal) {
    Compiler *topLevel = inlinedFrom;
    while (topLevel->inlinedFrom) {
        topLevel = topLevel->inlinedFrom;
    }

    Local *foundLocal = nullptr;
    for (int i = topLevel->locals.size() - 1; i >= 0; i--) {
        Local* local = &topLevel->locals[i];
        if (newLocal.name == local->name) {
            foundLocal = local;
            break;
        }
    }

    std::string_view newNameView = newLocal.name;
    if (foundLocal) {
        foundLocal->renamedFromNum ++;
        std::ostringstream str;
        str << "_shadow_" << newLocal.name;
        if (foundLocal->renamedFromNum > 1) {
            str << foundLocal->renamedFromNum;
        }
        std::string *myString = new std::string(str.str()); // @leak
        newNameView = std::string_view(*myString);
        newLocal.renamedTo = newNameView;
    }

    // Add a new local that shouldn't be touched (Ensure this somehow).
    // This makes it so multiple inlines of the same function will be correctly
    // renamed, and in addition will make sure any variables of this name *later
    // in the original function* will be correctly renamed. This local should have
    // the correct scopeDepth so it will be destroyed at the end of the scope.
    // (Bu they will need to stay until the end of the scope).
    // This local should be before any uninitialised local, to maintain the
    // correct ordering of locals, plus then markInitialized can just read the top
    int newIndex = topLevel->locals.size();
    if (newIndex > 0 && topLevel->locals[newIndex - 1].depth == -1) {
        newIndex --;
    }
    int newStackOffset = newIndex > 0 ? topLevel->locals[newIndex - 1].stackOffset : 0;
    topLevel->locals.insert(topLevel->locals.begin() + newIndex,
        { newNameView, topLevel->scopeDepth, newStackOffset });

}

void Compiler::declareVariable(Parser *parser, const std::string_view& name) {
    if (locals.size() == UINT8_COUNT) {
        parser->error("Too many local variables on the stack.");
    }

    for (int i = locals.size() - 1; i >= 0; i--) {
        Local* local = &locals[i];
        if (local->depth != -1 && local->depth < scopeDepth) {
            break; 
        }

        if (name == local->name) {
            parser->error("Already variable with this name in this scope.");
        }
    }
    locals.push_back({ name, -1, -1 });
    
    // We need to handle renames if this is an inlined compiler.
    if (inlinedFrom) {
        handleRenames(parser, locals.back());
    }
}

void Compiler::markInitialized() {
    // Must be called after the local's type has been finalized
    Local &local = locals.back();
    local.depth = scopeDepth;

    // I've opted to maintain a nextStackOffset that needs increasing and
    // decreasing when the local goes out of scope, this is because a few
    // places interact with stack offsets and it's a bit easier to just
    // read from 1 place, it's also nicer to just have to deal with the
    // current local's type rather than reading the previous local and
    // checking if it's static each time.
    if (local.isStatic) {
        local.stackOffset = nextStackOffset;
        nextStackOffset += slotSizeOfType(local.type);
    }
}

Chunk &Parser::currentChunk() { 
    return vmWriter->function->chunk;
}

Token Parser::current() {
    return scanner->currentToken;
}
Token Parser::previous() {
    return scanner->previousToken;
}

void Parser::advance() {
    // previous = current;
    while (true) {
        scanner->advanceToken();
        if (current().type == TokenType::Error) {
            errorAtCurrent(std::string(current().text));
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
    return current().type == type;
}

void Parser::consume(TokenType type, const std::string &message) {
    if (current().type != type) {
        errorAtCurrent(message);
        return;
    }
    advance();
}

void Parser::consumeEndStatement(const std::string &message) {
    if (current().type == TokenType::Semicolon || current().type == TokenType::Newline) {
        advance();
        return;
    }
    if (current().type == TokenType::EOF_) {
        return;
    }
    
    errorAtCurrent(message);
}

void Parser::errorAt(Token &token, const std::string &message) {
    if (panicMode) return;
    panicMode = true;
    hadError = true;
    std::cerr << "[line " << token.line << "] Error";

    size_t tokenSize = fmax(token.text.size(), 1);

    if (token.type == TokenType::EOF_) {
        std::cerr << " at end: " << message << std::endl;
        return;
    }
    if (token.type == TokenType::Error) tokenSize = 1;

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
    for (size_t i = 0; i < tokenSize; i++) std::cerr << '^';
    std::cerr << std::endl;
    std::cerr << std::endl;
}

uint16_t Parser::makeConstant(Value value) {
    int constant = currentChunk().addConstant(value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }
    return (uint16_t)constant;
}

void Parser::synchronize() {
    panicMode = false;

    while (current().type != TokenType::EOF_) {
        if (previous().type == TokenType::Semicolon ||
            previous().type == TokenType::Newline) return;

        switch (current().type) {
            case TokenType::Struct:
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
    if (compiler->type != CompilerType::Script) {
        compiler->function->name = new ObjString(std::string(previous().text));
    }

}


ObjFunction *Parser::endCompiler() {

    for (FunctionDeclaration *funDecl : compiler->functionDeclarations) {
        if (funDecl->polymorphic) continue;
        if (funDecl->isInline) continue;
        if (funDecl->overloads.size() != 0) continue;

        auto inst = createInstantiation(funDecl);
        typecheckInstantiationFromArgumentList(this, inst);
        compileFunctionInstantiation(*inst);
    }
    
    if (isBytecode) vmWriter->endCompiler(); // TODO: Is this ever needed?

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

// These should be on compiler
void Parser::beginScope() {
    compiler->scopeDepth ++;
}

void Parser::endScope() {
    compiler->scopeDepth --;

    // This should run regardless of whether we are in bytecode
    // mode or not, because we may have introduced any amount
    // of static locals throughout the scope.
    vmWriter->endScope();
    
    while (compiler->locals.size() > 0 &&
            (compiler->locals.back().depth > compiler->scopeDepth)) {
        if (compiler->locals.back().isStatic) {
            compiler->nextStackOffset -= slotSizeOfType(compiler->locals.back().type);
        }
        compiler->locals.pop_back();
    }
    
}


void Parser::parsePrecedence(Precedence precedence) {
    advance();

    ParseFn prefixRule = getRule(previous().type).prefix;
    if (!prefixRule) {
        error("Expect expression.");
        return;
    }

    ExpressionState es;
    es.canAssign = precedence <= Precedence::Assignment;
    
    (this->*prefixRule)(es);

    while (precedence <= getRule(current().type).precedence) {
        advance();
        ParseFn infixRule = getRule(previous().type).infix;
        (this->*infixRule)(es);
    }

    if (es.canAssign && match(TokenType::Equal)) {
        error("Invalid assignment target.");
    }
}


void Parser::parseVariable(const std::string &errorMessage) {
    consume(TokenType::Identifier, errorMessage);
    compiler->declareVariable(this, previous().text);
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
    } else if (match(TokenType::Struct)) {
        structDeclaration();
    } else if (match(TokenType::Static)) {
        staticDeclaration();
    } else {
        statement();
    }

    if (panicMode) synchronize();
}

void Parser::staticDeclaration() {
    if (isBytecode) {
        error("Cannot nest static.");
        return;
    }
    isBytecode = true;
    declaration();

    vmWriter->vm.run();

    isBytecode = false;

    if (generatedCodeBuffer.size() > 0) {
        Scanner *initialScanner = this->scanner;

        Scanner tempScanner { generatedCodeBuffer };
        scanner = &tempScanner;

        scanner->current = 0;
        scanner->start = 0;
        scanner->line = 1;
        scanner->parens = 0;
        advance();
        
        // This is the same as block() but without the end brace
        while (!check(TokenType::RightBrace) && !check(TokenType::EOF_)) {
            while (match(TokenType::Newline)) {}
            declaration();
            while (match(TokenType::Newline)) {}
        }

        // Reset back
        this->scanner = initialScanner;

        // Clear this before we start parsing? If static in the string
        generatedCodeBuffer = "";
    }
}

void Parser::statement() {
    // We don't know which statement is the last so every
    // statement resets this type and expressionStatement
    // will set it, leaving the last one as the correct type.
    // Performance could suffer doing it this way?
    compiler->implicitReturnType = nullptr;
    
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
        if (!isBytecode) ast->beginBlock();
        block();
        if (!isBytecode) ast->endBlock();
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
                error("Can't have more than 255 arguments.");
            }

            typecheckPrintArgument(this, &printState, argCount);
            argCount ++;

        } while (match(TokenType::Comma));
    }
    consume(TokenType::RightParen, "Expect ')' after arguments.");
    consumeEndStatement("Expect ';' or newline after value.");
    
    if (isBytecode) vmWriter->print();
    else ast->print();

    typecheckPrintEnd(this, &printState, argCount);
}

void Parser::returnStatement() {
    if (compiler->type == CompilerType::Script) {
        error("Can't return from top-level code.");
    }
    if (match(TokenType::Semicolon) || match(TokenType::Newline)) {
        typecheckReturnNil(this, compiler->function);
        if (isBytecode) vmWriter->returnStatement(true);
        else ast->returnStatement(true);
    } else {
        expression();
        consumeEndStatement("Expect ';' or newline after return value");
        typecheckReturn(this, compiler->function);
        if (isBytecode) vmWriter->returnStatement(false);
        else ast->returnStatement(false);
    }

}

void Parser::expressionStatement() {
    expression();
    consumeEndStatement("Expect ';' or newline after expression.");

    if (isBytecode) vmWriter->exprStatement();
    else ast->exprStatement();

    compiler->implicitReturnType = compiler->expressionTypeStack.back();
    typecheckEndStatement(this);
}

void Parser::ifStatement() {
    expression();
    
    consume(TokenType::LeftBrace, "Expect '{' after if.");

    IfStatement *ifStmtAst;
    IfStatementPatchState patchState;

    if (isBytecode) { patchState = vmWriter->beginIfStatementBlock(); }
    else { ifStmtAst = ast->beginIfStatementBlock(); }
    
    typecheckIfCondition(this);

    beginScope();
    block();
    endScope();

    if (isBytecode) vmWriter->elseStatementBlock(&patchState);
    else ast->endBlock();

    if (match(TokenType::Else)) {
        consume(TokenType::LeftBrace, "Expect '{' after if.");

        beginScope();

        if (!isBytecode) ast->beginElseBlock(ifStmtAst);
        block();
        if (!isBytecode) ast->endBlock();
        endScope();
    }

    if (isBytecode) vmWriter->endIfStatementBlock(&patchState);
    
    consumeEndStatement("Expect ';' or newline after block.");
}

void Parser::whileStatement() {
    WhileStatementPatchState patchState;
    if (isBytecode) { patchState = vmWriter->beginWhileStatementBlock(); }
    
    expression();
    consume(TokenType::LeftBrace, "Expect '{' after condition.");

    if (isBytecode) vmWriter->endWhileCondition(&patchState);

    beginScope();
    if (!isBytecode) ast->beginWhileStatementBlock();
    typecheckIfCondition(this);

    block();
    if (!isBytecode) ast->endBlock();
    endScope();

    if (isBytecode) vmWriter->endWhileStatementBlock(&patchState);

    consumeEndStatement("Expect ';' or newline after block.");
}

void Parser::varDeclaration() {
    // bool isBytecode = false;
    // if (match(TokenType::At)) {
    //     consume(TokenType::Identifier, "Expect identifier after '@'.");
    //     if (previous().text == "static") {
    //         isBytecode = true;
    //     }
    // }
    parseVariable("Expect variable name.");
    compiler->locals.back().isStatic = isBytecode;

    Token nameToken = previous();

    Type type = types::Void;
    if (match(TokenType::Colon)) {
        consume(TokenType::Identifier, "Expect type name after ':'.");
        type = typeByName(this, previous().text);
    }

    compiler->locals.back().type = type;

    bool initializer = false;
    if (match(TokenType::Equal)) {
        expression();
        initializer = true;
    } else {
        if (isBytecode) {
            vmWriter->varDeclarationNoValue();
        }
    }
    consumeEndStatement("Expect ';' or newline after variable declaration.");

    if (initializer) {
        typecheckVarDeclarationInitializer(this, compiler->locals.back());
    }
    compiler->markInitialized();

    // Nothing happens the VM - it is just left on the stack
    if (!isBytecode) {
        ast->varDeclaration(compiler->locals.back(), initializer);
    }
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


void Parser::structDeclaration() {
    consume(TokenType::Identifier, "Expect struct name.");
    
    StructTypeData typeData;
    typeData.name = previous().text;

    consume(TokenType::LeftBrace, "Expect '{' after struct name.");
    while (true) {
        while (match(TokenType::Semicolon) || match(TokenType::Newline)) {
            continue;
        }
        if (match(TokenType::Identifier)) {
            auto memberName = previous().text;
            consume(TokenType::Colon, "Expect ':' after member name.");
            consume(TokenType::Identifier, "Expect identifier after ':'.");
            auto memberTypeName = previous().text;
            consumeEndStatement("Expect ';' or newline after member.");
            Type memberType = typeByName(this, memberTypeName);
            StructMember m = { std::string(memberName), memberType };
            typeData.members.push_back(m);
            continue;
        } else {
            break;
        }
    }
    Type type = addNamedType(this, typeData.name, typeData);
    if (!isBytecode) ast->structDeclaration(type);
    consume(TokenType::RightBrace, "Expect '}' after member list.");
}


void Parser::number(ExpressionState es) {
    
    typecheckNumber(this);

    if (isBytecode) vmWriter->number(previous());
    else ast->number(previous());
}

void Parser::grouping(ExpressionState es) {
    expression();
    consume(TokenType::RightParen, "Expect ')' after expression.");
}

void Parser::arraylit(ExpressionState es) {
    int numElements = 0;
    Type elementType = types::Void;
    while (true) {
        expression();
        Type itElementType = compiler->expressionTypeStack.back();
        compiler->expressionTypeStack.pop_back();
        if (numElements == 0) {
            elementType = itElementType;
        } else if (!typecheckIsAssignable(this, elementType, itElementType)) {
            error("Can't put this is the array .");
        }
        numElements ++;
        if (match(TokenType::RightSquare)) break;
        consume(TokenType::Comma, "Expect ',' or ']' after array element list.");
    }

    // Type returnType = typesByName["array"];
    Type arrayType = addNewType(this, ArrayTypeData { elementType });
    compiler->expressionTypeStack.push_back(arrayType);
    
    if (!isBytecode) ast->arrayLiteral(numElements, elementType);
}

void Parser::arrayget(ExpressionState es) {
    expression();
    consume(TokenType::RightSquare, "Expect ']' after expression.");

    typecheckArrayAccess(this);

    if (!isBytecode) {
        Type elementType = compiler->expressionTypeStack.back();
        compiler->expressionTypeStack.push_back(types::Void); // Fake this for the AST
        ast->typeLiteral(elementType);
        compiler->expressionTypeStack.pop_back();
        ast->functionCallNative("_array_index", 3);
    }
}

void Parser::unary(ExpressionState es) {
    TokenType operatorType = previous().type;
    
    expression();

    if (isBytecode) vmWriter->unary(operatorType);
}

void Parser::binary(ExpressionState es) {
    TokenType operatorType = previous().type;
    Token binaryToken = previous();

    ParseRule &rule = getRule(operatorType);
    int prec = static_cast<int>(rule.precedence);
    parsePrecedence(Precedence(prec + 1)); // +1 because of left associativity

    typecheckBinary(this, operatorType);

    if (isBytecode) vmWriter->infix(operatorType);
    else ast->infix(binaryToken);

}

void Parser::literal(ExpressionState es) {
    typecheckLiteral(this);
    switch (previous().type) {
        case TokenType::False: 
            if (isBytecode) vmWriter->booleanLiteral(false);
            else ast->booleanLiteral(false);
            break;
        case TokenType::True: 
            if (isBytecode) vmWriter->booleanLiteral(true);
            else ast->booleanLiteral(true);
            break;
        case TokenType::Nil: 
            if (isBytecode) vmWriter->nilLiteral();
            else ast->booleanLiteral(false);
            break;
        default: return;
    }
}

void Parser::string(ExpressionState es) {
    StringParseFlags spf;
    spf.indented = false;
    stringAdvanced(spf);
}

void Parser::stringAdvanced(StringParseFlags spf) {

    typecheckString(this);

    Token original = previous();
    std::string_view originalText = previous().text;

    // Quick return when there's no fancy stuff
    if (originalText.size() > 1 && originalText[originalText.size() - 1] == '"') {
        if (isBytecode) vmWriter->string(original);
        else ast->string(original);
        return;
    }

    Token string;
    string.line = 0;
    string.start = 0;
    string.type = TokenType::String;

    std::ostringstream ss;
    ss << originalText;

    StringLiteral *stringExpr = nullptr;

    if (!isBytecode) {
        // We do something unusual here, for non bytecode string
        // format we want the string first, and the arguments are
        // varargs to a C function. But we don't have the full string
        // until after we've traversed the arguments. So store the
        // expression, and update the string_view at the end. We
        // normally don't do something like this so be careful.
        ast->string(string);
        stringExpr = &mpark::get<StringLiteral>(ast->expressionStack.back()->variant);

        // For bytecode its actually far better to have the format
        // string last so it can pop arguments off the stack as the
        // vm traverses the format string. However its harder to have
        // the arguments, but the VM can at least read the arguments
        // in forward order, because all arguments are of type Value
    }

    int numArgs = 0;

    while (true) {
        if (match(TokenType::Dollar)) {
            // Usually the scanner will revert isString after a non-string token
            // has been produced. This flag will force it to continue to parse
            // strings after the identifier.
            scanner->isString = true;
            consume(TokenType::Identifier, "Expect identifier after '$'.");

            Token iden = previous();

            int local = compiler->resolveLocal(iden.text);
            if (local == -1) {
                errorAt(iden, "Couldn't find variable.");
                continue;
            }

            Type localType = compiler->locals[local].type;
            if (isBytecode) {
                vmWriter->namedVariable(local, false);
                if (localType == types::Number || localType == types::Bool) {
                    vmWriter->doubleToString();
                }
            } else {
                ast->variable(local);
                if (localType == types::Bool) {
                    ast->functionCallNative("_bool_to_string", 1);
                }
            }

            if (localType == types::Number) ss << "%f";
            else if (localType == types::String) ss << "%s";
            else if (localType == types::Bool) ss << "%s";
            else if (localType == types::VoidPtr) ss << "%p";
            else {
                error("Cannot format this type.");
            }

            numArgs ++;

        }
        else if (match(TokenType::String)) {
            // This stuff is kinda weird because in bytecode
            // mode we want literal strings so we need to
            // unescape what we read, but in regular mode
            // we want escaped strings
            if (previous().text == "\n") {
                ss << '\n';
            } else if (previous().text == "\\n") { 
                if (isBytecode) ss << '\n';
                else ss << previous().text;
            } else if (previous().text.size() == 2 && previous().text[0] == '\\') {
                if (isBytecode) ss << previous().text[1];
                else ss << previous().text;
            } else if (previous().text[previous().text.size() - 1] == '"') {
                ss << previous().text;
                break; // End quote
            } else {
                ss << previous().text;
            }
        } else {
            error("Unexpected token.");
        }
    }

    auto measureString = [](std::string_view view) {

        int minIndent = 1000;
        int currentLineIndent = 0;
        for (size_t i = 0; i < view.size(); i++) {

            if (view[i] == ' ') {
                currentLineIndent ++;
            } else if (view[i] != '\n' && view[i] != '\t') {
                minIndent = currentLineIndent < minIndent ? currentLineIndent : minIndent;
                if (minIndent == 0) return 0;
                currentLineIndent = 0;
                while (i < view.size() && view[i] != '\n') {
                    i ++;
                }
            } else if (view[i] == '\n') {
                currentLineIndent = 0;
            }

        }
        return minIndent == 1000 ? 0 : minIndent;
    };

    auto unindentString = [](std::string_view view, int indent) {
        std::ostringstream text;
        text << '\"';
        for (size_t i = 0; i < view.size(); i++) {
            if (view[i] == '\n') {
                text << '\\';
                text << 'n';
                if (indent > 0) {
                    for (int j = 0; j < indent && i < view.size(); j++, i++);
                }
            } else {
                text << view[i];
            }
        }
        text << '\"';
        return text.str();
    };

    std::string *heapString = new std::string(); // @leak
    if (spf.indented) {
        std::string_view withoutQuotes = ss.str();
        withoutQuotes.remove_prefix(1);
        withoutQuotes.remove_suffix(1);
        int indent = measureString(withoutQuotes);
        *heapString = unindentString(withoutQuotes, indent);
    } else {
        *heapString = ss.str();
    }

    if (isBytecode) {
        string.text = std::string_view(*heapString);
        vmWriter->string(string);
        vmWriter->stringFormat(numArgs);
    } else {
        stringExpr->name.text = std::string_view(*heapString);
        ast->stringFormat(numArgs);
    }

}

void Parser::at(ExpressionState es) {
    consume(TokenType::Identifier, "Expect identifier after '@'.");

    if (previous().text == "string") {
        bool indented = false;
        consume(TokenType::LeftParen, "Expect '(' after 'string'.");

        while (true) {
            consume(TokenType::Identifier, "Expect identifier after '('.");
            if (previous().text == "indented") indented = true;

            if (match(TokenType::Comma)) continue;
            consume(TokenType::RightParen, "Expect ',' or ')' after identifier.");
            break;

        }

        consume(TokenType::String, "Expect string literal.");
        StringParseFlags spf;
        spf.indented = indented;
        stringAdvanced(spf);
    }
}

void Parser::and_(ExpressionState es) {
    Token andToken = previous();

    WhileStatementPatchState patchState;
    if (isBytecode) { patchState = vmWriter->beginAndExpression(); }

    parsePrecedence(Precedence::And);

    if (isBytecode) vmWriter->endAndCondition(&patchState);
    else ast->infix(andToken);

    typecheckAnd(this);

}

void Parser::or_(ExpressionState es) {
    Token orToken = previous();
    
    IfStatementPatchState patchState;
    if (isBytecode) { patchState = vmWriter->beginOrExpression(); }

    parsePrecedence(Precedence::Or);
    if (isBytecode) vmWriter->endOrCondition(&patchState);
    else ast->infix(orToken);

    typecheckOr(this);

}

void Parser::dot(ExpressionState es) {
    consume(TokenType::Identifier, "Expect property name after '.'.");
    Token property = previous();
    // uint8_t name = identifierConstant(previous().text);

    typecheckPropertyAccess(this, property.text);
    if (!isBytecode) {
        ast->property(property);
    }
  
    if (es.canAssign && match(TokenType::Equal)) {
        expression();
        typecheckAssignExpression(this);
        if (!isBytecode) {
            ast->assignment();
        }
        // emitBytes(OP_SET_PROPERTY, name); // TODO:
    } else {
        // emitBytes(OP_GET_PROPERTY, name);
    }
}


void Parser::variable(ExpressionState es) {
    namedVariable(previous().text, es);
}

void Parser::namedVariable(const std::string_view &name, ExpressionState es) {
    Token nameToken = previous();

    int arg = compiler->resolveLocal(name);
    if (arg == -2) {
        error("Can't read local variable in its own initializer.");
    }
    if (arg != -1) {
        typecheckVariable(this, arg);
        Local &local = compiler->locals[arg];

        if (!isBytecode) {
            if (local.isStatic) vmWriter->namedVariable(arg, false);
            else ast->variable(arg);
        }

        if (es.canAssign && match(TokenType::Equal)) {
            expression();
            typecheckAssignExpression(this);
            if (isBytecode) vmWriter->namedVariable(arg, true);
            else ast->assignment();
        } else {
            if (isBytecode) vmWriter->namedVariable(arg, false);
        }
        return;
    }
    
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
        return;
    }

    error("No variable found.");
}

ParseRule rules[] = {
    {&Parser::grouping, &Parser::callLambda, Precedence::Call},     // LeftParen
    {nullptr,           nullptr,           Precedence::None},       // RightParen
    {nullptr,           nullptr,           Precedence::None},       // LeftBrace
    {nullptr,           nullptr,           Precedence::None},       // RightBrace
    {&Parser::arraylit, &Parser::arrayget, Precedence::Call},       // LeftSquare
    {nullptr,           nullptr,           Precedence::None},       // RightSquare
    {nullptr,           nullptr,           Precedence::None},       // Comma
    {nullptr,           &Parser::dot,      Precedence::Call},       // Dot
    {&Parser::unary,    &Parser::binary,   Precedence::Term},       // Minus
    {nullptr,           &Parser::binary,   Precedence::Term},       // Plus
    {nullptr,           nullptr,           Precedence::None},       // Semicolon
    {nullptr,           &Parser::binary,   Precedence::Factor},     // Slash
    {nullptr,           &Parser::binary,   Precedence::Factor},     // Star
    {nullptr,           nullptr,           Precedence::None},       // Colon
    {&Parser::at,       nullptr,           Precedence::None},       // At
    {nullptr,           nullptr,           Precedence::None},       // Dollar
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
    {nullptr,           nullptr,           Precedence::None},       // Struct
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
    {&Parser::lambda,   nullptr,           Precedence::None},       // Block
    {nullptr,           nullptr,           Precedence::None},       // Static
    {nullptr,           nullptr,           Precedence::None},       // Newline
    {nullptr,           nullptr,           Precedence::None},       // Error
    {nullptr,           nullptr,           Precedence::None},       // Eof
};

// Make sure we haven't missed any
static_assert(sizeof(rules) / sizeof(ParseRule) == static_cast<long>(TokenType::EOF_) + 1);

static ParseRule &getRule(TokenType type) {
    return rules[static_cast<int>(type)];
}