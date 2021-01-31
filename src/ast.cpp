#include <sstream>

using std::endl;

struct Expr;
struct Declaration;

struct FunctionCallAst {
    FunctionDeclaration *functionDeclaration;
    FunctionInstantiation instantiation;
    int argCount;
    std::vector<Expr*> arguments;
};
struct FunctionCallNative {
    std::string name;
    std::vector<Expr*> arguments;
};

struct InfixExpr {
    Token operatorToken;
    Expr *left = nullptr;
    Expr *right = nullptr;
};
struct PropertyExpr {
    Expr *left = nullptr;
    Token property;
};

struct StringLiteral {
    Token name;
};
struct NumberLiteral {
    Token name;
};
struct BooleanLiteral {
    bool value;
};

struct AssignmentExpr {
    Expr *left;
    Expr *value = nullptr;
    TokenType operatorType;
};
struct VariableLiteral {
    std::string_view name;
    //we may have renamed
    // TODO: token info here?
};
struct TypeLiteral {
    Type type;
};
struct ArrayLiteral {
    Type elementType;
    Expr **elements;
    int numElements;
};
struct Unit {
};

struct Expr {
    Type type;

    mpark::variant<FunctionCallAst, FunctionCallNative, InfixExpr, PropertyExpr, StringLiteral, NumberLiteral,
        BooleanLiteral, AssignmentExpr, VariableLiteral, ArrayLiteral, TypeLiteral, Unit> variant;
};

struct FunDeclaration {
    // FunctionDeclaration *functionDeclaration = nullptr;
    Declaration *firstDeclaration = nullptr;
};
struct FunInstantiation {
    FunDeclaration *decl;
    FunctionInstantiation *inst;
};
struct VarDeclaration {
    std::string_view name;
    Type type;
    Expr *value = nullptr;
};

struct PrintStatement {
    Expr *argument = nullptr;
};
struct IfStatement {
    Expr *expr = nullptr;
    Declaration *firstDeclaration = nullptr;
    Declaration *firstElseDeclaration = nullptr;
};
struct ReturnStatement {
    Expr *expr = nullptr;
};
struct WhileStatement {
    Expr *expr = nullptr;
    Declaration *firstDeclaration = nullptr;
};
struct Block {
    Declaration *firstDeclaration = nullptr;
};
struct ExprStatement {
    Expr *expr = nullptr;
};
struct GotoStatement {
    std::string label;
};
struct LabelStatement {
    std::string label;
};

struct Statement {
    mpark::variant<PrintStatement, IfStatement, ReturnStatement,
        WhileStatement, Block, ExprStatement, GotoStatement,
        LabelStatement> variant;
};

struct Declaration {
    Declaration *nextSibling = nullptr;

    mpark::variant<FunDeclaration, VarDeclaration, Statement> variant;
};

struct CompileOptions {
    bool noPrint = false;
};

template <class T, class U>
T *makeVariant(U *decl) {
    decl->variant = T {};
    return &mpark::get<T>(decl->variant);
}

struct AstBlockScope {
    AstBlockScope* prevBlock = nullptr;

    Declaration *initialDeclaration = nullptr;
    Declaration *lastDeclaration = nullptr;
    Declaration **nextDeclarationPtr = nullptr;

    AstBlockScope(AstBlockScope *prevBlock, Declaration **nextDeclPtr): 
        prevBlock(prevBlock), nextDeclarationPtr(nextDeclPtr) {}
};

struct AstGen {
    // std::array<Declaration, 256> allDecls;
    // size_t numDecls = 0;
    // std::array<Expr, 1024> allExprs;
    // size_t numExprs = 0;


    std::vector<Expr*> expressionStack;
    // std::vector<Declaration*> declarationStack;
    // std::vector<Declaration**> nextDeclarationStack;

    AstBlockScope* currentBlock;
    // std::vector<AstBlockScope> ;

    std::vector<Type> structDeclarations;

    std::vector<FunInstantiation> functionInstantiations;

    Parser *parser = nullptr;
    Declaration *initialDeclaration = nullptr;

    // Declaration *lastDeclaration = nullptr;

    AstGen() {
        // blockStack.push_back({});

        currentBlock = new AstBlockScope(nullptr, &initialDeclaration);

    }

    Expr *newExpr() {
        // if (numExprs > 1024) exit(0);
        // Expr *expr = &allExprs[numExprs];
        Expr *expr = new Expr;
        // numExprs ++;
        return expr;
    }

    Declaration *pushNewDeclaration() {
        // if (numDecls > 256) exit(0);
        // Declaration *decl = &allDecls[numDecls];
        Declaration *decl = new Declaration;
        // numDecls ++;
        if (!currentBlock->initialDeclaration) { currentBlock->initialDeclaration = decl; }
        currentBlock->lastDeclaration = decl;

        *currentBlock->nextDeclarationPtr = decl;
        currentBlock->nextDeclarationPtr = &decl->nextSibling;
        return decl;
    }

    Expr *popExpression() {
        assert(expressionStack.size() > 0);
        Expr *expr = expressionStack.back();
        expressionStack.pop_back();
        return expr;
    }

    void reverseLastNumExpressions(size_t num) {
        // :ReverseStack
        assert(expressionStack.size() >= num);
        std::reverse(expressionStack.end() - num, expressionStack.end());
    }

    // Expressions

    void string(Token name) {
        auto expr = newExpr();
        auto lit = makeVariant<StringLiteral>(expr);
        expr->type = parser->compiler->expressionTypeStack.back();
        lit->name = name;
        expressionStack.push_back(expr);
    }
    void number(Token name) {
        auto expr = newExpr();
        auto lit = makeVariant<NumberLiteral>(expr);
        expr->type = parser->compiler->expressionTypeStack.back();
        lit->name = name;
        expressionStack.push_back(expr);
    }
    void variable(Local *local) {
        assert(parser->compiler->expressionTypeStack.back() == local->type);
        auto expr = newExpr();
        auto var = makeVariant<VariableLiteral>(expr);
        expr->type = parser->compiler->expressionTypeStack.back();
        var->name = local->renamedTo.size() ? local->renamedTo : local->name;
        expressionStack.push_back(expr);
    }
    void booleanLiteral(bool value) {
        auto expr = newExpr();
        auto b = makeVariant<BooleanLiteral>(expr);
        expr->type = parser->compiler->expressionTypeStack.back();
        b->value = value;
        expressionStack.push_back(expr);
    }
    void unit() {
        auto expr = newExpr();
        auto b = makeVariant<Unit>(expr);
        expr->type = parser->compiler->expressionTypeStack.back();
        expressionStack.push_back(expr);
    }

    void property(Token property) {
        auto expr = newExpr();
        auto prop = makeVariant<PropertyExpr>(expr);
        prop->left = popExpression();
        prop->property = property;
        expr->type = parser->compiler->expressionTypeStack.back();
        expressionStack.push_back(expr);
    }
    void assignment(TokenType operatorType) {
        auto expr = newExpr();
        auto assgn = makeVariant<AssignmentExpr>(expr);
        assgn->value = popExpression();
        assgn->left = popExpression();
        assgn->operatorType = operatorType;
        expr->type = parser->compiler->expressionTypeStack.back();
        expressionStack.push_back(expr);
    }

    void stringFormat(int numArgs) {
        functionCallNative("_make_string", numArgs + 1);
    }

    void stringEquals(bool isEqual) {
        functionCallNative("strcmp", 2);

        // I wish we could just give it text directly
        Token numberToken;
        std::string *text = new std::string(); // @leak
        if (isEqual) *text = "0";
        else *text = "1";
        numberToken.text = *text;
        number(numberToken);

        Token operatorToken;
        std::string *operatorText = new std::string("=="); // @leak
        operatorToken.text = *operatorText;
        operatorToken.type = TokenType::EqualEqual;
        infix(operatorToken);
    }

    void infix(Token operatorToken) {
        auto expr = newExpr();
        Expr *right = popExpression();
        Expr *left = popExpression();
        auto infix = makeVariant<InfixExpr>(expr);
        expr->type = parser->compiler->expressionTypeStack.back();
        infix->operatorToken = operatorToken;
        infix->left = left;
        infix->right = right;
        expressionStack.push_back(expr);
    }

    void functionCall(FunctionInstantiation inst, int argCount) {
        std::vector<Expr*> args;
        for (int i = 0; i < argCount; i++) {
            args.insert(args.begin(), 1, popExpression());
        }
        auto expr = newExpr();
        expr->type = parser->compiler->expressionTypeStack.back();
        auto call = makeVariant<FunctionCallAst>(expr);
        call->argCount = argCount;
        call->arguments = args;
        call->instantiation = inst;
        call->functionDeclaration = inst.declaration;
        expressionStack.push_back(expr);
    }
    void functionCallNative(std::string name, int argCount) {
        std::vector<Expr*> args;
        for (int i = 0; i < argCount; i++) {
            args.insert(args.begin(), 1, popExpression());
        }
        auto expr = newExpr();
        expr->type = parser->compiler->expressionTypeStack.back();
        auto call = makeVariant<FunctionCallNative>(expr);
        call->arguments = args;
        call->name = name;
        expressionStack.push_back(expr);
    }

    void arrayLiteral(int numElements, Type elementType) {
        auto expr = newExpr();
        expr->type = parser->compiler->expressionTypeStack.back();
        auto array = makeVariant<ArrayLiteral>(expr);
        array->elementType = elementType;
        array->numElements = numElements;
        array->elements = new Expr*[numElements];
        assert(expressionStack.size() >= (size_t)numElements);
        for (int i = 0; i < numElements; i++) {
            array->elements[i] = expressionStack[expressionStack.size() - numElements + i];
        }
        for (int i = 0; i < numElements; i++) {
            popExpression();
        }
        expressionStack.push_back(expr);
    }

    void typeLiteral(Type type) {
        auto expr = newExpr();
        expr->type = parser->compiler->expressionTypeStack.back();
        auto typeLit = makeVariant<TypeLiteral>(expr);
        typeLit->type = type;
        expressionStack.push_back(expr);
    }
    // Declarations

    void exprStatement() {
        auto decl = pushNewDeclaration();
        auto stmt = makeVariant<Statement>(decl);
        auto exprStmt = makeVariant<ExprStatement>(stmt);
        exprStmt->expr = popExpression();
    }

    void returnStatement(bool returnedValue) {
        auto decl = pushNewDeclaration();
        auto stmt = makeVariant<Statement>(decl);
        auto retStmt = makeVariant<ReturnStatement>(stmt);
        retStmt->expr = returnedValue ? popExpression() : nullptr;
    }

    void gotoStatement(std::string label) {
        auto decl = pushNewDeclaration();
        auto stmt = makeVariant<Statement>(decl);
        auto gotoStmt = makeVariant<GotoStatement>(stmt);
        gotoStmt->label = label;
    }

    void labelStatement(std::string label) {
        auto decl = pushNewDeclaration();
        auto stmt = makeVariant<Statement>(decl);
        auto labelStmt = makeVariant<LabelStatement>(stmt);
        labelStmt->label = label;
    }

    IfStatement *beginIfStatementBlock() {
        auto decl = pushNewDeclaration();
        auto stmt = makeVariant<Statement>(decl);
        auto ifStmt = makeVariant<IfStatement>(stmt);
        ifStmt->expr = popExpression();

        currentBlock = new AstBlockScope(currentBlock, &ifStmt->firstDeclaration);
        return ifStmt;
    }
    void beginElseBlock(IfStatement *ifStmt) {
        currentBlock = new AstBlockScope(currentBlock, &ifStmt->firstElseDeclaration);
    }

    void beginWhileStatementBlock() {
        auto decl = pushNewDeclaration();
        auto stmt = makeVariant<Statement>(decl);
        auto whileStmt = makeVariant<WhileStatement>(stmt);
        whileStmt->expr = popExpression();
        
        currentBlock = new AstBlockScope(currentBlock, &whileStmt->firstDeclaration);
    }
    
    void varDeclaration(Local &local, bool initializer) {
        auto decl = pushNewDeclaration();
        auto varDecl = makeVariant<VarDeclaration>(decl);
        varDecl->name = local.renamedTo.size() ? std::string_view(local.renamedTo) : local.name;
        varDecl->value = initializer ? popExpression() : nullptr;
        varDecl->type = local.type;
        assert(varDecl->type);
    }

    void print() {
        auto decl = pushNewDeclaration();
        auto stmt = makeVariant<Statement>(decl);
        auto print = makeVariant<PrintStatement>(stmt);
        print->argument = popExpression();
    }

    void beginBlock() {
        auto decl = pushNewDeclaration();
        auto stmt = makeVariant<Statement>(decl);
        auto block = makeVariant<Block>(stmt);
        currentBlock = new AstBlockScope(currentBlock, &block->firstDeclaration);
    }
    void endBlock() {
        assert(currentBlock->prevBlock);
        auto oldBlock = currentBlock;
        currentBlock = currentBlock->prevBlock;
        delete oldBlock;
    }

    void beginFunctionDeclaration(FunctionInstantiation *inst) {
        auto decl = pushNewDeclaration();
        auto func = makeVariant<FunDeclaration>(decl);
        functionInstantiations.push_back({func, inst});
        currentBlock = new AstBlockScope(currentBlock, &func->firstDeclaration);
    }
    void endFunctionDeclaration() {
        assert(currentBlock->prevBlock);
        auto oldBlock = currentBlock;
        currentBlock = currentBlock->prevBlock;
        delete oldBlock;
    }

    void structDeclaration(Type type) {
        assert(type->isStruct());
        structDeclarations.push_back(type);
    }

    void makeImplicitReturn() {
        // We want to remove the last declaration, and if it's an
        // expression statement put the expression on the stack
        // Is there another way, instead of rewriting - never
        // actually write the final declaration, but return it
        // to the compiler so it can be pushed directly.

        // BTW this doesn't work properly if there are nested
        // blocks at the last position in the function

        auto stmt = mpark::get_if<Statement>(&currentBlock->lastDeclaration->variant);
        if (!stmt) { unit(); return; }
        auto exprStmt = mpark::get_if<ExprStatement>(&stmt->variant);
        if (!exprStmt) { unit(); return; }

        Expr *returnExpr = exprStmt->expr;
        expressionStack.push_back(returnExpr);

        // Quick go at removing last element. Maybe doubley linked
        // list in the future? Finds second to last element
        Declaration *decl = currentBlock->initialDeclaration;
        while (decl && decl->nextSibling && decl->nextSibling->nextSibling) {
            decl = decl->nextSibling;
        }
        assert(currentBlock->lastDeclaration == decl->nextSibling);
        // delete decl->nextSibling; @leak
        decl->nextSibling = nullptr;

        currentBlock->nextDeclarationPtr = &decl->nextSibling;


    }
};


const char *tabs[] = {
    "", "  ", "    ", "      ", "        ",
};

std::string typeToString(Type type);

struct CodeGen {

    Parser *parser = nullptr;
    CompileOptions compileOptions;
    std::ostringstream &ss;
    int indent = 0;

    CodeGen(CompileOptions compileOptions, std::ostringstream &ss): 
        compileOptions(compileOptions), ss(ss) {}

    void addIndent() {
        int x = indent;
        while (x > 4) {
            ss << tabs[4];
            x -= 4;
        }
        ss << tabs[x];
    }

    void addTypeName(Type type) {
        ss << typeToString(type);
    }

    void addFunctions(std::vector<FunInstantiation> &insts) {
        for (auto instAst : insts) {
            auto functionType = instAst.inst->type->functionTypeData();
            Type returnType = functionType->returnType;

            addTypeName(returnType);
            ss << " " << instAst.inst->renamedTo << "(";
            size_t i = 0;
            for (auto param : instAst.inst->declaration->parameters) {
                addTypeName(functionType->parameterTypes[i]);
                ss << " " << param.name;
                if (i < instAst.inst->declaration->parameters.size() - 1) {
                    ss << ", ";
                }
                i++;
            }
            ss << ") {" << endl;
            indent ++;
            addDecls(instAst.decl->firstDeclaration);
            indent --;
            ss << "}";
            ss << endl << endl;
            
        }
    }

    void addStructDeclarations(std::vector<Type> & structs) {
        for (auto stru : structs) {
            auto structData = stru->structTypeData();
            ss << "typedef struct " << structData->name << " {" << endl;
            indent ++;
            for (auto &member: structData->members) {
                addIndent();
                addTypeName(member.type);
                ss << " " << member.name << ";" << endl;

            }
            indent --;
            ss << "} " << structData->name << ";" << endl << endl;
        }
    }

    void addExpr(Expr *expr, bool parens = true) {
        rollbear::visit(overloaded {
            [&](InfixExpr &infix) {
                if (parens) ss << "(";
                addExpr(infix.left);
                ss << " " << infix.operatorToken.text << " ";
                addExpr(infix.right);
                if (parens) ss << ")";
            },
            [&](PropertyExpr &ex) {
                addExpr(ex.left);
                ss << ".";
                ss << ex.property.text;
            },
            [&](StringLiteral &ex) {
                ss << ex.name.text;
            },
            [&](NumberLiteral &ex) {
                ss << ex.name.text;
            },
            [&](BooleanLiteral &ex) {
                ss << (ex.value ? "true" : "false");
            },
            [&](AssignmentExpr &ex) {
                addExpr(ex.left);
                if (ex.operatorType == TokenType::Equal) {
                    ss << " = ";
                } else if (ex.operatorType == TokenType::PlusEqual) {
                    ss << " += ";
                } else if (ex.operatorType == TokenType::MinusEqual) {
                    ss << " -= ";
                } else if (ex.operatorType == TokenType::StarEqual) {
                    ss << " *= ";
                } else if (ex.operatorType == TokenType::SlashEqual) {
                    ss << " /= ";
                }
                addExpr(ex.value, false);
            },
            [&](VariableLiteral &ex) {
                ss << ex.name;
            },
            [&](TypeLiteral &ty) {
                addTypeName(ty.type);
            },
            [&](Unit &u) {},
            [&](FunctionCallAst &call) {
                ss << call.instantiation.renamedTo;
                ss << "(";
                size_t i = 0; 
                for (Expr *arg : call.arguments) {
                    if (i > 0) {
                        ss << ", ";
                    }
                    addExpr(arg, false);
                    i++;
                }
                ss << ")";
            },
            [&](FunctionCallNative &call) {
                ss << call.name;
                ss << "(";
                size_t i = 0; 
                for (Expr *arg : call.arguments) {
                    if (i > 0) {
                        ss << ", ";
                    }
                    addExpr(arg, false);
                    i++;
                }
                ss << ")";
            },
            [&](ArrayLiteral &array) {
                ss << "_new_array_from_literal(";
                ss << array.numElements << ", ";
                ss << array.numElements << ", ";
                ss << "sizeof(";
                addTypeName(array.elementType);
                ss << "), ((";
                addTypeName(array.elementType);
                ss << "[" << array.numElements << "]){";
                for (int i = 0; i < array.numElements; i++) {
                    if (i > 0) {
                        ss << ", ";
                    }
                    addExpr(array.elements[i]);
                }
                ss << "}))";
            },
        }, expr->variant);
    }

    void addStmt(Statement *stmt) {
        rollbear::visit(overloaded {
            [&](PrintStatement &print) {
                ss << "printf(\"";
                Type type = print.argument->type;
                if (type == types::Number) ss << "%f";
                else if (type == types::String) ss << "%s";
                else if (type == types::Bool) ss << "%s";
                else if (type == types::VoidPtr) ss << "%p";
                else if (type->isStruct()) ss << "%s";
                else if (type->isFunction()) ss << "%s";
                else if (type->isArray()) ss << "%s";
                else {
                    assert(0 && "Printing not implemented for this type");
                }
                ss << "\\n\", ";
                if (type == types::Bool) { 
                    ss << "("; addExpr(print.argument, false); ss << " ? \"true\" : \"false\")";
                } else if (type->isStruct()) { ss << "\"<struct " + type->structTypeData()->name + ">\"";
                } else if (type->isFunction()) { ss << "\"<function>\"";
                } else if (type->isArray()) { ss << "\"<array>\"";
                } else { addExpr(print.argument, false); }
                ss << ")";
                ss << ";" << endl;
            },
            [&](IfStatement &ifStmt) {
                ss << "if (";
                addExpr(ifStmt.expr, false);
                ss << ") {" << endl;
                indent ++; addDecls(ifStmt.firstDeclaration); indent --;
                if (ifStmt.firstElseDeclaration) {
                    addIndent();
                    ss << "} else {" << endl;
                    indent ++; addDecls(ifStmt.firstElseDeclaration); indent --;
                }
                addIndent();
                ss << "}" << endl;

            },
            [&](ReturnStatement &ret) {
                ss << "return ";
                if (ret.expr) addExpr(ret.expr, false);
                ss << ";" << endl;
            },
            [&](GotoStatement &gotoStmt) {
                ss << "goto " << gotoStmt.label << ";" << endl;
            },
            [&](LabelStatement &labelStmt) {
                ss << labelStmt.label << ": ;" << endl;
            },
            [&](WhileStatement &whileStmt) {
                ss << "while (";
                addExpr(whileStmt.expr, false);
                ss << ") {" << endl;
                indent ++; addDecls(whileStmt.firstDeclaration); indent --;
                addIndent();
                ss << "}" << endl;
            },
            [&](Block &block) {
                ss << "{" << endl;
                indent ++;
                addDecls(block.firstDeclaration);
                indent --;
                addIndent();
                ss << "}" << endl;
            },
            [&](ExprStatement &exprStmt) {
                addExpr(exprStmt.expr);
                ss << ";" << endl;
            }
        }, stmt->variant);

    }

    void addDecl(Declaration *decl) {
        rollbear::visit(overloaded {
            [&](FunDeclaration &fun) {
                // Do nothing
            },
            [&](VarDeclaration &var) {
                addIndent();
                addTypeName(var.type);
                ss << " ";
                ss << var.name;
                if (var.value) {
                    ss << " = ";
                    addExpr(var.value, false);
                }
                ss << ";" << endl;
            },
            [&](Statement &stmt) {
                addIndent();
                addStmt(&stmt);
            }
        }, decl->variant);
    }

    void addTypedefs() {
        ss << "#include <time.h>" << endl;
        ss << "#include <stdio.h>" << endl;
        ss << "#include <stdlib.h>" << endl;
        ss << "#include <string.h>" << endl;
        ss << "#include <stdarg.h>" << endl;
        ss << "#define true 1" << endl;
        ss << "#define false 0" << endl;
        ss << "typedef int bool;" << endl;
        ss << "typedef const char* string;" << endl;
        ss << "typedef void* voidptr;" << endl;
        ss << "double clock_seconds() { return (double)clock() / CLOCKS_PER_SEC; }" << endl;
        ss << "string alloc_string(int length) { return (string)malloc(length * sizeof(string)); }" << endl;
        ss << "string string_concat(string a, string b) { char *dst = (char*)malloc((strlen(a) + strlen(b)) * sizeof(string)); strcpy(dst, a); strcat(dst, b); return dst; }" << endl;
        ss << "#define _array_index(a, i, type) *(type*)(a.data + (int)i * (int)a.elem_size)" << endl;
        ss << "#define _bool_to_string(b) ((b) ? \"true\" : \"false\")" << endl;
        ss << "void assert_failed(int line) { printf(\"Assertion failed line %i\\n\", line); exit(1); }" << endl;
        ss << "#define assert(b) if (!(b)) assert_failed(__LINE__)" << endl;

        if (compileOptions.noPrint) {
            ss << "#define printf(...)" << endl;
        }

        // TODO: Make this better
        ss << "const char *_make_string(const char *format, ...) {";
        ss << "    char *str = malloc(1024); ";
        ss << "    va_list argptr;";
        ss << "    va_start(argptr, format);";
        ss << "    vsnprintf(str, 1024, format, argptr);";
        ss << "    va_end(argptr);";
        ss << "    str[1024 - 1] = '\\0';";
        ss << "    return str;";
        ss << "}" << endl;
        ss << endl;
    }
    
    void addDecls(Declaration *initial) {
        Declaration *decl = initial;
        while (decl) {
            addDecl(decl);
            decl = decl->nextSibling;
        }
    }

    void addMain(Declaration *initial) {
        ss << "int main(int argc, char *argv[]) {" << endl;
        indent ++;
        addDecls(initial);
        indent --;
        ss << "  return 0;" << endl;
        ss << "}";
    }
};

void generateCodeC(CompileOptions compileOptions, AstGen *astGen, std::ostringstream &stream) {
    CodeGen codeGen(compileOptions, stream);
    codeGen.parser = astGen->parser;
    
    codeGen.addTypedefs();
    codeGen.addStructDeclarations(astGen->structDeclarations);
    codeGen.addFunctions(astGen->functionInstantiations);
    codeGen.addMain(astGen->initialDeclaration);

}