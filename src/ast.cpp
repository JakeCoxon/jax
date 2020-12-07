#include <sstream>

using std::endl;

struct Expr;
struct Declaration;

struct FunctionCall {
    FunctionDeclaration *functionDeclaration;
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
};
struct VariableLiteral {
    Token name;
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

    mpark::variant<FunctionCall, FunctionCallNative, InfixExpr, PropertyExpr, StringLiteral, NumberLiteral,
        BooleanLiteral, AssignmentExpr, VariableLiteral, ArrayLiteral, TypeLiteral, Unit> variant;
};

struct FunDeclaration {
    // FunctionDeclaration *functionDeclaration = nullptr;
    Declaration *firstDeclaration = nullptr;
};
struct FunInstantiation {
    FunDeclaration *decl;
    FunctionInstantiation inst;
};
struct VarDeclaration {
    Token name;
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

struct Statement {
    mpark::variant<PrintStatement, IfStatement, ReturnStatement, WhileStatement, Block, ExprStatement> variant;
};

struct Declaration {
    Declaration *nextSibling = nullptr;

    mpark::variant<FunDeclaration, VarDeclaration, Statement> variant;
};

template <class T, class U>
T *makeVariant(U *decl) {
    decl->variant = T {};
    return &mpark::get<T>(decl->variant);
}

struct AstGen {
    std::vector<Expr*> expressionStack;
    std::vector<Declaration*> declarationStack;
    std::vector<Declaration**> nextDeclarationStack;

    std::vector<Type> structDeclarations;

    std::vector<FunInstantiation> functionInstantiations;

    Parser *parser = nullptr;
    Declaration *initialDeclaration = nullptr;
    Declaration *lastDeclaration = nullptr;

    AstGen() {
        nextDeclarationStack.push_back(&initialDeclaration);
    }


    Expr *popExpression() {
        assert(expressionStack.size() > 0);
        Expr *expr = expressionStack.back();
        expressionStack.pop_back();
        return expr;
    }


    void string(Token name) {
        auto expr = new Expr;
        auto lit = makeVariant<StringLiteral>(expr);
        expr->type = parser->compiler->expressionTypeStack.back();
        lit->name = name;
        expressionStack.push_back(expr);
    }
    void number(Token name) {
        auto expr = new Expr;
        auto lit = makeVariant<NumberLiteral>(expr);
        expr->type = parser->compiler->expressionTypeStack.back();
        lit->name = name;
        expressionStack.push_back(expr);
    }
    void variable(Token name) {
        auto expr = new Expr;
        auto var = makeVariant<VariableLiteral>(expr);
        expr->type = parser->compiler->expressionTypeStack.back();
        var->name = name;
        expressionStack.push_back(expr);
    }
    void booleanLiteral(bool value) {
        auto expr = new Expr;
        auto b = makeVariant<BooleanLiteral>(expr);
        expr->type = parser->compiler->expressionTypeStack.back();
        b->value = value;
        expressionStack.push_back(expr);
    }
    void unit() {
        auto expr = new Expr;
        auto b = makeVariant<Unit>(expr);
        expr->type = parser->compiler->expressionTypeStack.back();
        expressionStack.push_back(expr);
    }

    void property(Token property) {
        auto expr = new Expr;
        auto prop = makeVariant<PropertyExpr>(expr);
        prop->left = popExpression();
        prop->property = property;
        expr->type = parser->compiler->expressionTypeStack.back();
        expressionStack.push_back(expr);
    }
    void assignment() {
        auto expr = new Expr;
        auto assgn = makeVariant<AssignmentExpr>(expr);
        assgn->value = popExpression();
        assgn->left = popExpression();
        expr->type = parser->compiler->expressionTypeStack.back();
        expressionStack.push_back(expr);
    }

    void stringFormat(int numArgs) {
        functionCallNative("_make_string", numArgs + 1);
    }

    void infix(Token operatorToken) {
        auto expr = new Expr;
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
        auto expr = new Expr();
        expr->type = parser->compiler->expressionTypeStack.back();
        auto call = makeVariant<FunctionCall>(expr);
        call->argCount = argCount;
        call->arguments = args;
        call->functionDeclaration = inst.declaration;
        expressionStack.push_back(expr);
    }
    void functionCallNative(std::string name, int argCount) {
        std::vector<Expr*> args;
        for (int i = 0; i < argCount; i++) {
            args.insert(args.begin(), 1, popExpression());
        }
        auto expr = new Expr();
        expr->type = parser->compiler->expressionTypeStack.back();
        auto call = makeVariant<FunctionCallNative>(expr);
        call->arguments = args;
        call->name = name;
        expressionStack.push_back(expr);
    }

    void arrayLiteral(int numElements, Type elementType) {
        auto expr = new Expr();
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
        auto expr = new Expr();
        expr->type = parser->compiler->expressionTypeStack.back();
        auto typeLit = makeVariant<TypeLiteral>(expr);
        typeLit->type = type;
        expressionStack.push_back(expr);
    }
    // Declarations

    void exprStatement() {
        auto decl = new Declaration;
        auto stmt = makeVariant<Statement>(decl);
        auto exprStmt = makeVariant<ExprStatement>(stmt);
        exprStmt->expr = popExpression();
        lastDeclaration = decl;

        *nextDeclarationStack.back() = decl;
        nextDeclarationStack.back() = &decl->nextSibling;
    }

    void returnStatement(bool isNil) {
        auto decl = new Declaration;
        auto stmt = makeVariant<Statement>(decl);
        auto retStmt = makeVariant<ReturnStatement>(stmt);
        retStmt->expr = isNil ? nullptr : popExpression();
        lastDeclaration = decl;

        *nextDeclarationStack.back() = decl;
        nextDeclarationStack.back() = &decl->nextSibling;
    }

    IfStatement *beginIfStatementBlock() {
        auto decl = new Declaration;
        auto stmt = makeVariant<Statement>(decl);
        auto ifStmt = makeVariant<IfStatement>(stmt);
        ifStmt->expr = popExpression();
        lastDeclaration = decl;

        *nextDeclarationStack.back() = decl;
        nextDeclarationStack.back() = &decl->nextSibling;

        nextDeclarationStack.push_back(&ifStmt->firstDeclaration);
        return ifStmt;
    }
    void beginElseBlock(IfStatement *ifStmt) {
        nextDeclarationStack.push_back(&ifStmt->firstElseDeclaration);
    }

    void beginWhileStatementBlock() {
        auto decl = new Declaration;
        auto stmt = makeVariant<Statement>(decl);
        auto whileStmt = makeVariant<WhileStatement>(stmt);
        whileStmt->expr = popExpression();
        lastDeclaration = decl;

        *nextDeclarationStack.back() = decl;
        nextDeclarationStack.back() = &decl->nextSibling;
        nextDeclarationStack.push_back(&whileStmt->firstDeclaration);
    }
    
    void varDeclaration(Token name, Type type, bool initializer) {
        auto decl = new Declaration;
        auto varDecl = makeVariant<VarDeclaration>(decl);
        varDecl->name = name;
        varDecl->value = initializer ? popExpression() : nullptr;
        varDecl->type = type;
        lastDeclaration = decl;
        // declarationStack.push_back(decl);

        *nextDeclarationStack.back() = decl;
        nextDeclarationStack.back() = &decl->nextSibling;
    }

    void print() {
        auto decl = new Declaration;
        auto stmt = makeVariant<Statement>(decl);
        auto print = makeVariant<PrintStatement>(stmt);
        print->argument = popExpression();
        // declarationStack.push_back(decl);
        lastDeclaration = decl;

        *nextDeclarationStack.back() = decl;
        nextDeclarationStack.back() = &decl->nextSibling;
    }

    void beginBlock() {
        auto decl = new Declaration;
        auto stmt = makeVariant<Statement>(decl);
        auto block = makeVariant<Block>(stmt);
        // declarationStack.push_back(decl);
        lastDeclaration = decl;
        *nextDeclarationStack.back() = decl;
        nextDeclarationStack.back() = &decl->nextSibling;
        nextDeclarationStack.push_back(&block->firstDeclaration);
    }
    void endBlock() {
        assert(nextDeclarationStack.size());
        nextDeclarationStack.pop_back();
    }

    void beginFunctionDeclaration(FunctionInstantiation inst) {
        auto decl = new Declaration;
        auto func = makeVariant<FunDeclaration>(decl);
        functionInstantiations.push_back({func, inst});
        lastDeclaration = decl;
        // declarationStack.push_back(decl);
        *nextDeclarationStack.back() = decl;
        nextDeclarationStack.back() = &decl->nextSibling;
        nextDeclarationStack.push_back(&func->firstDeclaration);
    }
    void endFunctionDeclaration() {
        assert(nextDeclarationStack.size());
        nextDeclarationStack.pop_back();
    }

    void structDeclaration(Type type) {
        assert(type->isStruct());
        structDeclarations.push_back(type);
    }

    void makeLambda() {
        // For now just put nothing, but eventually we probably
        // want to remove the last declaration, and if it's an
        // expression statement put the expression on the stack
        // Is there another way, instead of rewriting - never
        // actually write the final declaration, but return it
        // to the compiler so it can be pushed directly.
        unit();


        // auto stmt = mpark::get_if<Statement>(lastDeclaration.variant);
        // if (!stmt) return;
        // auto expr = mpark::get_if<Statement>(lastDeclaration.variant);
        // if ()
    }
};


const char *tabs[] = {
    "", "  ", "    ", "      ", "        ", "          "
};

struct CodeGen {

    Parser *parser = nullptr;
    std::ostringstream &ss;
    int indent = 0;

    CodeGen(std::ostringstream &ss): ss(ss) {}

    void addIndent() {
        ss << tabs[indent];
    }

    void addTypeName(Type type) {
        if (type == types::Void)          { ss << "void"; }
        else if (type == types::Number)   { ss << "double"; }
        else if (type == types::Bool)     { ss << "bool"; }
        else if (type == types::String)   { ss << "string"; }
        else if (type == types::Dynamic)  { ss << "dynamic"; }
        else if (type == types::Unknown)  { ss << "UNKNOWN"; }
        else if (type == types::Function) { ss << "FUNCTION"; }
        else {
            if (type->isPrimitive()) {
                ss << type->primitiveTypeData()->name;
            } else if (type->isStruct()) {
                ss << type->structTypeData()->name;
            } else if (type->isArray()) {
                ss << "array_base";
            } else {
                assert(0 && "Can't writer typename.");
            }
        }
    }

    void addFunctions(std::vector<FunInstantiation> &insts) {
        for (auto instAst : insts) {
            auto functionType = instAst.inst.type->functionTypeData();
            Type returnType = functionType->returnType;

            addTypeName(returnType);
            ss << " " << instAst.inst.declaration->name << "(";
            size_t i = 0;
            for (auto param : instAst.inst.declaration->parameters) {
                addTypeName(functionType->parameterTypes[i]);
                ss << " " << param.name;
                if (i < instAst.inst.declaration->parameters.size() - 1) {
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
                ss << " = ";
                addExpr(ex.value, false);
            },
            [&](VariableLiteral &ex) {
                ss << ex.name.text;
            },
            [&](TypeLiteral &ty) {
                addTypeName(ty.type);
            },
            [&](Unit &u) {},
            [&](FunctionCall &call) {
                ss << call.functionDeclaration->name;
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
                else {
                    assert(0 && "Printing not implemented for this type");
                }
                ss << "\\n\", ";
                if (type == types::Bool) { 
                    ss << "("; addExpr(print.argument, false); ss << " ? \"true\" : \"false\")";
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
                ss << var.name.text;
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

void generateCodeC(AstGen *astGen, std::ostringstream &stream) {
    CodeGen codeGen(stream);
    codeGen.parser = astGen->parser;
    
    codeGen.addTypedefs();
    codeGen.addStructDeclarations(astGen->structDeclarations);
    codeGen.addFunctions(astGen->functionInstantiations);
    codeGen.addMain(astGen->initialDeclaration);

}