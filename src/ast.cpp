#include <sstream>

using std::endl;

enum class ExprKind {
    FunctionCall, Infix, String, Number, Boolean, Assignment, Var
};
enum class DeclKind {
    Fun, Var, Stmt
};
enum class StmtKind {
    Print, If, While, Return, Block, Expr
};

struct Expr {
    ExprKind kind;
    Type type;
};

struct FunctionCall {
    Expr expr;
    FunctionDeclaration *functionDeclaration;
    int argCount;
    std::vector<Expr*> arguments;
};

struct InfixExpr {
    Expr expr;
    Token operatorToken;
    Expr *left = nullptr;
    Expr *right = nullptr;
};

struct StringLiteral {
    Expr expr;
    Token name;
};
struct NumberLiteral {
    Expr expr;
    Token name;
};
struct BooleanLiteral {
    Expr expr;
    bool value;
};

struct AssignmentExpr {
    Expr expr;
    Token name;
    Expr *value = nullptr;
};
struct VariableLiteral {
    Expr expr;
    Token name;
};


struct Declaration {
    DeclKind kind;
    Declaration *nextSibling = nullptr;
};
struct FunDeclaration {
    Declaration decl;
    // FunctionDeclaration *functionDeclaration = nullptr;
    Declaration *firstDeclaration = nullptr;
};
struct FunInstantiation {
    FunDeclaration *decl;
    FunctionInstantiation inst;
};
struct VarDeclaration {
    Declaration decl;
    Token name;
    Type type;
    Expr *value = nullptr;
};
struct Statement {
    Declaration decl;
    StmtKind kind;
};


struct PrintStatement {
    Statement stmt;
    Expr *argument = nullptr;
};
struct IfStatement {
    Statement stmt;
    Expr *expr = nullptr;
    Declaration *firstDeclaration = nullptr;
    Declaration *firstElseDeclaration = nullptr;
};
struct ReturnStatement {
    Statement stmt;
    Expr *expr = nullptr;
};
struct WhileStatement {
    Statement stmt;
    Expr *expr = nullptr;
    Declaration *firstDeclaration = nullptr;
};
struct Block {
    Statement stmt;
    Declaration *firstDeclaration = nullptr;
};
struct ExprStatement {
    Statement stmt;
    Expr *expr = nullptr;
};


const char *tabs[] = {
    "", "  ", "    ", "      ", "        ", "          "
};

struct CodeGen {
    Parser *parser = nullptr;
    std::stringstream ss;
    int indent = 0;

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
            if (type->isStruct()) {
                ss << type->structTypeData()->name;
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
        switch (expr->kind) {
            case ExprKind::String: {
                ss << ((StringLiteral*)expr)->name.text;
                break;
            }
            
            case ExprKind::Number: {
                ss << ((NumberLiteral*)expr)->name.text;
                break;
            }
            
            case ExprKind::Boolean: {
                ss << (((BooleanLiteral*)expr)->value ? "true" : "false");
                break;
            }
            
            case ExprKind::Assignment: {
                ss << ((AssignmentExpr*)expr)->name.text;
                ss << " = ";
                addExpr(((AssignmentExpr*)expr)->value, false);
                break;
            }
            
            case ExprKind::Var: {
                ss << ((VariableLiteral*)expr)->name.text;
                break;
            }
            case ExprKind::Infix: {
                auto infix = (InfixExpr*)expr;
                if (parens) ss << "(";
                addExpr(infix->left);
                ss << " " << infix->operatorToken.text << " ";
                addExpr(infix->right);
                if (parens) ss << ")";
                break;
            }
            case ExprKind::FunctionCall: {
                auto call = (FunctionCall*)expr;
                ss << call->functionDeclaration->name;
                ss << "(";
                size_t i = 0; 
                for (Expr *arg : call->arguments) {
                    addExpr(arg, false);
                    if (i < call->arguments.size() - 1) {
                        ss << ", ";
                    }
                    i++;
                }
                ss << ")";
            }
        }
    }

    void addStmt(Statement *stmt) {
        switch (stmt->kind) {
            case StmtKind::Print: {
                ss << "printf(\"";
                auto print = (PrintStatement*)stmt;
                Type type = print->argument->type;
                if (type == types::Number) ss << "%f";
                else if (type == types::String) ss << "%s";
                else if (type == types::Bool) ss << "%s";
                ss << "\\n\", ";
                if (type == types::Bool) { 
                    ss << "("; addExpr(print->argument, false); ss << " ? \"true\" : \"false\")";
                } else { addExpr(print->argument, false); }
                ss << ")";
                ss << ";" << endl;
                break;
            }
            case StmtKind::If: {
                auto ifStmt = (IfStatement*)stmt;
                ss << "if (";
                addExpr(ifStmt->expr, false);
                ss << ") {" << endl;
                indent ++; addDecls(ifStmt->firstDeclaration); indent --;
                if (ifStmt->firstElseDeclaration) {
                    addIndent();
                    ss << "} else {" << endl;
                    indent ++; addDecls(ifStmt->firstElseDeclaration); indent --;
                }
                addIndent();
                ss << "}" << endl;

                break;
            }
            case StmtKind::Return: {
                auto ret = (ReturnStatement*)stmt;
                ss << "return ";
                if (ret->expr) addExpr(ret->expr, false);
                ss << ";" << endl;
                break;
            }
            case StmtKind::While: {
                auto whileStmt = (WhileStatement*)stmt;
                ss << "while (";
                addExpr(whileStmt->expr, false);
                ss << ") {" << endl;
                indent ++; addDecls(whileStmt->firstDeclaration); indent --;
                addIndent();
                ss << "}" << endl;
                break;
            }
            case StmtKind::Block: {
                ss << "{" << endl;
                indent ++;
                addDecls(((Block*)stmt)->firstDeclaration);
                indent --;
                addIndent();
                ss << "}" << endl;
                break;
            }
            case StmtKind::Expr: {
                addExpr(((ExprStatement*)stmt)->expr);
                ss << ";" << endl;
                break;
            }
        }
    }

    void addDecl(Declaration *decl) {
        switch (decl->kind) {
            case DeclKind::Fun: {
                break;
            }
            case DeclKind::Var: {
                addIndent();
                auto var = (VarDeclaration*)decl;
                addTypeName(var->type);
                ss << " ";
                ss << var->name.text;
                if (var->value) {
                    ss << " = ";
                    addExpr(var->value, false);
                }
                ss << ";" << endl;
                break;
            }
            case DeclKind::Stmt: {
                addIndent();
                addStmt((Statement*)decl);
                break;
            }
        }
    }

    void addTypedefs() {
        ss << "#include <time.h>" << endl;
        ss << "#include <stdio.h>" << endl;
        ss << "#define true 1" << endl;
        ss << "#define false 0" << endl;
        ss << "typedef int bool;" << endl;
        ss << "typedef char* string;" << endl;
        ss << "double clock_seconds() { return (double)clock() / CLOCKS_PER_SEC; }" << endl;
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

struct AstGen {
    std::vector<Expr*> expressionStack;
    std::vector<Declaration*> declarationStack;
    std::vector<Declaration**> nextDeclarationStack;

    std::vector<Type> structDeclarations;

    std::vector<FunInstantiation> functionInstantiations;

    Parser *parser = nullptr;
    Declaration *initialDeclaration = nullptr;

    AstGen() {
        nextDeclarationStack.push_back(&initialDeclaration);
    }


    Expr *popExpression() {
        Expr *expr = expressionStack.back();
        expressionStack.pop_back();
        return expr;
    }


    void string(Token name) {
        auto lit = new StringLiteral;
        lit->expr.type = parser->compiler->expressionTypeStack.back();
        lit->expr.kind = ExprKind::String;
        lit->name = name;
        expressionStack.push_back(&lit->expr);
    }
    void number(Token name) {
        auto lit = new NumberLiteral;
        lit->expr.type = parser->compiler->expressionTypeStack.back();
        lit->expr.kind = ExprKind::Number;
        lit->name = name;
        expressionStack.push_back(&lit->expr);
    }
    void variable(Token name) {
        auto var = new VariableLiteral;
        var->expr.type = parser->compiler->expressionTypeStack.back();
        var->expr.kind = ExprKind::Var;
        var->name = name;
        expressionStack.push_back(&var->expr);
    }
    void booleanLiteral(bool value) {
        auto b = new BooleanLiteral;
        b->expr.type = parser->compiler->expressionTypeStack.back();
        b->expr.kind = ExprKind::Boolean;
        b->value = value;
        expressionStack.push_back(&b->expr);
    }

    void assignment(Token name) {
        auto assgn = new AssignmentExpr;
        assgn->expr.type = parser->compiler->expressionTypeStack.back();
        assgn->expr.kind = ExprKind::Assignment;
        assgn->name = name;
        assgn->value = popExpression();
        expressionStack.push_back(&assgn->expr);
    }

    void infix(Token operatorToken) {
        Expr *right = popExpression();
        Expr *left = popExpression();
        auto infix = new InfixExpr;
        infix->expr.type = parser->compiler->expressionTypeStack.back();
        infix->expr.kind = ExprKind::Infix;
        infix->operatorToken = operatorToken;
        infix->left = left;
        infix->right = right;
        expressionStack.push_back(&infix->expr);
    }

    void functionCall(FunctionInstantiation inst, int argCount) {
        std::vector<Expr*> args;
        for (int i = 0; i < argCount; i++) {
            args.insert(args.begin(), 1, popExpression());
        }
        auto call = new FunctionCall();
        call->expr.kind = ExprKind::FunctionCall;
        call->expr.type = parser->compiler->expressionTypeStack.back();
        call->argCount = argCount;
        call->arguments = args;
        call->functionDeclaration = inst.declaration;
        expressionStack.push_back(&call->expr);
    }

    // Declarations

    void exprStatement() {
        auto exprStmt = new ExprStatement;
        exprStmt->stmt.decl.kind = DeclKind::Stmt;
        exprStmt->stmt.kind = StmtKind::Expr;
        exprStmt->expr = popExpression();

        *nextDeclarationStack.back() = &exprStmt->stmt.decl;
        nextDeclarationStack.back() = &exprStmt->stmt.decl.nextSibling;
    }

    void returnStatement(bool isNil) {
        auto exprStmt = new ReturnStatement;
        exprStmt->stmt.decl.kind = DeclKind::Stmt;
        exprStmt->stmt.kind = StmtKind::Return;
        exprStmt->expr = isNil ? nullptr : popExpression();

        *nextDeclarationStack.back() = &exprStmt->stmt.decl;
        nextDeclarationStack.back() = &exprStmt->stmt.decl.nextSibling;
    }

    IfStatement *beginIfStatementBlock() {
        auto ifStmt = new IfStatement;
        ifStmt->stmt.decl.kind = DeclKind::Stmt;
        ifStmt->stmt.kind = StmtKind::If;
        ifStmt->expr = popExpression();

        *nextDeclarationStack.back() = &ifStmt->stmt.decl;
        nextDeclarationStack.back() = &ifStmt->stmt.decl.nextSibling;

        nextDeclarationStack.push_back(&ifStmt->firstDeclaration);
        return ifStmt;
    }
    void beginElseBlock(IfStatement *ifStmt) {
        nextDeclarationStack.push_back(&ifStmt->firstElseDeclaration);
    }

    void beginWhileStatementBlock() {
        auto whileStmt = new WhileStatement;
        whileStmt->stmt.decl.kind = DeclKind::Stmt;
        whileStmt->stmt.kind = StmtKind::While;
        whileStmt->expr = popExpression();

        *nextDeclarationStack.back() = &whileStmt->stmt.decl;
        nextDeclarationStack.back() = &whileStmt->stmt.decl.nextSibling;
        nextDeclarationStack.push_back(&whileStmt->firstDeclaration);
    }
    
    void varDeclaration(Token name, Type type) {
        auto varDecl = new VarDeclaration;
        varDecl->decl.kind = DeclKind::Var;
        varDecl->name = name;
        varDecl->value = popExpression();
        varDecl->type = type;
        declarationStack.push_back(&varDecl->decl);

        *nextDeclarationStack.back() = &varDecl->decl;
        nextDeclarationStack.back() = &varDecl->decl.nextSibling;
    }

    void print() {
        auto print = new PrintStatement;
        print->stmt.decl.kind = DeclKind::Stmt;
        print->stmt.kind = StmtKind::Print;
        print->argument = popExpression();
        declarationStack.push_back(&print->stmt.decl);

        *nextDeclarationStack.back() = &print->stmt.decl;
        nextDeclarationStack.back() = &print->stmt.decl.nextSibling;
    }

    void beginBlock() {
        auto block = new Block;
        block->stmt.decl.kind = DeclKind::Stmt;
        block->stmt.kind = StmtKind::Block;
        declarationStack.push_back(&block->stmt.decl);
        *nextDeclarationStack.back() = &block->stmt.decl;
        nextDeclarationStack.back() = &block->stmt.decl.nextSibling;
        nextDeclarationStack.push_back(&block->firstDeclaration);
    }
    void endBlock() {
        nextDeclarationStack.pop_back();
    }

    void beginFunctionDeclaration(FunctionInstantiation inst) {
        auto func = new FunDeclaration;
        functionInstantiations.push_back({func, inst});
        func->decl.kind = DeclKind::Fun;
        declarationStack.push_back(&func->decl);
        *nextDeclarationStack.back() = &func->decl;
        nextDeclarationStack.back() = &func->decl.nextSibling;
        nextDeclarationStack.push_back(&func->firstDeclaration);
    }
    void endFunctionDeclaration() {
        nextDeclarationStack.pop_back();
    }

    void structDeclaration(Type type) {
        structDeclarations.push_back(type);
    }
};

std::string generateCodeC(AstGen *astGen) {
    CodeGen codeGen;
    codeGen.parser = astGen->parser;
    
    codeGen.addTypedefs();
    codeGen.addStructDeclarations(astGen->structDeclarations);
    codeGen.addFunctions(astGen->functionInstantiations);
    codeGen.addMain(astGen->initialDeclaration);

    return codeGen.ss.str();
}