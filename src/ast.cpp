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
    int type;
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
    Token name;
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
};
struct ReturnStatement {
    Statement stmt;
    Expr *expr = nullptr;
};
struct WhileStatement {
    Statement stmt;
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

    void addTypeName(int type) {
        switch (type) {
            case TypeId::Void:      ss << "void"; break;
            case TypeId::Number:    ss << "double"; break;
            case TypeId::Bool:      ss << "bool"; break;
            case TypeId::String:    ss << "string"; break;
            case TypeId::Dynamic:   ss << "dynamic"; break;
            case TypeId::Unknown:   ss << "UNKNOWN"; break;
            case TypeId::Function:  ss << "FUNCTION"; break;
        }
    }

    void addFunctions(std::vector<FunInstantiation> &insts) {
        for (auto instAst : insts) {
            auto functionType = mpark::get<FunctionTypeObj>(parser->types[instAst.inst.type]);
            int returnType = functionType.returnType;

            addTypeName(returnType);
            ss << " " << instAst.inst.declaration->name << "(";
            size_t i = 0;
            for (auto param : instAst.inst.declaration->parameters) {
                addTypeName(functionType.parameterTypes[i]);
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
                ss << ((BooleanLiteral*)expr)->name.text;
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
                    addExpr(arg);
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
                int type = ((PrintStatement*)stmt)->argument->type;
                if (type == TypeId::Number) ss << "%f";
                else if (type == TypeId::String) ss << "%s";
                else if (type == TypeId::Bool) ss << "%i";
                ss << "\\n\", ";
                addExpr(((PrintStatement*)stmt)->argument, false);
                ss << ")";
                break;
            }
            case StmtKind::If: {
                break;
            }
            case StmtKind::Return: {
                ss << "return ";
                addExpr(((ReturnStatement*)stmt)->expr, false);
                break;
            }
            case StmtKind::While: {
                break;
            }
            case StmtKind::Block: {
                ss << "{" << endl;
                indent ++;
                addDecls(((Block*)stmt)->firstDeclaration);
                indent --;
                addIndent();
                ss << "}";
                break;
            }
            case StmtKind::Expr: {
                addExpr(((ExprStatement*)stmt)->expr);
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
                addTypeName(var->value->type);
                ss << " ";
                ss << var->name.text << " = ";
                addExpr(var->value, false);
                ss << ";" << endl;
                break;
            }
            case DeclKind::Stmt: {
                addIndent();
                addStmt((Statement*)decl);
                ss << ";" << endl;
                break;
            }
        }
    }

    void addTypedefs() {
        ss << "#include <stdio.h>" << endl;
        ss << "typedef char* string;" << endl;
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
    
    void varDeclaration(Token name) {
        auto varDecl = new VarDeclaration;
        varDecl->decl.kind = DeclKind::Var;
        varDecl->name = name;
        varDecl->value = popExpression();
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
};

void generateCodeC(AstGen *astGen) {
    CodeGen codeGen;
    codeGen.parser = astGen->parser;
    
    codeGen.addTypedefs();
    codeGen.addFunctions(astGen->functionInstantiations);
    codeGen.addMain(astGen->initialDeclaration);

    std::cout << codeGen.ss.str();
}