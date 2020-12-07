


FunctionInstantiation *Parser::getInstantiationByStackArguments(FunctionDeclaration *functionDeclaration, int argCount) {
    // slow as hell
    for (size_t j = 0; j < functionDeclaration->overloads.size(); j++) {
        auto inst = &functionDeclaration->overloads[j];
        auto functionTypeObj = inst->type->functionTypeData();

        bool isMatched = true;
        for (size_t i = 0; i < functionDeclaration->parameters.size(); i++) {
            size_t end = compiler->expressionTypeStack.size();
            Type argumentType = compiler->expressionTypeStack[end - argCount + i];
            if (!typecheckIsAssignable(this, functionTypeObj->parameterTypes[i], argumentType)) {
                isMatched = false; break;
            }
        }
        if (isMatched) return inst;
    }
    return nullptr;
}


FunctionInstantiation *Parser::createInstantiation(FunctionDeclaration *functionDeclaration) {

    // TODO: Garbage collection
    auto newFunction = new ObjFunction();
    Compiler *compiler = new Compiler(newFunction, CompilerType::Function, functionDeclaration->enclosingCompiler);
    auto functionType = typecheckFunctionDeclaration(this, newFunction);

    // TODO: Garbage collection
    newFunction->name = new ObjString(std::string(functionDeclaration->name));
    newFunction->arity = functionDeclaration->parameters.size();
    newFunction->functionDeclaration = functionDeclaration;

    compiler->compiled = true; // Mark this so we don't recurse

    FunctionInstantiation functionInst = {
        functionType, newFunction, compiler, functionDeclaration
    };
    functionDeclaration->overloads.push_back(functionInst);

    return &functionDeclaration->overloads.back();
}

void Parser::compileFunctionInstantiation(FunctionInstantiation &functionInst) {
    
    Compiler *initialCompiler = this->compiler;
    Scanner *initialScanner = this->scanner;
    FunctionDeclaration *functionDeclaration = functionInst.declaration;
    ObjFunction* newFunction = &functionInst.function.asFunction();
    Type functionType = functionInst.type;
    auto functionTypeObj = functionType->functionTypeData();
    Compiler *compiler = functionInst.compiler;

    if (functionDeclaration->isExtern) {
        typecheckFunctionDeclarationReturn(this, newFunction, functionType, functionDeclaration->returnType);
    } else {
        
        beginScope();
        ast->beginFunctionDeclaration(functionInst);
        
        this->compiler = compiler;

        // I don't think we want to push the current function/top level script on the stack anymore
        // compiler->locals.push_back(Local("", 0, 0)); // Function object
        // compiler->nextStackSlot += 2;

        newFunction->argSlots = 0;
        for (size_t i = 0; i < functionDeclaration->parameters.size(); i++) {
            compiler->declareVariable(this, functionDeclaration->parameters[i].name);
            compiler->markInitialized();

            Local &local = compiler->locals.back();
            local.type = functionTypeObj->parameterTypes[i];
            
            // TODO: bytecode only
            local.stackOffset = compiler->nextStackSlot;
            int slotSize = slotSizeOfType(local.type);
            compiler->nextStackSlot += slotSize;
            newFunction->argSlots += slotSize;
        }

        typecheckFunctionDeclarationReturn(this, newFunction, functionType, functionDeclaration->returnType);
    
        
        
        Scanner tempScanner { initialScanner->source };
        scanner = &tempScanner;

        scanner->current = functionDeclaration->blockStart;
        scanner->start = functionDeclaration->blockStart;
        scanner->line = functionDeclaration->blockLine;
        scanner->parens = 0;
        advance();
        
        block();
        ast->endFunctionDeclaration();
        endCompiler();

        // Reset back
        this->scanner = initialScanner;
        this->compiler = initialCompiler;
    }

}


void Parser::funDeclaration() {
    consume(TokenType::Identifier, "Expect function name.");
    auto name = previous().text;
    auto function = new ObjFunction();
    Compiler *enclosingCompiler = compiler;
    Compiler *functionCompiler = new Compiler(function, CompilerType::Function, enclosingCompiler);
    uint16_t constant = makeConstant(function);
    // int functionType = typecheckFunctionDeclaration(this, function);
    // initCompiler(functionCompiler);
    // beginScope();

    std::vector<FunctionParameter> parameters;
    bool polymorphic = false;

    consume(TokenType::LeftParen, "Expect '(' after function name.");
    if (!check(TokenType::RightParen)) {
        do {
            function->arity++;
            if (function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }

            consume(TokenType::Identifier, "Expect parameter name.");
            auto parameterName = previous().text;

            Type argumentType = types::Unknown;
            if (match(TokenType::Colon)) {
                consume(TokenType::Identifier, "Expect type name after ':'.");
                argumentType = typeByName(this, previous().text);
            }
            // typecheckParameter(this, function, functionType, argumentType);

            parameters.push_back({ parameterName, argumentType });
            if (argumentType == types::Unknown) {
                polymorphic = true;
            }
        } while (match(TokenType::Comma));
    }
    consume(TokenType::RightParen, "Expect ')' after after parameters.");

    Type returnType = types::Void;
    if (match(TokenType::Colon)) {
        consume(TokenType::Identifier, "Expect type name after ':'.");
        returnType = typeByName(this, previous().text);
    }
    // typecheckFunctionDeclarationReturn(this, function, functionType, returnType);


    auto decl = new FunctionDeclaration; // @leak
    decl->name = name;
    decl->parameters = parameters;
    decl->returnType = returnType;
    decl->polymorphic = polymorphic;
    decl->enclosingCompiler = enclosingCompiler;
    decl->isExtern = false;
    decl->constant = constant;
    decl->blockStart = scanner->start;
    decl->blockLine = scanner->line;
    enclosingCompiler->functionDeclarations.push_back(decl);

    if (match(TokenType::At)) {
        consume(TokenType::Identifier, "Expect name after '@'.");
        if (previous().text == "extern") {
            decl->isExtern = true;
        } else if (previous().text == "inline") {
            decl->isInline = true;
        } else {
            error("Unrecognised at-name");
            return;
        }
    }
    
    if (decl->isExtern) return;

    consume(TokenType::LeftBrace, "Expect '{' before function body.");

    // Must be AFTER the left brace
    decl->blockStart = scanner->start;
    decl->blockLine = scanner->line;

    int braces = 0;
    while ((braces > 0 || !check(TokenType::RightBrace)) && !check(TokenType::EOF_)) {
        if (match(TokenType::LeftBrace)) braces ++;
        else if (match(TokenType::RightBrace)) braces --;
        else advance();
    }
    consume(TokenType::RightBrace, "Expect '}' after block");

    // compiler = compiler->enclosing;
}


void Parser::lambda(ExpressionState es) {
    if (!isBytecode) {
        error("Not supported yet");
        return;
    }

    auto function = new ObjFunction(); // @leak
    function->name = new ObjString("lambda"); // @leak

    Compiler *enclosingCompiler = compiler;
    Compiler *functionCompiler = new Compiler(function, CompilerType::Function, enclosingCompiler);
    uint16_t constant = makeConstant(function);

    std::vector<FunctionParameter> parameters;

    auto decl = new FunctionDeclaration; // @leak
    decl->name = std::string_view(function->name->text);
    decl->parameters = parameters;
    decl->returnType = types::Void;
    decl->polymorphic = true;
    decl->enclosingCompiler = enclosingCompiler;
    decl->isExtern = false;
    decl->constant = constant;

    function->functionDeclaration = decl;

    consume(TokenType::LeftBrace, "Expect '{' after 'block'.");

    // Must be AFTER the left brace
    decl->blockStart = scanner->start;
    decl->blockLine = scanner->line;

    int braces = 0;
    while ((braces > 0 || !check(TokenType::RightBrace)) && !check(TokenType::EOF_)) {
        if (match(TokenType::LeftBrace)) braces ++;
        else if (match(TokenType::RightBrace)) braces --;
        else advance();
    }
    consume(TokenType::RightBrace, "Expect '}' after block");

    vmWriter->emitConstant(constant);
    // typecheckFunctionDeclaration(this, function /* not used */);
    // typecheckFunctionDeclarationReturn(this, function, )
    typecheckLambda(this);

    // expression();
    // consume(TokenType::LeftBrace, "Expect property name after '.'.");
    // Token property = previous();
    // // uint8_t name = identifierConstant(previous().text);

    // typecheckPropertyAccess(this, property.text);
    // ast->property(property);
  
    // if (es.canAssign && match(TokenType::Equal)) {
    //     expression();
    //     typecheckAssignExpression(this);
    //     ast->assignment();
    //     // emitBytes(OP_SET_PROPERTY, name); // TODO:
    // } else {
    //     // emitBytes(OP_GET_PROPERTY, name);
    // }
}


void Parser::callFunction(FunctionDeclaration *functionDeclaration) {
    
    
    typecheckBeginFunctionCall(this, nullptr);

    uint8_t argCount = argumentList(functionDeclaration);

    auto inst = getInstantiationByStackArguments(functionDeclaration, argCount);
    if (!inst) {
        inst = createInstantiation(functionDeclaration);
        typecheckInstantiationAgainstStack(this, inst, argCount);
        compileFunctionInstantiation(*inst);
    }
    
    typecheckEndFunctionCall(this, inst->function, argCount);
    
    if (isBytecode) vmWriter->functionCall(*inst, argCount);
    else ast->functionCall(*inst, argCount);
}

void Parser::callLambda(ExpressionState es) {
    if (isBytecode) {
        error("Not supported yet.");
        return;
    }

    // No arguments for now
    consume(TokenType::RightParen, "Expect ')' after argument list.");

    // To call this lambda we need to retreive it from the VM
    // and compile it.

    typecheckCallLambda(this);

    vmWriter->vm.run();
    ObjFunction &function = vmWriter->vm.peek<Value>().asFunction();
    FunctionDeclaration *funDecl = function.functionDeclaration;

    Compiler *initialCompiler = this->compiler;
    Scanner *initialScanner = this->scanner;

    Compiler *compiler = new Compiler(&function, CompilerType::Function, funDecl->enclosingCompiler);

    this->compiler = compiler;

    Scanner tempScanner { initialScanner->source };
    scanner = &tempScanner;

    scanner->current = funDecl->blockStart;
    scanner->start = funDecl->blockStart;
    scanner->line = funDecl->blockLine;
    scanner->parens = 0;
    advance();
    
    block();

    // Reset back
    this->scanner = initialScanner;
    this->compiler = initialCompiler;


    ast->makeLambda();

}