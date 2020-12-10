


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
    
    FunctionDeclaration *functionDeclaration = functionInst.declaration;
    ObjFunction* newFunction = &functionInst.function.asFunction();
    Type functionType = functionInst.type;

    if (functionDeclaration->isExtern) {
        typecheckFunctionDeclarationReturnValue(this, newFunction, functionType, functionDeclaration->returnType);
        return;
    } 

    Compiler *initialCompiler = this->compiler;
    Scanner *initialScanner = this->scanner;
    auto functionTypeObj = functionType->functionTypeData();
    Compiler *compiler = functionInst.compiler;
        
    beginScope();
    ast->beginFunctionDeclaration(functionInst);
    
    this->compiler = compiler;

    newFunction->argSlots = 0;
    for (size_t i = 0; i < functionDeclaration->parameters.size(); i++) {
        compiler->declareVariable(this, functionDeclaration->parameters[i].name);
        compiler->markInitialized(functionTypeObj->parameterTypes[i]);

        if (isBytecode) {
            int slotSize = slotSizeOfType(compiler->locals.back().type);
            newFunction->argSlots += slotSize;
        }
    }
    

    typecheckFunctionDeclarationReturnValue(this, newFunction, functionType, functionDeclaration->returnType);
    
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

    std::vector<FunctionParameter> parameters;

    auto decl = new FunctionDeclaration; // @leak
    decl->name = std::string_view(function->name->text);
    decl->parameters = parameters;
    decl->returnType = types::Void;
    decl->polymorphic = true;
    decl->enclosingCompiler = this->compiler;
    decl->isExtern = false;
    decl->constant = makeConstant(function);

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

    vmWriter->emitConstant(decl->constant);
    // typecheckFunctionDeclaration(this, function /* not used */);
    // typecheckFunctionDeclarationReturnValue(this, function, )
    typecheckLambda(this);

}


Type Parser::inlineFunction(ObjFunction *function, FunctionDeclaration *funDecl) {

    Compiler *initialCompiler = this->compiler;
    Scanner *initialScanner = this->scanner;

    // TODO: Any sub-compilers from this point will have
    // enclosingCompiler = parser->compiler, which will be
    // the following newCompiler, but the lifetime of this
    // compiler ends at the end of this scope. This compiler
    // is inlined so really the enclosingCompiler should be
    // parser->compiler->inlinedFrom
    // TODO: Why is function needed here? Why is function
    // needed in compiler at all? just for bytecode?
    Compiler newCompiler(function, CompilerType::Function, funDecl->enclosingCompiler);
    newCompiler.inlinedFrom = this->compiler;

    // Handle function parameters before we switch to the
    // new compiler. The compiler methods should be written
    // in a way to not make an assumption on which compiler
    // is currently active.
    function->argSlots = 0;
    for (size_t i = 0; i < funDecl->parameters.size(); i++) {
        if (i > 0) {
            consume(TokenType::Comma, "Expected ',' after expression.");
        }
        
        newCompiler.declareVariable(this, funDecl->parameters[i].name);
        Local &local = newCompiler.locals.back();

        // Maybe not needed because expression happens immediately.
        if (check(TokenType::RightParen)) {
            error("Expected argument, this argument is not optional.");
            break;
        }

        expression();
        typecheckFunctionArgument(this, funDecl, i);
        
        newCompiler.markInitialized(funDecl->parameters[i].type);
        typecheckVarDeclarationInitializer(this, local);
        
        // Nothing happens the VM - it is just left on the stack
        if (!isBytecode) {
            ast->varDeclaration(local, /* initializer */ true);
        }

        if (isBytecode) {
            int slotSize = slotSizeOfType(newCompiler.locals.back().type);
            function->argSlots += slotSize;
        }
    }
    consume(TokenType::RightParen, "Expect ')' after arguments.");

    Scanner tempScanner { initialScanner->source };
    this->compiler = &newCompiler;
    this->scanner = &tempScanner;
    this->scanner->current = funDecl->blockStart;
    this->scanner->start = funDecl->blockStart;
    this->scanner->line = funDecl->blockLine;
    this->scanner->parens = 0;
    advance();

    block();

    // Reset back
    this->scanner = initialScanner;
    this->compiler = initialCompiler;

    return newCompiler.implicitReturnType;
}


uint8_t Parser::argumentList(FunctionDeclaration *functionDeclaration) {
    uint8_t argCount = 0;
    if (!check(TokenType::RightParen)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }
            typecheckFunctionArgument(this, functionDeclaration, argCount);
            argCount ++;
        } while (match(TokenType::Comma));
    }
    consume(TokenType::RightParen, "Expect ')' after arguments.");
    return argCount;
}

void Parser::callFunction(FunctionDeclaration *functionDeclaration) {
    
    if (functionDeclaration->isInline) {
        // TODO: Why is funciton needed here?
        ObjFunction *function = new ObjFunction();
        inlineFunction(function, functionDeclaration);
        compiler->expressionTypeStack.push_back(types::Void);
        ast->unit();
        return;
    }
    
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
    //consume(TokenType::RightParen, "Expect ')' after argument list.");

    // To call this lambda we need to run the VM to figure out
    // what the lambda functon object is. weretreive it from
    // the VM and compile it.

    typecheckCallLambda(this);

    vmWriter->vm.run();
    ObjFunction &function = vmWriter->vm.peek<Value>().asFunction();
    FunctionDeclaration *funDecl = function.functionDeclaration;

    Type implicitReturnType = inlineFunction(&function, funDecl);

    assert(compiler->expressionTypeStack.size()); // TBH maybe there isn't one?
    compiler->expressionTypeStack.pop_back();
    compiler->expressionTypeStack.push_back(implicitReturnType);
    ast->makeImplicitReturn();

}