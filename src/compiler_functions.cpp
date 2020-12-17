


FunctionInstantiation *Parser::getInstantiationByStackArguments(FunctionDeclaration *functionDeclaration, int argCount) {
    // @speed slow as hell
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
    Compiler *compiler = new Compiler(newFunction, CompilerType::Function, functionDeclaration->enclosingCompiler); // @leak
    auto functionType = typecheckFunctionDeclaration(this, newFunction);

    // TODO: Garbage collection
    newFunction->name = new ObjString(std::string(functionDeclaration->name));
    newFunction->arity = functionDeclaration->parameters.size();
    newFunction->functionDeclaration = functionDeclaration;

    compiler->compiled = true; // Mark this so we don't recurse

    int size = functionDeclaration->overloads.size() + 1;
    std::string renamedTo = std::string(functionDeclaration->name);
    if (size > 1) {
        renamedTo += "__" + std::to_string(size);
    }

    FunctionInstantiation functionInst = {
        functionType, newFunction, compiler, functionDeclaration, renamedTo
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

    // We need to update the nextStackOffset of the new compiler
    // to be the same as the one that we were _called from_ (not
    // necessarily) the enclosing compiler. This function was
    // not called from bytecode, it is a runtime function, so the
    // VM shares the stack of the calling function.
    // It feels a bit weird to mutate the compiler instance like
    // this but we want to ensure that we pick up the correct
    // initialCompiler.
    compiler->nextStackOffset = initialCompiler->nextStackOffset;
        
    if (isBytecode) vmWriter->beginFunctionDeclaration(functionInst);
    else ast->beginFunctionDeclaration(functionInst);
    
    this->compiler = compiler;
    beginScope();

    newFunction->argSlots = 0;
    for (size_t i = 0; i < functionDeclaration->parameters.size(); i++) {
        Local &local = compiler->declareVariable(this, functionDeclaration->parameters[i].name);
        local.type = functionTypeObj->parameterTypes[i];
        local.isStatic = isBytecode || functionDeclaration->parameters[i].isStatic;
        compiler->markInitialized();

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
    endCompiler();
    if (isBytecode) vmWriter->endFunctionDeclaration(initialCompiler);
    else ast->endFunctionDeclaration();

    // Reset back
    this->scanner = initialScanner;
    this->compiler = initialCompiler;

}

void Parser::functionParameters(FunctionDeclaration *decl) {

    do {
        if (decl->parameters.size() >= 255) {
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

        decl->parameters.push_back({ parameterName, argumentType });
        if (argumentType == types::Lambda) {
            decl->parameters.back().isStatic = true;
        }
        if (argumentType == types::Unknown) {
            decl->polymorphic = true;
        }
    } while (match(TokenType::Comma));
}


void Parser::funDeclaration() {
    consume(TokenType::Identifier, "Expect function name.");
    auto name = previous().text;
    auto function = new ObjFunction();
    Compiler *enclosingCompiler = compiler;
    // Compiler *functionCompiler = new Compiler(function, CompilerType::Function, enclosingCompiler);
    uint16_t constant = makeConstant(function);
    // int functionType = typecheckFunctionDeclaration(this, function);
    // initCompiler(functionCompiler);
    // beginScope();

    auto decl = new FunctionDeclaration; // @leak
    decl->name = name;
    decl->parameters = {};
    decl->polymorphic = false;
    decl->enclosingCompiler = enclosingCompiler;
    decl->isExtern = false;
    decl->isStatic = isBytecode;
    decl->constant = constant;
    decl->blockStart = scanner->start;
    decl->blockLine = scanner->line;
    enclosingCompiler->functionDeclarations.push_back(decl);

    consume(TokenType::LeftParen, "Expect '(' after function name.");
    if (!check(TokenType::RightParen)) {
        functionParameters(decl);
    }
    consume(TokenType::RightParen, "Expect ')' after after parameters.");
    function->arity = decl->parameters.size();

    decl->returnType = types::Void;
    if (match(TokenType::Colon)) {
        consume(TokenType::Identifier, "Expect type name after ':'.");
        decl->returnType = typeByName(this, previous().text);
    }



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
    // This must be a static variable
    // if (!isBytecode) {
    //     error("Not supported yet");
    //     return;
    // }

    consume(TokenType::LeftBrace, "Expect '{' after 'block'.");
    lambdaContents();
}

void Parser::lambdaContents() {

    auto function = new ObjFunction(); // @leak
    function->name = new ObjString("lambda line " + std::to_string(scanner->line)); // @leak

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

    

    if (match(TokenType::Pipe)) {
        functionParameters(decl);
        consume(TokenType::Pipe, "Expected '|' after parameter list.");
    }
    function->arity = decl->parameters.size();

    // Must be AFTER the left brace / parameter list
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
    newCompiler.inlinedFromTop = this->compiler->inlinedFromTop;
    newCompiler.nextStackOffset = this->compiler->nextStackOffset;


    // Handle function parameters before we switch to the
    // new compiler. The compiler methods should be written
    // in a way to not make an assumption on which compiler
    // is currently active.
    // The reason for fetching the arguments in an iterator
    // style instead of fetching them all at once is because
    // its easier to deal with things on the top of stack
    // one-by-one
    function->argSlots = 0;
    size_t argCount = 0;
    while (argumentListNext(funDecl, &argCount)) {
        auto param = funDecl->parameters[argCount - 1];
        newCompiler.declareVariable(this, param.name);
        Local &local = newCompiler.locals.back();
        local.isStatic = param.isStatic;

        local.type = param.type;
        typecheckVarDeclarationInitializer(this, local);
        // markInitialized after typecheck incase isStatic
        // has changed
        newCompiler.markInitialized();
                                       
        
        if (!local.isStatic) {
            ast->varDeclaration(local, /* initializer */ true);
        } else {
            // Nothing happens in the VM - it is just left on
            // the stack but we should record the total slot size
            int slotSize = slotSizeOfType(newCompiler.locals.back().type);
            function->argSlots += slotSize;
        }
    }

    if (argCount != funDecl->parameters.size()) {
        error("Not enough arguments for this function.");
        return types::Void;
    }

    // Run the VM. We don't _have_ to do this here but it makes debugging
    // easier if we know what the stack is at this point. Also I suppose
    // we wan't to run any sideeffects that function arguments might have.
    vmWriter->vm.run();

    Scanner tempScanner { initialScanner->source };
    tempScanner.current = funDecl->blockStart;
    tempScanner.start = funDecl->blockStart;
    tempScanner.line = funDecl->blockLine;
    this->compiler = &newCompiler;
    this->scanner = &tempScanner;
    advance();

    block();

    // We have to pop the bytecode variables that have been introduced
    // during this inline because they are on a shared stack. This will
    // include parameters - is that okay?
    endScope();

    // Reset back
    this->scanner = initialScanner;
    this->compiler = initialCompiler;


    return newCompiler.implicitReturnType;
}

bool Parser::argumentListNext(FunctionDeclaration *functionDeclaration, size_t *argCount) {
    // Returns true if we found an argument - must continue to loop.

    if (!match(TokenType::RightParen)) {
        // This is a bit hacky, I want a better state machine
        if (!check(TokenType::Comma) && previous().type == TokenType::RightBrace) {
            return false;
        }

        if (*argCount > 0) {
            consume(TokenType::Comma, "Expect ',' or ')' after argument");
        }

        expression();
        if (*argCount == 255) {
            error("Can't have more than 255 arguments.");
            return false;
        }
        typecheckFunctionArgument(this, functionDeclaration, *argCount);
        (*argCount) ++;
        return true;
    }

    // Matched right paren at this point

    if (match(TokenType::LeftBrace)) {
        lambdaContents();
        typecheckFunctionArgument(this, functionDeclaration, *argCount);
        (*argCount) ++;
        return true;
    }
    return false;
}

void Parser::callFunction(FunctionDeclaration *functionDeclaration) {
    
    if (isBytecode && (!functionDeclaration->isStatic && !functionDeclaration->isExtern)) {
        error("Cannot call a static function from runtime.");
        return;
    }

    if (functionDeclaration->isInline && !functionDeclaration->isStatic) {
        // TODO: Why is function needed here?
        ObjFunction *function = new ObjFunction();
        // TODO: We need to do something about returns,
        // semantically they should return from this function,
        // but they currently return from the outer function.
        // We should add a goto.
        inlineFunction(function, functionDeclaration);
        compiler->expressionTypeStack.push_back(types::Void);
        ast->unit();
        return;
    }

    typecheckBeginFunctionCall(this, nullptr);

    // Calls to static functions must have static arguments
    bool wasBytecode = isBytecode;
    if (functionDeclaration->isStatic) {
        isBytecode = true;
    }

    size_t argCount = 0;
    while (argumentListNext(functionDeclaration, &argCount)) {
        continue;
    }

    if (functionDeclaration->isStatic) {
        isBytecode = wasBytecode;
    }

    auto inst = getInstantiationByStackArguments(functionDeclaration, argCount);
    if (!inst) {
        inst = createInstantiation(functionDeclaration);
        typecheckInstantiationAgainstStack(this, inst, argCount);
        compileFunctionInstantiation(*inst);
    }
    
    typecheckEndFunctionCall(this, inst->function, argCount);
    
    if (functionDeclaration->isStatic || isBytecode) { 
        vmWriter->functionCall(*inst, argCount);
    } else ast->functionCall(*inst, argCount);
}


void Parser::callLambda(ExpressionState es) {
    if (isBytecode) {
        // TODO: The function object is already on the stack
        // all we need is a lambda call opcode. and if we have
        // that it would be nice to use that for regular calls
        // as well.
        error("Not supported yet.");
        return;
    }

    typecheckCallLambda(this);

    // To call this lambda we need to run the VM to figure out
    // what the lambda function object is. we retreive it from
    // the VM and compile it.

    vmWriter->vm.run();
    ObjFunction &function = vmWriter->vm.peek<Value>().asFunction();
    FunctionDeclaration *funDecl = function.functionDeclaration;

    // Bit of a hack - the lambda we are calling is on the stack and
    // we need to clean it up, we know for sure we won't use it again
    // because we aren't in bytecode mode, so we can pop it immediately,
    // However the assumption is that the code refers to the variable
    // and immediately calls it, what happens if the variable is never
    // called?
    // BTW we do this before inlining of the function, because otherwise
    // our stack offsets are misaligned.
    vmWriter->exprStatement();

    Type implicitReturnType = inlineFunction(&function, funDecl);

    assert(compiler->expressionTypeStack.size()); // TBH maybe there isn't one?


    compiler->expressionTypeStack.pop_back(); // the lambda itself
    compiler->expressionTypeStack.push_back(implicitReturnType);
    ast->makeImplicitReturn();

}