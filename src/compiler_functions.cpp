
FunctionInstantiation *getInstantiationByTypes(Parser *parser, FunctionDeclaration *functionDeclaration, span<Type> types) {
    // @speed slow as hell
    // We don't cache type parameters and we don't intern strings
    for (size_t j = 0; j < functionDeclaration->overloadIndices.size(); j++) {
        auto inst = parser->functionInstantiations[functionDeclaration->overloadIndices[j]];
        auto functionTypeObj = inst->type->functionTypeData();

        bool isMatched = true;
        size_t i = 0;
        for (auto toMatchType : types) {
            if (!typecheckIsAssignable(parser, functionTypeObj->parameterTypes[i], toMatchType)) {
                isMatched = false;
                break;
            }
            i++;
        }
        if (isMatched) return inst;
    }
    return nullptr;
}


FunctionCall resolveFunctionCallByTypes(Parser *parser, Compiler *compiler, const std::string_view &name, span<Type> types) {
    size_t argCount = types.end_ - types.begin_;
    for (size_t i = 0; i < compiler->functionDeclarations.size(); i++) {
        FunctionDeclaration *functionDeclaration = compiler->functionDeclarations[i];

        if (functionDeclaration->parameters.size() != argCount) {
            continue;
        }
        if (functionDeclaration->name != name) {
            continue;
        }


        auto inst = getInstantiationByTypes(parser, functionDeclaration, types);
        if (inst) {
            return { functionDeclaration, inst, argCount };
        } else {
            // See if the declaration matches, in which case we have no
            // instantiation, but we can make one

            bool isMatched = true;
            size_t i = 0;
            for (auto toMatchType : types) {
                if (functionDeclaration->parameters[i].type != types::Unknown &&
                    !typecheckIsAssignable(parser, functionDeclaration->parameters[i].type, toMatchType)
                ) {
                    isMatched = false;
                    break;
                }
                i++;
            }
            if (isMatched) {
                return { functionDeclaration, nullptr, argCount };
            }
        }
    }
    if (compiler->enclosing) { return resolveFunctionCallByTypes(parser, compiler->enclosing, name, types); }
    return {};
}


FunctionInstantiation *Parser::createInstantiation(FunctionDeclaration *functionDeclaration) {

    // TODO: Garbage collection
    auto newFunction = new ObjFunction();
    Compiler *compiler = new Compiler(newFunction, CompilerType::Function, functionDeclaration->enclosingCompiler); // @leak
    compiler->returnType = functionDeclaration->returnType;
    auto functionType = typecheckFunctionDeclaration(this, newFunction);

    // TODO: Garbage collection
    std::string originalName = std::string(functionDeclaration->name);
    newFunction->name = new ObjString(originalName);
    newFunction->arity = functionDeclaration->parameters.size();
    newFunction->functionDeclaration = functionDeclaration;

    compiler->compiled = true; // Mark this so we don't recurse

    auto [functionNameIt, added] = functionNames.try_emplace(originalName);

    std::string renamedTo = originalName;
    int size = functionNameIt->second.instantiationIndices.size() + 1;
    if (size > 1) {
        renamedTo += "__" + std::to_string(size);
    }

    FunctionInstantiation *functionInst = new FunctionInstantiation {
        functionType, newFunction, compiler, functionDeclaration, renamedTo
    }; // @leak
    functionInstantiations.push_back(functionInst);
    functionDeclaration->overloadIndices.push_back(functionInstantiations.size() - 1);

    
    functionNameIt->second.instantiationIndices.push_back(functionInstantiations.size() - 1);

    return functionInst;
}

void Parser::compileFunctionInstantiation(FunctionInstantiation *functionInst) {
    
    FunctionDeclaration *functionDeclaration = functionInst->declaration;
    ObjFunction* newFunction = &functionInst->function.asFunction();
    Type functionType = functionInst->type;

    if (functionDeclaration->isExtern) {
        typecheckFunctionDeclarationReturnValue(this, newFunction, functionType, functionDeclaration->returnType);
        return;
    } 

    Compiler *initialCompiler = this->compiler;
    Scanner *initialScanner = this->scanner;
    auto functionTypeObj = functionType->functionTypeData();
    Compiler *compiler = functionInst->compiler;

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
            decl->hasLambdas = true;
            decl->parameters.back().isStatic = true;
        }
        if (argumentType->isPrimitive()) {
            auto prim = argumentType->primitiveTypeData();
            if (prim->constructor) {
                decl->polymorphic = true;
            }
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

    if (decl->hasLambdas && !decl->isInline) {
        error("Functions taking lambdas must be declared inline.");
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
    // Small function because this must conform to the
    // parser expression interface but also we want to
    // reuse lambdaContents again

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
    decl->returnType = types::Unknown;

    function->functionDeclaration = decl;

    

    if (match(TokenType::Pipe)) {
        functionParameters(decl);
        consume(TokenType::Pipe, "Expect '|' after parameter list.");
    }
    function->arity = decl->parameters.size();

    // Must be AFTER the left brace and parameter list
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


size_t Parser::argumentList() {

    // Calls to static functions must have static arguments
    // TODO: This is quite difficult with function overloading
    // bool wasBytecode = isBytecode;
    // if (functionDeclaration->isStatic) {
    //     isBytecode = true;
    // }

    size_t argCount = 0;

    while (!match(TokenType::RightParen)) {
        if (argCount > 0) {
            consume(TokenType::Comma, "Expect ',' or ')' after argument");
        }
        expression();

        if (argCount == 255) {
            error("Can't have more than 255 arguments.");
            return 0;
        }

        // typecheckFunctionArgument(this, functionDeclaration, argCount);
        argCount ++;
    }

    if (match(TokenType::LeftBrace)) {
        lambdaContents();
        // typecheckFunctionArgument(this, functionDeclaration, argCount);
        argCount ++;
    }

    // if (argCount != functionDeclaration->parameters.size()) {
    //     error("Not enough arguments for this function.");
    // }


    // TODO: see above
    // if (functionDeclaration->isStatic) {
    //     isBytecode = wasBytecode;
    // }

    return argCount;
}


Type Parser::inlineFunction(Compiler *newCompiler, FunctionDeclaration *funDecl) {

    // There are a number of different compiler structs
    // referenced in this function, it can be quite
    // confusing.
    // 1. initialCompiler is the immediate parent function
    //    or lambda compiler that we are inlining _into_
    //    but its not necessarily the top-most function
    //    that we are inlining into.
    // 2. The top-most one is inlinedFromTop which is used
    //    to track the numInlinedReturns so we can uniquely
    //    refer to return locals and gotos.
    // 3. newCompiler is the compiler we create for the
    //    function that we are actually inlining, it will
    //    have its own locals etc.
    // 4. funDecl->enclosingCompiler is just passed to
    //    newCompiler as usual so the code can track the
    //    lexical scope.
    
    Compiler *initialCompiler = this->compiler;
    Scanner *initialScanner = this->scanner;

    // :ConfusingSubCompiler
    // TODO: Any sub-compilers from this point will have
    // enclosingCompiler = parser->compiler, which will be
    // the following newCompiler, but the lifetime of this
    // compiler ends at the end of this scope. This compiler
    // is inlined so really the enclosingCompiler should be
    // parser->compiler->inlinedFrom. It doesn't seem to be
    // causing any issues - how do we observe this?
    // TODO: Why is function needed here? Why is function
    // needed in compiler at all? just for bytecode?

    // :ReverseStack
    // This is used because the argument list has already been
    // parsed in forwards order, but we want to build the AST
    // using the familiar popExpression() and typechecking
    // functions. Reversing the function arguments means we
    // will pop in forwards order.
    size_t numNonStaticExpressions = 0;
    typecheckReverseLastNumTypes(this, funDecl->parameters.size());

    for (size_t i = 0; i < funDecl->parameters.size(); i++) {

        auto param = funDecl->parameters[i];
        newCompiler->declareVariable(this, param.name);
        Local &local = newCompiler->locals.back();
        local.isStatic = param.isStatic;
        local.type = param.type;
        typecheckVarDeclarationInitializer(this, local);

        // markInitialized after typecheck incase isStatic has changed
        // due to arg being infered as lambda
        newCompiler->markInitialized();

        if (!local.isStatic) {
            numNonStaticExpressions ++;
        }
    }

    ast->reverseLastNumExpressions(numNonStaticExpressions);
    

    newCompiler->function->argSlots = 0;
    for (size_t i = 0; i < funDecl->parameters.size(); i++) {
        Local &local = newCompiler->locals[i];

        if (!local.isStatic) {
            ast->varDeclaration(local, /* initializer */ true);
        } else {
            // Nothing happens in the VM - it is just left on
            // the stack but we should record the total slot size
            int slotSize = slotSizeOfType(newCompiler->locals.back().type);
            newCompiler->function->argSlots += slotSize;
        }
    }
    

    // Setup a temporary local on the _newCompiler_ for
    // the return value.
    int returnLocalId = -1;
    VarDeclaration *returnLocalVarDecl = nullptr;
    bool makeInlineGoto = false;

    if (newCompiler->compilerType == CompilerType::Function) {
        // We want to remap return statements into a 
        // combination of assigning to a return value and 
        // a goto, but only if this is a function - lambda
        // literals cannot be returned from
        makeInlineGoto = true;
    }

    int returnId;
    Local inlinedReturnLocal = { "", -1, -1 };

    if (makeInlineGoto) {
        // I'm not too sure about this stuff, do we want
        // to keep the AST as the _language_ syntax tree,
        // not the C target syntax tree? Maybe the returns
        // are kept but with goto data attached to it, and
        // this data is transformed only when we generate
        // the C output. This would imply that the functions
        // aren't inlined at this point, but the AST points
        // to the functions declarations. This would be
        // useful if we allow the AST to be modified by the
        // program.

        newCompiler->inlinedFromTop->numInlinedReturns ++;
        returnId = newCompiler->inlinedFromTop->numInlinedReturns;
        std::string *generatedName = new std::string(
            "__inlined_return_value" + std::to_string(
                newCompiler->inlinedFromTop->numInlinedReturns)); // @leak

        newCompiler->inlinedReturnLocal = &inlinedReturnLocal;
        inlinedReturnLocal.name = *generatedName;
        assert(funDecl->returnType);
        inlinedReturnLocal.type = funDecl->returnType;

        ast->varDeclaration(inlinedReturnLocal, false);
        returnLocalVarDecl = &mpark::get<VarDeclaration>(ast->currentBlock->lastDeclaration->variant);
    
        newCompiler->inlinedReturnLabel = "__return_label" + 
            std::to_string(newCompiler->inlinedFromTop->numInlinedReturns);
    }


    // Run the VM. We don't _have_ to do this here but it makes debugging
    // easier if we know what the stack is at this point. Also I suppose
    // we want to run any sideeffects that function arguments might have.
    vmWriter->vm.run();

    Scanner tempScanner { initialScanner->source };
    tempScanner.current = funDecl->blockStart;
    tempScanner.start = funDecl->blockStart;
    tempScanner.line = funDecl->blockLine;
    this->compiler = newCompiler;
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

    auto finalReturnType = newCompiler->returnType;
    if (finalReturnType == types::Unknown) {
        // This happens when implicitReturn and there was no
        // expression statement at the end of the function.
        // In which case imply void return type.
        assert(newCompiler->implicitReturnType);
        finalReturnType = types::Void;
    }
    if (makeInlineGoto) {
        // Hack because C won't compile a void variable lets make
        // it number because I'm lazy right right now - in the
        // future this should just not be added to the AST.
        // Rethink producing an AST with this granularity
        if (finalReturnType == types::Void) {
            finalReturnType = types::Number;
        }
        inlinedReturnLocal.type = finalReturnType;
        returnLocalVarDecl->type = finalReturnType;
    }
    compiler->expressionTypeStack.push_back(finalReturnType);

    // We need a result expression regardless of whether it's used
    // expressionStatement will consume it in any case.
    if (makeInlineGoto) {
        ast->labelStatement(newCompiler->inlinedReturnLabel);
        ast->variable(&inlinedReturnLocal);
    } else if (newCompiler->implicitReturnType) {
        ast->makeImplicitReturn();
    } else {
        // Make a unit so statements have something
        // to cling on to
        ast->unit();
    }

    return newCompiler->returnType;
}

void Parser::callFunction(FunctionCall call) {

    auto functionDeclaration = call.declaration;
    auto inst = call.instantiation;
    
    if (isBytecode && (!functionDeclaration->isStatic && !functionDeclaration->isExtern)) {
        error("Cannot call a static function from runtime.");
        return;
    }

    if (functionDeclaration->isInline && !functionDeclaration->isStatic) {
        // TODO: Why is function needed here?
        ObjFunction *function = new ObjFunction(); // @leak
        // :ConfusingSubCompiler
        Compiler newCompiler(function, CompilerType::Function, functionDeclaration->enclosingCompiler);
        newCompiler.implicitReturnType = false;
        newCompiler.returnType = functionDeclaration->returnType;
        newCompiler.inlinedFrom = compiler;
        newCompiler.inlinedFromTop = compiler->inlinedFromTop;
        newCompiler.nextStackOffset = compiler->nextStackOffset;

        inlineFunction(&newCompiler, functionDeclaration);
        return;
    }

    if (!inst) {
        inst = createInstantiation(functionDeclaration);
        typecheckInstantiationAgainstStack(this, inst, call.argCount);
        compileFunctionInstantiation(inst);
    }
    
    typecheckEndFunctionCall(this, inst->function, call.argCount);

    if (functionDeclaration->isStatic || isBytecode) { 
        vmWriter->functionCall(*inst, call.argCount);
    } else ast->functionCall(*inst, call.argCount);
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
    compiler->expressionTypeStack.pop_back(); // the lambda type

    // :ConfusingSubCompiler
    Compiler newCompiler(&function, CompilerType::Lambda, funDecl->enclosingCompiler);
    newCompiler.implicitReturnType = true;
    newCompiler.returnType = funDecl->returnType;
    newCompiler.inlinedFrom = compiler;
    newCompiler.inlinedFromTop = compiler->inlinedFromTop;
    newCompiler.nextStackOffset = compiler->nextStackOffset;

    argumentList();
    inlineFunction(&newCompiler, funDecl);

    assert(compiler->expressionTypeStack.size()); // TBH maybe there isn't one?


}