
struct IfStatementPatchState {
    int conditionSlots;
    int thenJump;
    int elseJump;
    int endJump;
};

struct WhileStatementPatchState {
    int conditionSlots;
    int loopStart;
    int exitJump;
};

struct VmWriter {

    Parser *parser;
    
    void emitReturn();
    void emitConstant(Value value);
    void emitDoubleConstant(double value);
    int emitJump(OpCode instruction);
    void patchJump(int offset);
    void emitLoop(int loopStart);
    Chunk &currentChunk();
    void emitByte(uint8_t byte);
    void emitByte(OpCode opcode);
    void emitTwoBytes(uint16_t bytes);

    void endCompiler() {
        emitReturn();
    }
    void endScope() {
        int numSlots = 0;
        Compiler *compiler = parser->compiler;

        while (compiler->locals.size() > 0 &&
                compiler->locals.back().depth >
                compiler->scopeDepth) {
            numSlots += slotSizeOfType(compiler->locals.back().type);
        }

        emitByte(OpCode::Pop);
        emitByte(numSlots);
    }

    void print() {
        emitByte(OpCode::Print);
        emitByte(1); // 1 only for now
    }
    void returnStatement(bool isNil) {
        if (isNil) {
            emitReturn();
        } else {
            emitByte(OpCode::Return);
        }
    }
    void exprStatement() {
        emitByte(OpCode::Pop);
        emitByte(slotSizeOfType(parser->compiler->expressionTypeStack.back()));
    }

    void varDeclarationNoValue() {
        emitByte(OpCode::Nil);
    }

    void number(Token token) {
        // https://stackoverflow.com/questions/11752705/does-stdstring-contain-null-terminator
        // Don't want to risk it - just convert to a new string instead
        double value = strtod(std::string(token.text).c_str(), nullptr);
        emitDoubleConstant(value);
    }

    void booleanLiteral(bool value) {
        emitByte(value ? OpCode::True : OpCode::False);
    }
    void nilLiteral() {
        emitByte(OpCode::Nil);
    }

    void unary(TokenType operatorType) {
        switch (operatorType) {
            case TokenType::Bang:  emitByte(OpCode::Not); break;
            case TokenType::Minus: emitByte(OpCode::Negate); break;
            default:
                return;
        }
    }

    void string(Token token) {

        auto str = token.text;
        str.remove_prefix(1);
        str.remove_suffix(1);
        // TODO: Garbage collection
        auto objStr = new ObjString(std::string(str));
        emitConstant(objStr);
    }

    void infix(TokenType operatorType) {

        Type type = parser->compiler->expressionTypeStack.back();
        bool isNumber = type == types::Number || type == types::Bool;
        
        switch (operatorType) {
            case TokenType::BangEqual:     emitByte(isNumber ? OpCode::EqualDouble : OpCode::Equal); emitByte(OpCode::Not); break;
            case TokenType::EqualEqual:    emitByte(isNumber ? OpCode::EqualDouble : OpCode::Equal); break;
            case TokenType::Greater:       emitByte(OpCode::Greater); break;
            case TokenType::GreaterEqual:  emitByte(OpCode::Less); emitByte(OpCode::Not); break;
            case TokenType::Less:          emitByte(OpCode::Less); break;
            case TokenType::LessEqual:     emitByte(OpCode::Greater); emitByte(OpCode::Not); break;
            case TokenType::Minus:         emitByte(OpCode::Subtract); break;
            case TokenType::Star:          emitByte(OpCode::Multiply); break;
            case TokenType::Slash:         emitByte(OpCode::Divide); break;
            case TokenType::Plus: {
                // if (concatenation) {
                //     emitByte(OpCode::Add);
                // } else {
                    emitByte(OpCode::AddDouble);
                // }
                break;
            }
            default: break;
        }
    }

    IfStatementPatchState beginIfStatementBlock() {

        IfStatementPatchState state;
        state.conditionSlots = slotSizeOfType(parser->compiler->expressionTypeStack.back());
        state.thenJump = emitJump(OpCode::JumpIfFalse);

        emitByte(OpCode::Pop);
        emitByte(state.conditionSlots);

        return state;
    }

    void elseStatementBlock(IfStatementPatchState *state) {
        state->elseJump = emitJump(OpCode::Jump);

        patchJump(state->thenJump);
        emitByte(OpCode::Pop);
        emitByte(state->conditionSlots);
    }

    void endIfStatementBlock(IfStatementPatchState *state) {
        patchJump(state->elseJump);
    }

    WhileStatementPatchState beginWhileStatementBlock() {
        WhileStatementPatchState state;

        state.loopStart = currentChunk().code.size();
        state.conditionSlots = slotSizeOfType(parser->compiler->expressionTypeStack.back());

        state.exitJump = emitJump(OpCode::JumpIfFalse);

        emitByte(OpCode::Pop);
        emitByte(state.conditionSlots);
        return state;
    }

    void endWhileStatementBlock(WhileStatementPatchState *state) {
        emitLoop(state->loopStart);

        patchJump(state->exitJump);
        emitByte(OpCode::Pop);
        emitByte(state->conditionSlots);
    }


    WhileStatementPatchState beginAndExpression() {
        WhileStatementPatchState state;
        state.conditionSlots = slotSizeOfType(parser->compiler->expressionTypeStack.back());
        state.exitJump = emitJump(OpCode::JumpIfFalse);

        emitByte(OpCode::Pop);
        emitByte(state.conditionSlots);

        return state;
    }

    void endAndCondition(WhileStatementPatchState *state) {
        patchJump(state->exitJump);
    }

    IfStatementPatchState beginOrExpression() {
        IfStatementPatchState state;
        state.conditionSlots = slotSizeOfType(parser->compiler->expressionTypeStack.back());
        state.elseJump = emitJump(OpCode::JumpIfFalse);
        state.endJump = emitJump(OpCode::Jump);
        patchJump(state.elseJump);

        emitByte(OpCode::Pop);
        emitByte(state.conditionSlots);

        return state;
    }
    void endOrCondition(IfStatementPatchState *state) {
        patchJump(state->endJump);
    }

    void functionCall(FunctionInstantiation &inst, int argCount) {
        // We used to patch some stuff but maybe not anymore
        // emitByte(OpCode::Constant);
        // emitByte(0xFF);
        // emitByte(0xFF);
        // size_t patchIndex = currentChunk().code.size() - 1;
        // currentChunk().code[patchIndex] = (constant >> 8) & 0xff;
        // currentChunk().code[patchIndex] = constant & 0xff;

        uint16_t constant = parser->makeConstant(inst.function);
        assert(constant < 256);
        
        emitByte(OpCode::Call);
        emitByte(constant);
    }

    void namedVariable(int localIndex, bool assignment) {
        OpCode getOp = OpCode::GetLocal;
        OpCode setOp = OpCode::SetLocal;

        Type type = parser->compiler->locals[localIndex].type;
        if (type == types::Number || type == types::Bool) {
            getOp = OpCode::GetLocalDouble;
            setOp = OpCode::SetLocalDouble;
        }

        size_t stackOffset = parser->compiler->locals[localIndex].stackOffset;
        assert(stackOffset < 256);

        if (assignment) {
            emitByte(setOp); emitByte((uint8_t)stackOffset);
        } else {
            emitByte(getOp); emitByte((uint8_t)stackOffset);
        }
        
    }



};
Chunk &VmWriter::currentChunk() { 
    return parser->compiler->function->chunk;
}
void VmWriter::emitByte(uint8_t byte) {
    currentChunk().write(byte, parser->previous().line);
}
void VmWriter::emitByte(OpCode opcode) {
    emitByte(static_cast<uint8_t>(opcode));
}
void VmWriter::emitTwoBytes(uint16_t bytes) {
    currentChunk().write((bytes << 8) & 0xff, parser->previous().line);
    currentChunk().write(bytes & 0xff, parser->previous().line);
}
void VmWriter::emitReturn() {
    emitByte(OpCode::Nil);
    emitByte(OpCode::Return);
}

void VmWriter::emitConstant(Value value) {
    uint16_t constantIndex = parser->makeConstant(value);

    if (constantIndex < 256) {
        emitByte(OpCode::Constant); emitByte(constantIndex);
    } else {
        assert(false); // Not implemented yet. Use a wide opcode
    }
    // emitByte(OpCode::Constant); emitTwoBytes(constantIndex);
}
void VmWriter::emitDoubleConstant(double value) {
    uint16_t constantIndex = currentChunk().addDoubleConstant(value);

    if (constantIndex < 256) {
        emitByte(OpCode::ConstantDouble); emitByte(constantIndex);
    } else {
        assert(false); // Not implemented yet. Use a wide opcode
    }
    // emitByte(OpCode::Constant); emitTwoBytes(constantIndex);
}


int VmWriter::emitJump(OpCode instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk().code.size() - 2;
}

void VmWriter::patchJump(int offset) {
    int jump = currentChunk().code.size() - offset - 2;

    if (jump > UINT16_MAX) {
        parser->error("Too much code to jump over.");
        return;
    }

    currentChunk().code[offset] = (jump >> 8) & 0xff;
    currentChunk().code[offset + 1] = jump & 0xff;
}

void VmWriter::emitLoop(int loopStart) {
    emitByte(OpCode::Loop);

    int offset = currentChunk().code.size() - loopStart + 2;
    if (offset > UINT16_MAX) parser->error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}