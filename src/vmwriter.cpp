
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