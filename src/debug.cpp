
static int simpleInstruction(const Chunk &chunk, const char* name, int offset) {
    std::cout << name << std::endl;
    return offset + 1;
}

static int byteInstruction(const Chunk &chunk, const char* name, int offset) {
    uint8_t slot = chunk.code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    return offset + 2; 
}

static int jumpInstruction(const Chunk &chunk, const char* name, int sign, int offset) {
    uint16_t jump = (uint16_t)(chunk.code[offset + 1] << 8);
    jump |= chunk.code[offset + 2];
    printf("%-16s %4d -> %d\n", name, offset,
            offset + 3 + sign * jump);
    return offset + 3;
}

static int constantInstruction(const Chunk &chunk, const char* name, int offset) {
    uint8_t constant = chunk.code[offset + 1];
    printf("%-16s %4d '", name, constant);
    std::cout << chunk.constants[constant];
    printf("'\n");
    return offset + 2;
}


int disassembleInstruction(const Chunk &chunk, int offset) {
    printf("%04d ", offset);

    if (offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1]) {
        printf("   | ");
    } else {
        printf("%4d ", chunk.lines[offset]);
    }

    auto instruction = OpCode(chunk.code[offset]);
    switch (instruction) {
        case OpCode::Constant:
            return constantInstruction(chunk, "Constant", offset);
        case OpCode::Nil:
            return simpleInstruction(chunk, "Nil", offset);
        case OpCode::True:
            return simpleInstruction(chunk, "True", offset);
        case OpCode::False:
            return simpleInstruction(chunk, "False", offset);
        case OpCode::Pop:
            return simpleInstruction(chunk, "Pop", offset);
        case OpCode::DefineGlobal:
            return constantInstruction(chunk, "DefineGlobal", offset);
        case OpCode::GetGlobal:
            return constantInstruction(chunk, "GetGlobal", offset);
        case OpCode::SetGlobal:
            return constantInstruction(chunk, "SetGlobal", offset);
        case OpCode::GetLocal:
            return byteInstruction(chunk, "GetLocal", offset);
        case OpCode::SetLocal:
            return byteInstruction(chunk, "SetLocal", offset);
        case OpCode::Equal:
            return simpleInstruction(chunk, "Equal", offset);
        case OpCode::Less:
            return simpleInstruction(chunk, "Less", offset);
        case OpCode::Greater:
            return simpleInstruction(chunk, "Greater", offset);
        case OpCode::Add:
            return simpleInstruction(chunk, "Add", offset);
        case OpCode::Subtract:
            return simpleInstruction(chunk, "Subtract", offset);
        case OpCode::Multiply:
            return simpleInstruction(chunk, "Multiply", offset);
        case OpCode::Divide:
            return simpleInstruction(chunk, "Divide", offset);
        case OpCode::Not:
            return simpleInstruction(chunk, "Not", offset);
        case OpCode::Negate:
            return simpleInstruction(chunk, "Negate", offset);
        case OpCode::Print:
            return simpleInstruction(chunk, "Print", offset);
        case OpCode::Jump:
            return jumpInstruction(chunk, "Jump", 1, offset);
        case OpCode::JumpIfFalse:
            return jumpInstruction(chunk, "JumpIfFalse", 1, offset);
        case OpCode::Loop:
            return jumpInstruction(chunk, "Loop", -1, offset);
        case OpCode::Return:
            return simpleInstruction(chunk, "Return", offset);
    }
}

void disassembleChunk(const Chunk &chunk, const std::string &name) {
    printf("== %s ==\n", name.c_str());

    for (unsigned long offset = 0; offset < chunk.code.size();) {
        offset = disassembleInstruction(chunk, offset);
    }
}