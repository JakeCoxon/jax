
static int simpleInstruction(const Chunk &chunk, const char* name, int offset) {
    tfm::printf("%s\n", name);
    return offset + 1;
}

static int byteInstruction(const Chunk &chunk, const char* name, int offset) {
    uint8_t slot = chunk.code[offset + 1];
    tfm::printf("%-16s %4d\n", name, slot);
    return offset + 2; 
}

static int jumpInstruction(const Chunk &chunk, const char* name, int sign, int offset) {
    uint16_t jump = (uint16_t)(chunk.code[offset + 1] << 8);
    jump |= chunk.code[offset + 2];
    tfm::printf("%-16s %4d -> %d\n", name, offset,
            offset + 3 + sign * jump);
    return offset + 3;
}

static int constantInstructionDouble(const Chunk &chunk, const char* name, int offset) {
    uint8_t constant = chunk.code[offset + 1];
    // uint16_t constant = (uint16_t)(chunk.code[offset + 1] << 8);
    // constant |= chunk.code[offset + 2];
    tfm::printf("%-16s %4d '%s'\n", name, constant, chunk.getDoubleConstant(constant));
    return offset + 2;
}
static int constantInstruction(const Chunk &chunk, const char* name, int offset) {
    uint8_t constant = chunk.code[offset + 1];
    // uint16_t constant = (uint16_t)(chunk.code[offset + 1] << 8);
    // constant |= chunk.code[offset + 2];
    tfm::printf("%-16s %4d '%s'\n", name, constant, chunk.getConstant(constant));
    return offset + 2;
}


int disassembleInstruction(const Chunk &chunk, int offset) {
    tfm::printf("%04d ", offset);

    if (offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1]) {
        tfm::printf("   | ");
    } else {
        tfm::printf("%4d ", chunk.lines[offset]);
    }

    auto instruction = OpCode(chunk.code[offset]);
    switch (instruction) {
        case OpCode::Constant:
            return constantInstruction(chunk, "Constant", offset);
        case OpCode::ConstantDouble:
            return constantInstructionDouble(chunk, "ConstantDouble", offset);
        case OpCode::Nil:
            return simpleInstruction(chunk, "Nil", offset);
        case OpCode::True:
            return simpleInstruction(chunk, "True", offset);
        case OpCode::False:
            return simpleInstruction(chunk, "False", offset);
        case OpCode::Pop:
            return byteInstruction(chunk, "Pop", offset);
        case OpCode::GetLocal:
            return byteInstruction(chunk, "GetLocal", offset);
        case OpCode::GetLocalDouble:
            return byteInstruction(chunk, "GetLocalDouble", offset);
        case OpCode::SetLocal:
            return byteInstruction(chunk, "SetLocal", offset);
        case OpCode::SetLocalDouble:
            return byteInstruction(chunk, "SetLocalDouble", offset);
        case OpCode::Equal:
            return simpleInstruction(chunk, "Equal", offset);
        case OpCode::EqualDouble:
            return simpleInstruction(chunk, "EqualDouble", offset);
        case OpCode::Less:
            return simpleInstruction(chunk, "Less", offset);
        case OpCode::Greater:
            return simpleInstruction(chunk, "Greater", offset);
        case OpCode::Add:
            return simpleInstruction(chunk, "Add", offset);
        case OpCode::AddDouble:
            return simpleInstruction(chunk, "AddDouble", offset);
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
            return byteInstruction(chunk, "Print", offset);
        case OpCode::PrintDouble:
            return byteInstruction(chunk, "PrintDouble", offset);
        case OpCode::ToStringDouble:
            return simpleInstruction(chunk, "ToStringDouble", offset);
        case OpCode::StringFormat:
            return byteInstruction(chunk, "StringFormat", offset);
        case OpCode::Jump:
            return jumpInstruction(chunk, "Jump", 1, offset);
        case OpCode::JumpIfFalse:
            return jumpInstruction(chunk, "JumpIfFalse", 1, offset);
        case OpCode::Loop:
            return jumpInstruction(chunk, "Loop", -1, offset);
        case OpCode::Call:
            return byteInstruction(chunk, "Call", offset);
        case OpCode::Return:
            return simpleInstruction(chunk, "Return", offset);
    }
}

void disassembleChunk(const Chunk &chunk, const std::string &name) {
    tfm::printf("== %s ==\n", name);

    for (unsigned long offset = 0; offset < chunk.code.size();) {
        offset = disassembleInstruction(chunk, offset);
    }
}