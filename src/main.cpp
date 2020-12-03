
#include <string>
#include <fstream>
#include <iostream>
#include <stdlib.h>

#include "scanner.cpp"
#include "value.cpp"
#include "vm.cpp"
#include "debug.cpp"
#include "compiler.cpp"
#include "natives.cpp"

std::string readFile(const std::string &path) {
    // http://insanecoding.blogspot.com/2011/11/how-to-read-in-file-in-c.html

    std::ifstream in(path, std::ios::in);
    if (!in) {
        std::cerr << "Could not open file " << path << std::endl;
        exit(74);
    }

    std::string contents;
    in.seekg(0, std::ios::end);
    contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&contents[0], contents.size());
    in.close();
    return contents;
}

// static void runFile(VM &vm, const std::string &path) {
//     std::string source = readFile(path);
//     InterpretResult result = vm.interpret(source);

//     if (result == InterpretResult::CompileError) exit(65);
//     if (result == InterpretResult::RuntimeError) exit(70);
// }

static void runFile(const std::string &path) {
    std::string source = readFile(path);
    std::string output = compileToString(source);

    std::cout << output << std::endl;

    std::ofstream myfile;
    myfile.open("output.c");
    myfile << output;
    myfile.close();

    system("clang output.c -o output");
    system("./output");
}

static void repl() {
    VM vm;
    std::string line;

    while (true) {
        tfm::printf("> ");

        if (!getline(std::cin, line)) {
            tfm::printf("\n");
            break;
        }

        vm.interpret(line);
    }
}

int main(int argc, const char* argv[]) {

    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        runFile(argv[1]);
    } else {
        std::cerr << "Usage: jax [path]" << std::endl;
        exit(64);
    }

    return 0;
}
