#include <time.h>
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

double clock_seconds() { return (double)clock() / CLOCKS_PER_SEC; }

static void runFile(const std::string &path) {
    
    double startTime = clock_seconds();

    std::string source = readFile(path);
    
    double startTimeCompile = clock_seconds();
    std::string output = compileToString(source);
    double compileTime = clock_seconds() - startTimeCompile;

    if (output.size()) {
        std::cout << output << std::endl;

        std::ofstream myfile;
        myfile.open("output.c");
        myfile << output;
        myfile.close();

        double compileTimeFile = clock_seconds() - startTime;

        system("clang output.c -o output");
        system("./output");
        printf("Compile time: %fs (%fs of which file op)\n", compileTime, compileTimeFile - compileTime);
    }
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
