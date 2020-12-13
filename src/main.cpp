#include <time.h>
#include <string>
#include <fstream>
#include <iostream>
#include <stdlib.h>


// #define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION
// #define DEBUG_PRINT_C_CODE

#include "scanner.cpp"
#include "vm.cpp"
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

double clock_ms() { return (double)clock() / CLOCKS_PER_SEC * 1000.0; }

static void runFile(const std::string &path) {
    
    double startTime = clock_ms();

    std::string source = readFile(path);
    
    double startTimeCompile = clock_ms();
    std::ostringstream output;
    bool success = compileToString(source, output);
    double compileTime = clock_ms() - startTimeCompile;

    

    if (success) {
#ifdef DEBUG_PRINT_C_CODE
        std::cout << output.str() << std::endl;
#endif

        std::ofstream myfile;
        myfile.open("output.c");
        myfile << output.str() << endl;
        myfile.close();


        int res = system("clang -Wunused-value output.c -o output");
        double everythingTime = clock_ms() - startTime;

        if (res != 0) return;

        double startRuntime = clock_ms();
        system("./output");
        double runTime = clock_ms() - startRuntime;
        printf("Compile to c string: %fms\n", compileTime);
        printf("File op + c compile: %fms\n", (everythingTime - compileTime));
        printf("           Run time: %fms\n", runTime);
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
