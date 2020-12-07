#include <time.h>

void registry(Parser *parser) {

    registerNative(parser, "clock_seconds", types::Number, {}, 
    [&](VM *vm, int argCount) -> void {
        vm->push((double)clock() / CLOCKS_PER_SEC);
    });

    std::vector<FunctionParameter> params = {};
    params.push_back({"text", types::String});
    registerNative(parser, "add_code", types::Void, params, 
    [parser](VM *vm, int argCount) -> void {
        ObjString &str = vm->peek<Value>().asString();
        parser->generatedCodeBuffer += str.text;
    });

}