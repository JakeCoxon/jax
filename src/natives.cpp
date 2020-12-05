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

    {
        // int arrayType = addNewType(parser, {"array"});

        // registerNative(parser, "array", arrayType, {}, 
        // [&](VM *vm, int argCount, Value *args) -> Value {
        //     // TODO: Garbage collection
        //     return new ObjResource(new std::vector<Value>());
        // });

        // registerNative(parser, "size", TypeId::Number, {{"array", arrayType}}, 
        // [&](VM *vm, int argCount, Value *args) -> Value {
        //     auto array = (std::vector<Value>*)(args[0].asResource().pointer);
        //     size_t size = array->size();
        //     return (double)size;
        // });

        // registerNative(parser, "add", TypeId::Void, {{"array", arrayType}, {"item", TypeId::Dynamic}}, 
        // [&](VM *vm, int argCount, Value *args) -> Value {
        //     auto array = (std::vector<Value>*)(args[0].asResource().pointer);
        //     array->push_back(args[1]);
        //     return mpark::monostate{};
        // });

        // registerNative(parser, "get", TypeId::Dynamic, {{"array", arrayType}, {"index", TypeId::Number}}, 
        // [&](VM *vm, int argCount, Value *args) -> Value {
        //     auto array = (std::vector<Value>*)(args[0].asResource().pointer);
        //     Value v = array->at((int)args[1].asNumber());
        //     return v;
        // });
    }
}