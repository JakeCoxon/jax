#include <time.h>

void registry(Parser *parser) {
    registerNative(parser, "clock", TypeId::Number, {}, 
    [&](VM *vm, int argCount, Value *args) -> Value {
        return (double)clock() / CLOCKS_PER_SEC;
    });
}