#include <variant.hpp>
#include <visit.hpp>

struct Obj {};
struct ObjString: Obj {
    std::string text;

    ObjString(const std::string &text): text(text) {}
};

struct ObjFunction;
struct ObjNative;
struct ObjResource;
struct Value;
std::ostream &operator<<(std::ostream &os, const Value &v);

struct Value {
    using VariantType = mpark::variant<
        mpark::monostate, double, bool, ObjString*, ObjFunction*, ObjNative*, ObjResource*
    >;
    VariantType variant;

    Value() = default;
    Value(const Value &) = default;
    template <typename T> Value(T const &v): variant(v) {}

    bool isNull() { return mpark::holds_alternative<mpark::monostate>(variant); }
    bool isNumber() { return mpark::holds_alternative<double>(variant); }
    bool isBool() { return mpark::holds_alternative<bool>(variant); }
    bool isString() { return mpark::holds_alternative<ObjString*>(variant); }
    bool isFunction() { return mpark::holds_alternative<ObjFunction*>(variant); }
    bool isNativeFunction() { return mpark::holds_alternative<ObjNative*>(variant); }
    bool isResource() { return mpark::holds_alternative<ObjResource*>(variant); }

    double asNumber() { return mpark::get<double>(variant); }
    bool asBool() { return mpark::get<bool>(variant); }
    ObjString &asString() { return *mpark::get<ObjString*>(variant); }
    ObjFunction &asFunction() { return *mpark::get<ObjFunction*>(variant); }
    ObjNative &asNativeFunction() { return *mpark::get<ObjNative*>(variant); }
    ObjResource &asResource() { return *mpark::get<ObjResource*>(variant); }

    template<class T> inline auto visit(T visitor) { return rollbear::visit(visitor, variant); }
    template<class T> inline auto visit(T visitor) const { return rollbear::visit(visitor, variant); }

    std::string toString();

    inline bool isFalsey() const;
    bool operator==(Value &rhs) const;

    static Value& Nil() {
        static Value instance;
        return instance;
    }
};
