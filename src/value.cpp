#include <variant.hpp>
#include <visit.hpp>

struct Obj {};
struct ObjString: Obj {
    std::string text;

    ObjString(const std::string &text): text(text) {}
};

struct ObjFunction;
struct ObjNative;
struct Value;
std::string toString(const Value &value);
inline std::ostream &operator<<(std::ostream &os, const Value &v);

struct Value {
    using VariantType = mpark::variant<
        mpark::monostate, double, bool, ObjString*, ObjFunction*, ObjNative*
    >;
    VariantType variant;

    Value() = default;
    template <typename T> Value(T const &v): variant(v) {}

    bool isNull() { return mpark::holds_alternative<mpark::monostate>(variant); }
    bool isNumber() { return mpark::holds_alternative<double>(variant); }
    bool isBool() { return mpark::holds_alternative<bool>(variant); }
    bool isString() { return mpark::holds_alternative<ObjString*>(variant); }
    bool isFunction() { return mpark::holds_alternative<ObjFunction*>(variant); }
    bool isNativeFunction() { return mpark::holds_alternative<ObjNative*>(variant); }

    double asNumber() { return mpark::get<double>(variant); }
    bool asBool() { return mpark::get<bool>(variant); }
    ObjString &asString() { return *mpark::get<ObjString*>(variant); }
    ObjFunction &asFunction() { return *mpark::get<ObjFunction*>(variant); }
    ObjNative &asNativeFunction() { return *mpark::get<ObjNative*>(variant); }

    template<class T> inline auto visit(T visitor) { return rollbear::visit(visitor, variant); }
    template<class T> inline auto visit(T visitor) const { return rollbear::visit(visitor, variant); }

    // inline std::string toString() const;
    inline bool isFalsey() const;
    bool operator==(Value &rhs) const;

    static Value& Nil() {
        static Value instance;
        return instance;
    }
};
