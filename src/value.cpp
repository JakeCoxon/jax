#include <variant.hpp>
#include <visit.hpp>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

struct Obj {};
struct ObjString: Obj {
    std::string text;
};

struct Value {
    using VariantType = mpark::variant<mpark::monostate, double, bool, ObjString*>;
    VariantType variant;

    Value() = default;
    template <typename T> Value(T const &v): variant(v) {}

    bool isNull() { return mpark::holds_alternative<mpark::monostate>(variant); }
    bool isNumber() { return mpark::holds_alternative<double>(variant); }
    bool isBool() { return mpark::holds_alternative<bool>(variant); }
    bool isString() { return mpark::holds_alternative<ObjString*>(variant); }

    double asNumber() { return mpark::get<double>(variant); }
    bool asBool() { return mpark::get<bool>(variant); }
    ObjString &asString() { return *mpark::get<ObjString*>(variant); }

    template<class T> inline auto visit(T visitor) { return rollbear::visit(visitor, variant); }
    template<class T> inline auto visit(T visitor) const { return rollbear::visit(visitor, variant); }

    inline std::string toString() const;
    inline bool isFalsey() const;
    bool operator==(Value &rhs) const;

    static Value& Nil() {
        static Value instance;
        return instance;
    }
};

struct OutputVisitor {
    std::ostream &os;
    void operator()(mpark::monostate n) { os << "nil"; }
    void operator()(double d) { os << d; }
    void operator()(bool b) { os << (b ? "true" : "false"); }
    void operator()(ObjString *s) { os << s->text; }

    template <typename T> bool operator()(T b) = delete; // Catch non-explicit conversions
};

inline std::ostream &operator<<(std::ostream &os, const Value &v) {
    v.visit(OutputVisitor { os });
    return os;
}

struct ToStringVisitor {
    std::string operator()(mpark::monostate n) { return "nil"; }
    std::string operator()(double d) { return std::to_string(d); }
    std::string operator()(bool b) { return (b ? "true" : "false"); }
    std::string operator()(ObjString *s) { return s->text; }

    template <typename T> bool operator()(T b) = delete; // Catch non-explicit conversions
};

std::string Value::toString() const {
    return visit(ToStringVisitor());
}

struct IsFalseyVisitor {
    bool operator()(mpark::monostate n) { return true; }
    bool operator()(double d) { return false; }
    bool operator()(bool b) { return !b; }
    bool operator()(ObjString *s) { return false; }

    template <typename T> bool operator()(T b) = delete; // Catch non-explicit conversions
};

bool Value::isFalsey() const {
    return visit(IsFalseyVisitor());
}

struct IsEqualVisitor {
    template<class T> bool operator()(const T a, const T b) const { return a == b; }
    template<class T, class U> bool operator()(const T a, const U b) const { return false; }
};

bool Value::operator==(Value &rhs) const {
    return rollbear::visit(IsEqualVisitor{}, variant, rhs.variant);
}
