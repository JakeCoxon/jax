#include <variant.hpp>
#include <visit.hpp>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

struct Value {
    using VariantType = mpark::variant<mpark::monostate, double, bool>;
    VariantType variant;

    Value() = default;
    template <typename T> Value(T const &v): variant(v) {}

    bool isNull() { return mpark::holds_alternative<mpark::monostate>(variant); }
    bool isNumber() { return mpark::holds_alternative<double>(variant); }
    bool isBool() { return mpark::holds_alternative<bool>(variant); }

    double asNumber() { return mpark::get<double>(variant); }
    bool asBool() { return mpark::get<bool>(variant); }

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
    void operator()(const mpark::monostate n) const { os << "nil"; }
    void operator()(const double d) const { os << d; }
    void operator()(const bool b) const { os << (b ? "true" : "false"); }
};

inline std::ostream &operator<<(std::ostream &os, const Value &v) {
    v.visit(OutputVisitor { os });
    return os;
}

struct ToStringVisitor {
    std::string operator()(const mpark::monostate n) const { return "nil"; }
    std::string operator()(const double d) const { return std::to_string(d); }
    std::string operator()(const bool b) const { return (b ? "true" : "false"); }
};

std::string Value::toString() const {
    return visit(ToStringVisitor());
}

struct IsFalseyVisitor {
    bool operator()(const mpark::monostate n) const { return true; }
    bool operator()(const double d) const { return false; }
    bool operator()(const bool b) const { return !b; }
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
