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

    template<class T> auto visit(T visitor) { return rollbear::visit(visitor, variant); }
    template<class T> auto visit(T visitor) const { return rollbear::visit(visitor, variant); }

    inline std::string toString() const;

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
