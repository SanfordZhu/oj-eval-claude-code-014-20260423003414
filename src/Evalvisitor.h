#pragma once
#ifndef PYTHON_INTERPRETER_EVALVISITOR_H
#define PYTHON_INTERPRETER_EVALVISITOR_H

#include "Python3ParserBaseVisitor.h"
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <stdexcept>
#include <algorithm>
#include <sstream>
#include <iomanip>
#include <cmath>

class RuntimeError : public std::runtime_error {
public:
    RuntimeError(const std::string& msg) : std::runtime_error(msg) {}
};

// Big integer implementation
class BigInt {
private:
    std::string digits;
    bool negative;

    void normalize() {
        while (digits.length() > 1 && digits.back() == '0') {
            digits.pop_back();
        }
        if (digits.length() == 1 && digits[0] == '0') {
            negative = false;
        }
    }

    int compareAbs(const BigInt& other) const {
        if (digits.length() != other.digits.length()) {
            return digits.length() - other.digits.length();
        }
        for (int i = digits.length() - 1; i >= 0; i--) {
            if (digits[i] != other.digits[i]) {
                return digits[i] - other.digits[i];
            }
        }
        return 0;
    }

public:
    BigInt() : digits("0"), negative(false) {}
    BigInt(int v) : negative(v < 0) {
        long long val = v;
        if (val < 0) val = -val;
        if (val == 0) {
            digits = "0";
        } else {
            while (val > 0) {
                digits.push_back('0' + (val % 10));
                val /= 10;
            }
        }
    }
    BigInt(long long v) : negative(v < 0) {
        if (v < 0) v = -v;
        if (v == 0) {
            digits = "0";
        } else {
            while (v > 0) {
                digits.push_back('0' + (v % 10));
                v /= 10;
            }
        }
    }
    BigInt(const std::string& s) : negative(false) {
        size_t start = 0;
        if (s.length() > 0 && s[0] == '-') {
            negative = true;
            start = 1;
        } else if (s.length() > 0 && s[0] == '+') {
            start = 1;
        }
        digits = "";
        for (size_t i = start; i < s.length(); i++) {
            if (s[i] >= '0' && s[i] <= '9') {
                digits.push_back(s[i]);
            }
        }
        std::reverse(digits.begin(), digits.end());
        normalize();
    }

    bool isZero() const {
        return digits.length() == 1 && digits[0] == '0';
    }

    bool isNegative() const {
        return negative && !isZero();
    }

    BigInt operator-() const {
        BigInt result = *this;
        if (!isZero()) {
            result.negative = !negative;
        }
        return result;
    }

    bool operator==(const BigInt& other) const {
        if (negative != other.negative) return false;
        return compareAbs(other) == 0;
    }

    bool operator!=(const BigInt& other) const {
        return !(*this == other);
    }

    bool operator<(const BigInt& other) const {
        if (negative != other.negative) {
            return negative;
        }
        int cmp = compareAbs(other);
        if (negative) {
            return cmp > 0;
        }
        return cmp < 0;
    }

    bool operator<=(const BigInt& other) const {
        return *this < other || *this == other;
    }

    bool operator>(const BigInt& other) const {
        return !(*this <= other);
    }

    bool operator>=(const BigInt& other) const {
        return !(*this < other);
    }

    BigInt operator+(const BigInt& other) const {
        if (negative && other.negative) {
            BigInt result = addAbs(*this, other);
            result.negative = true;
            return result;
        }
        if (!negative && !other.negative) {
            return addAbs(*this, other);
        }
        if (negative) {
            if (compareAbs(other) > 0) {
                BigInt result = subAbs(*this, other);
                result.negative = true;
                return result;
            } else {
                return subAbs(other, *this);
            }
        }
        if (compareAbs(other) >= 0) {
            return subAbs(*this, other);
        } else {
            BigInt result = subAbs(other, *this);
            result.negative = true;
            return result;
        }
    }

    BigInt operator-(const BigInt& other) const {
        return *this + (-other);
    }

    BigInt operator*(const BigInt& other) const {
        BigInt result;
        result.digits.resize(digits.length() + other.digits.length(), '0');
        for (size_t i = 0; i < digits.length(); i++) {
            int carry = 0;
            for (size_t j = 0; j < other.digits.length(); j++) {
                int prod = (digits[i] - '0') * (other.digits[j] - '0') +
                          (result.digits[i + j] - '0') + carry;
                result.digits[i + j] = '0' + (prod % 10);
                carry = prod / 10;
            }
            size_t k = i + other.digits.length();
            while (carry > 0) {
                int sum = (result.digits[k] - '0') + carry;
                result.digits[k] = '0' + (sum % 10);
                carry = sum / 10;
                k++;
            }
        }
        result.negative = negative != other.negative;
        result.normalize();
        return result;
    }

    BigInt operator/(const BigInt& other) const {
        if (other.isZero()) {
            throw RuntimeError("Division by zero");
        }
        BigInt quotient, remainder;
        divMod(*this, other, quotient, remainder);
        return quotient;
    }

    BigInt operator%(const BigInt& other) const {
        if (other.isZero()) {
            throw RuntimeError("Modulo by zero");
        }
        BigInt quotient, remainder;
        divMod(*this, other, quotient, remainder);
        return remainder;
    }

    BigInt floorDiv(const BigInt& other) const {
        if (other.isZero()) {
            throw RuntimeError("Division by zero");
        }
        BigInt quotient, remainder;
        divMod(*this, other, quotient, remainder);
        // Python floor division: if signs differ and remainder != 0, subtract 1
        if (negative != other.negative && !remainder.isZero()) {
            quotient = quotient - BigInt(1);
        }
        return quotient;
    }

    double toDouble() const {
        double result = 0.0;
        double place = 1.0;
        for (char c : digits) {
            result += (c - '0') * place;
            place *= 10.0;
        }
        if (negative) result = -result;
        return result;
    }

    std::string toString() const {
        std::string result = digits;
        std::reverse(result.begin(), result.end());
        if (negative && !isZero()) {
            result = "-" + result;
        }
        return result;
    }

private:
    static BigInt addAbs(const BigInt& a, const BigInt& b) {
        BigInt result;
        result.digits.clear();
        int carry = 0;
        size_t maxLen = std::max(a.digits.length(), b.digits.length());
        for (size_t i = 0; i < maxLen || carry > 0; i++) {
            int sum = carry;
            if (i < a.digits.length()) sum += a.digits[i] - '0';
            if (i < b.digits.length()) sum += b.digits[i] - '0';
            result.digits.push_back('0' + (sum % 10));
            carry = sum / 10;
        }
        return result;
    }

    static BigInt subAbs(const BigInt& a, const BigInt& b) {
        BigInt result;
        result.digits.clear();
        int borrow = 0;
        for (size_t i = 0; i < a.digits.length(); i++) {
            int diff = (a.digits[i] - '0') - borrow;
            if (i < b.digits.length()) diff -= (b.digits[i] - '0');
            if (diff < 0) {
                diff += 10;
                borrow = 1;
            } else {
                borrow = 0;
            }
            result.digits.push_back('0' + diff);
        }
        result.normalize();
        return result;
    }

    static void divMod(const BigInt& a, const BigInt& b, BigInt& q, BigInt& r) {
        if (b.isZero()) {
            throw RuntimeError("Division by zero");
        }
        q.digits.clear();
        r.digits.clear();
        for (int i = a.digits.length() - 1; i >= 0; i--) {
            r.digits.insert(r.digits.begin(), a.digits[i]);
            r.normalize();
            int count = 0;
            while (r.compareAbs(b) >= 0) {
                r = subAbs(r, b);
                count++;
            }
            q.digits.insert(q.digits.begin(), '0' + count);
        }
        q.normalize();
        r.normalize();
        q.negative = a.negative != b.negative;
        r.negative = a.negative;
    }
};

enum class ValueType {
    NONE,
    BOOL,
    INT,
    FLOAT,
    STRING,
    TUPLE
};

class Value {
public:
    ValueType type;
    bool bool_val;
    BigInt int_val;
    double float_val;
    std::string string_val;
    std::vector<Value> tuple_val;

    Value() : type(ValueType::NONE), bool_val(false), int_val(0), float_val(0.0) {}
    Value(bool v) : type(ValueType::BOOL), bool_val(v), int_val(0), float_val(0.0) {}
    Value(int v) : type(ValueType::INT), bool_val(false), int_val(v), float_val(0.0) {}
    Value(long long v) : type(ValueType::INT), bool_val(false), int_val(v), float_val(0.0) {}
    Value(const BigInt& v) : type(ValueType::INT), bool_val(false), int_val(v), float_val(0.0) {}
    Value(double v) : type(ValueType::FLOAT), bool_val(false), int_val(0), float_val(v) {}
    Value(const std::string& v) : type(ValueType::STRING), bool_val(false), int_val(0), float_val(0.0), string_val(v) {}
    Value(const char* v) : type(ValueType::STRING), bool_val(false), int_val(0), float_val(0.0), string_val(v) {}
    Value(const std::vector<Value>& v) : type(ValueType::TUPLE), bool_val(false), int_val(0), float_val(0.0), tuple_val(v) {}

    bool isNone() const { return type == ValueType::NONE; }
    bool isBool() const { return type == ValueType::BOOL; }
    bool isInt() const { return type == ValueType::INT; }
    bool isFloat() const { return type == ValueType::FLOAT; }
    bool isString() const { return type == ValueType::STRING; }
    bool isTuple() const { return type == ValueType::TUPLE; }
    bool isNumber() const { return isInt() || isFloat(); }

    bool toBool() const {
        switch (type) {
            case ValueType::NONE: return false;
            case ValueType::BOOL: return bool_val;
            case ValueType::INT: return !int_val.isZero();
            case ValueType::FLOAT: return float_val != 0.0;
            case ValueType::STRING: return !string_val.empty();
            case ValueType::TUPLE: return !tuple_val.empty();
        }
        return false;
    }

    std::string toString() const {
        switch (type) {
            case ValueType::NONE: return "None";
            case ValueType::BOOL: return bool_val ? "True" : "False";
            case ValueType::INT: return int_val.toString();
            case ValueType::FLOAT: {
                char buf[100];
                snprintf(buf, sizeof(buf), "%.6f", float_val);
                std::string s(buf);
                // Remove trailing zeros after decimal point
                size_t dot = s.find('.');
                if (dot != std::string::npos) {
                    size_t last_non_zero = s.find_last_not_of('0');
                    if (last_non_zero != std::string::npos) {
                        if (s[last_non_zero] == '.') {
                            s = s.substr(0, last_non_zero + 1);
                        } else {
                            s = s.substr(0, last_non_zero + 1);
                        }
                    }
                }
                // Ensure at least one decimal place for float
                if (s.find('.') == std::string::npos) {
                    s += ".0";
                }
                return s;
            }
            case ValueType::STRING: return string_val;
            case ValueType::TUPLE: {
                std::string s = "(";
                for (size_t i = 0; i < tuple_val.size(); i++) {
                    if (i > 0) s += ", ";
                    s += tuple_val[i].toString();
                }
                if (tuple_val.size() == 1) s += ",";
                s += ")";
                return s;
            }
        }
        return "";
    }

    std::string repr() const {
        if (isString()) {
            return "\"" + string_val + "\"";
        }
        return toString();
    }

    bool operator==(const Value& other) const {
        if (type != other.type) {
            if (isNumber() && other.isNumber()) {
                if (isFloat() || other.isFloat()) {
                    return getFloat() == other.getFloat();
                }
                return getInt() == other.getInt();
            }
            return false;
        }
        switch (type) {
            case ValueType::NONE: return true;
            case ValueType::BOOL: return bool_val == other.bool_val;
            case ValueType::INT: return int_val == other.int_val;
            case ValueType::FLOAT: return float_val == other.float_val;
            case ValueType::STRING: return string_val == other.string_val;
            case ValueType::TUPLE:
                if (tuple_val.size() != other.tuple_val.size()) return false;
                for (size_t i = 0; i < tuple_val.size(); i++) {
                    if (!(tuple_val[i] == other.tuple_val[i])) return false;
                }
                return true;
        }
        return false;
    }

    bool operator!=(const Value& other) const {
        return !(*this == other);
    }

    bool operator<(const Value& other) const {
        if (isNumber() && other.isNumber()) {
            if (isFloat() || other.isFloat()) {
                return getFloat() < other.getFloat();
            }
            return getInt() < other.getInt();
        }
        if (isString() && other.isString()) {
            return string_val < other.string_val;
        }
        throw RuntimeError("Cannot compare these types");
    }

    bool operator<=(const Value& other) const {
        return *this < other || *this == other;
    }

    bool operator>(const Value& other) const {
        return !(*this <= other);
    }

    bool operator>=(const Value& other) const {
        return !(*this < other);
    }

    Value operator+(const Value& other) const {
        if (isString() && other.isString()) {
            return Value(string_val + other.string_val);
        }
        if (isNumber() && other.isNumber()) {
            if (isFloat() || other.isFloat()) {
                return Value(getFloat() + other.getFloat());
            }
            return Value(int_val + other.int_val);
        }
        if (isString() && other.isNumber()) {
            return Value(string_val + other.toString());
        }
        if (isNumber() && other.isString()) {
            return Value(toString() + other.string_val);
        }
        throw RuntimeError("Unsupported operand types for +");
    }

    Value operator-(const Value& other) const {
        if (isNumber() && other.isNumber()) {
            if (isFloat() || other.isFloat()) {
                return Value(getFloat() - other.getFloat());
            }
            return Value(int_val - other.int_val);
        }
        throw RuntimeError("Unsupported operand types for -");
    }

    Value operator*(const Value& other) const {
        if (isString() && other.isInt()) {
            std::string result;
            BigInt n = other.int_val;
            if (n < BigInt(0)) n = BigInt(0);
            for (BigInt i = BigInt(0); i < n; i = i + BigInt(1)) {
                result += string_val;
            }
            return Value(result);
        }
        if (isInt() && other.isString()) {
            std::string result;
            BigInt n = int_val;
            if (n < BigInt(0)) n = BigInt(0);
            for (BigInt i = BigInt(0); i < n; i = i + BigInt(1)) {
                result += other.string_val;
            }
            return Value(result);
        }
        if (isNumber() && other.isNumber()) {
            if (isFloat() || other.isFloat()) {
                return Value(getFloat() * other.getFloat());
            }
            return Value(int_val * other.int_val);
        }
        throw RuntimeError("Unsupported operand types for *");
    }

    Value operator/(const Value& other) const {
        if (isNumber() && other.isNumber()) {
            double d = other.getFloat();
            if (d == 0.0) throw RuntimeError("Division by zero");
            return Value(getFloat() / d);
        }
        throw RuntimeError("Unsupported operand types for /");
    }

    Value floorDiv(const Value& other) const {
        if (isNumber() && other.isNumber()) {
            if (isFloat() || other.isFloat()) {
                double a = getFloat();
                double b = other.getFloat();
                if (b == 0.0) throw RuntimeError("Division by zero");
                double result = std::floor(a / b);
                return Value(result);
            }
            return Value(int_val.floorDiv(other.int_val));
        }
        throw RuntimeError("Unsupported operand types for //");
    }

    Value mod(const Value& other) const {
        if (isNumber() && other.isNumber()) {
            if (isFloat() || other.isFloat()) {
                double a = getFloat();
                double b = other.getFloat();
                if (b == 0.0) throw RuntimeError("Modulo by zero");
                // Python modulo: a % b = a - (a // b) * b
                double q = std::floor(a / b);
                return Value(a - q * b);
            }
            return Value(int_val % other.int_val);
        }
        throw RuntimeError("Unsupported operand types for %");
    }

    BigInt getInt() const {
        if (isInt()) return int_val;
        if (isFloat()) return BigInt(static_cast<long long>(float_val));
        if (isBool()) return BigInt(bool_val ? 1 : 0);
        throw RuntimeError("Cannot convert to int");
    }

    double getFloat() const {
        if (isFloat()) return float_val;
        if (isInt()) return int_val.toDouble();
        if (isBool()) return bool_val ? 1.0 : 0.0;
        throw RuntimeError("Cannot convert to float");
    }
};

class Scope {
public:
    std::map<std::string, Value> variables;
    Scope* parent;

    Scope(Scope* p = nullptr) : parent(p) {}

    Value get(const std::string& name) {
        auto it = variables.find(name);
        if (it != variables.end()) {
            return it->second;
        }
        if (parent) {
            return parent->get(name);
        }
        throw RuntimeError("Undefined variable: " + name);
    }

    void set(const std::string& name, const Value& val) {
        variables[name] = val;
    }

    bool has(const std::string& name) {
        if (variables.find(name) != variables.end()) return true;
        if (parent) return parent->has(name);
        return false;
    }
};

class ReturnValue {
public:
    Value value;
    ReturnValue(const Value& v) : value(v) {}
};

class BreakException {};
class ContinueException {};

class EvalVisitor : public Python3ParserBaseVisitor {
private:
    Scope* currentScope;
    Scope* globalScope;
    std::map<std::string, std::pair<std::vector<std::string>, Python3Parser::SuiteContext*>> functions;
    int loopDepth;

    void enterScope() {
        currentScope = new Scope(currentScope);
    }

    void exitScope() {
        Scope* old = currentScope;
        currentScope = currentScope->parent;
        delete old;
    }

    Value evalBool(Value v) {
        return Value(v.toBool());
    }

    Value callBuiltin(const std::string& name, const std::vector<Value>& args);

public:
    EvalVisitor();
    ~EvalVisitor();

    virtual std::any visitFile_input(Python3Parser::File_inputContext *ctx) override;
    virtual std::any visitFuncdef(Python3Parser::FuncdefContext *ctx) override;
    virtual std::any visitExpr_stmt(Python3Parser::Expr_stmtContext *ctx) override;
    virtual std::any visitFlow_stmt(Python3Parser::Flow_stmtContext *ctx) override;
    virtual std::any visitIf_stmt(Python3Parser::If_stmtContext *ctx) override;
    virtual std::any visitWhile_stmt(Python3Parser::While_stmtContext *ctx) override;
    virtual std::any visitSuite(Python3Parser::SuiteContext *ctx) override;
    virtual std::any visitOr_test(Python3Parser::Or_testContext *ctx) override;
    virtual std::any visitAnd_test(Python3Parser::And_testContext *ctx) override;
    virtual std::any visitNot_test(Python3Parser::Not_testContext *ctx) override;
    virtual std::any visitComparison(Python3Parser::ComparisonContext *ctx) override;
    virtual std::any visitArith_expr(Python3Parser::Arith_exprContext *ctx) override;
    virtual std::any visitTerm(Python3Parser::TermContext *ctx) override;
    virtual std::any visitFactor(Python3Parser::FactorContext *ctx) override;
    virtual std::any visitAtom_expr(Python3Parser::Atom_exprContext *ctx) override;
    virtual std::any visitAtom(Python3Parser::AtomContext *ctx) override;
    virtual std::any visitFormat_string(Python3Parser::Format_stringContext *ctx) override;
    virtual std::any visitTestlist(Python3Parser::TestlistContext *ctx) override;
    virtual std::any visitArglist(Python3Parser::ArglistContext *ctx) override;
    virtual std::any visitArgument(Python3Parser::ArgumentContext *ctx) override;
    virtual std::any visitTest(Python3Parser::TestContext *ctx) override;
    virtual std::any visitSmall_stmt(Python3Parser::Small_stmtContext *ctx) override;
    virtual std::any visitSimple_stmt(Python3Parser::Simple_stmtContext *ctx) override;
    virtual std::any visitStmt(Python3Parser::StmtContext *ctx) override;
    virtual std::any visitCompound_stmt(Python3Parser::Compound_stmtContext *ctx) override;
    virtual std::any visitBreak_stmt(Python3Parser::Break_stmtContext *ctx) override;
    virtual std::any visitContinue_stmt(Python3Parser::Continue_stmtContext *ctx) override;
    virtual std::any visitReturn_stmt(Python3Parser::Return_stmtContext *ctx) override;
    virtual std::any visitParameters(Python3Parser::ParametersContext *ctx) override;
    virtual std::any visitTypedargslist(Python3Parser::TypedargslistContext *ctx) override;
    virtual std::any visitTfpdef(Python3Parser::TfpdefContext *ctx) override;
    virtual std::any visitTrailer(Python3Parser::TrailerContext *ctx) override;
    virtual std::any visitComp_op(Python3Parser::Comp_opContext *ctx) override;
    virtual std::any visitAddorsub_op(Python3Parser::Addorsub_opContext *ctx) override;
    virtual std::any visitMuldivmod_op(Python3Parser::Muldivmod_opContext *ctx) override;
};

#endif//PYTHON_INTERPRETER_EVALVISITOR_H
