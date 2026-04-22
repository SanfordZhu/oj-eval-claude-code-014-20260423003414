#include "Evalvisitor.h"
#include "Python3Lexer.h"
#include "Python3Parser.h"
#include "antlr4-runtime.h"
#include <iostream>
#include <sstream>
#include <iomanip>
#include <cmath>

using namespace antlr4;

EvalVisitor::EvalVisitor() {
    globalScope = new Scope();
    currentScope = globalScope;
    loopDepth = 0;
}

EvalVisitor::~EvalVisitor() {
    while (currentScope != globalScope) {
        exitScope();
    }
    delete globalScope;
}

Value EvalVisitor::callBuiltin(const std::string& name, const std::vector<Value>& args) {
    if (name == "print") {
        for (size_t i = 0; i < args.size(); i++) {
            if (i > 0) std::cout << " ";
            std::cout << args[i].toString();
        }
        std::cout << std::endl;
        return Value();
    } else if (name == "int") {
        if (args.empty()) return Value(0);
        const Value& v = args[0];
        if (v.isInt()) return v;
        if (v.isFloat()) return Value(static_cast<long long>(v.float_val));
        if (v.isBool()) return Value(v.bool_val ? 1 : 0);
        if (v.isString()) {
            try {
                return Value(BigInt(v.string_val));
            } catch (...) {
                throw RuntimeError("Invalid int conversion");
            }
        }
        throw RuntimeError("Cannot convert to int");
    } else if (name == "float") {
        if (args.empty()) return Value(0.0);
        const Value& v = args[0];
        if (v.isFloat()) return v;
        if (v.isInt()) return Value(v.int_val.toDouble());
        if (v.isBool()) return Value(v.bool_val ? 1.0 : 0.0);
        if (v.isString()) {
            try {
                return Value(std::stod(v.string_val));
            } catch (...) {
                throw RuntimeError("Invalid float conversion");
            }
        }
        throw RuntimeError("Cannot convert to float");
    } else if (name == "str") {
        if (args.empty()) return Value("");
        return Value(args[0].toString());
    } else if (name == "bool") {
        if (args.empty()) return Value(false);
        return Value(args[0].toBool());
    }
    throw RuntimeError("Unknown builtin function: " + name);
}

std::any EvalVisitor::visitFile_input(Python3Parser::File_inputContext *ctx) {
    for (auto stmt : ctx->stmt()) {
        visit(stmt);
    }
    return Value();
}

std::any EvalVisitor::visitFuncdef(Python3Parser::FuncdefContext *ctx) {
    std::string name = ctx->NAME()->toString();
    std::vector<std::string> params;
    auto typedargslist = ctx->parameters()->typedargslist();
    if (typedargslist) {
        for (size_t i = 0; i < typedargslist->tfpdef().size(); i++) {
            params.push_back(typedargslist->tfpdef(i)->NAME()->toString());
        }
    }
    functions[name] = {params, ctx->suite()};
    return Value();
}

std::any EvalVisitor::visitExpr_stmt(Python3Parser::Expr_stmtContext *ctx) {
    auto testlists = ctx->testlist();
    auto augassign = ctx->augassign();

    if (augassign) {
        // Get variable name from atom
        auto atom = testlists[0]->test(0)->or_test()->and_test(0)->not_test(0)->comparison()
            ->arith_expr(0)->term(0)->factor(0)->atom_expr()->atom();
        if (!atom->NAME()) {
            throw RuntimeError("Cannot assign to non-variable");
        }
        std::string name = atom->NAME()->toString();
        Value lhs = currentScope->get(name);
        Value rhs = std::any_cast<Value>(visit(testlists[1]));
        Value result;

        if (augassign->ADD_ASSIGN()) {
            result = lhs + rhs;
        } else if (augassign->SUB_ASSIGN()) {
            result = lhs - rhs;
        } else if (augassign->MULT_ASSIGN()) {
            result = lhs * rhs;
        } else if (augassign->DIV_ASSIGN()) {
            result = lhs / rhs;
        } else if (augassign->IDIV_ASSIGN()) {
            result = lhs.floorDiv(rhs);
        } else if (augassign->MOD_ASSIGN()) {
            result = lhs.mod(rhs);
        }

        currentScope->set(name, result);
        return Value();
    }

    // Assignment
    auto assigns = ctx->ASSIGN();
    if (!assigns.empty()) {
        // Chained assignment: a = b = c = value
        Value value = std::any_cast<Value>(visit(testlists.back()));

        for (size_t i = 0; i < testlists.size() - 1; i++) {
            auto testlist = testlists[i];
            auto tests = testlist->test();
            if (tests.size() == 1) {
                // Single variable assignment
                auto atom = tests[0]->or_test()->and_test(0)->not_test(0)->comparison()
                    ->arith_expr(0)->term(0)->factor(0)->atom_expr()->atom();
                if (atom->NAME()) {
                    std::string name = atom->NAME()->toString();
                    currentScope->set(name, value);
                }
            } else {
                // Multiple assignment: a, b = 1, 2
                if (value.isTuple()) {
                    for (size_t j = 0; j < tests.size() && j < value.tuple_val.size(); j++) {
                        auto atom = tests[j]->or_test()->and_test(0)->not_test(0)->comparison()
                            ->arith_expr(0)->term(0)->factor(0)->atom_expr()->atom();
                        if (atom->NAME()) {
                            std::string name = atom->NAME()->toString();
                            currentScope->set(name, value.tuple_val[j]);
                        }
                    }
                }
            }
        }
    } else {
        // Just an expression
        return visit(testlists[0]);
    }
    return Value();
}

std::any EvalVisitor::visitFlow_stmt(Python3Parser::Flow_stmtContext *ctx) {
    if (ctx->break_stmt()) {
        return visit(ctx->break_stmt());
    } else if (ctx->continue_stmt()) {
        return visit(ctx->continue_stmt());
    } else if (ctx->return_stmt()) {
        return visit(ctx->return_stmt());
    }
    return Value();
}

std::any EvalVisitor::visitIf_stmt(Python3Parser::If_stmtContext *ctx) {
    auto tests = ctx->test();
    auto suites = ctx->suite();

    for (size_t i = 0; i < tests.size(); i++) {
        Value cond = std::any_cast<Value>(visit(tests[i]));
        if (cond.toBool()) {
            visit(suites[i]);
            return Value();
        }
    }

    if (ctx->ELSE()) {
        visit(suites.back());
    }
    return Value();
}

std::any EvalVisitor::visitWhile_stmt(Python3Parser::While_stmtContext *ctx) {
    loopDepth++;
    try {
        while (true) {
            Value cond = std::any_cast<Value>(visit(ctx->test()));
            if (!cond.toBool()) break;
            visit(ctx->suite());
        }
    } catch (BreakException&) {
        // Break out of loop
    } catch (ContinueException&) {
        // Continue to next iteration
        loopDepth--;
        throw;
    }
    loopDepth--;
    return Value();
}

std::any EvalVisitor::visitSuite(Python3Parser::SuiteContext *ctx) {
    if (ctx->simple_stmt()) {
        return visit(ctx->simple_stmt());
    }
    for (auto stmt : ctx->stmt()) {
        visit(stmt);
    }
    return Value();
}

std::any EvalVisitor::visitOr_test(Python3Parser::Or_testContext *ctx) {
    auto and_tests = ctx->and_test();
    if (and_tests.size() == 1) {
        return visit(and_tests[0]);
    }

    for (size_t i = 0; i < and_tests.size(); i++) {
        Value result = std::any_cast<Value>(visit(and_tests[i]));
        if (result.toBool()) {
            return Value(true);
        }
    }
    return Value(false);
}

std::any EvalVisitor::visitAnd_test(Python3Parser::And_testContext *ctx) {
    auto not_tests = ctx->not_test();
    if (not_tests.size() == 1) {
        return visit(not_tests[0]);
    }

    for (size_t i = 0; i < not_tests.size(); i++) {
        Value result = std::any_cast<Value>(visit(not_tests[i]));
        if (!result.toBool()) {
            return Value(false);
        }
    }
    return Value(true);
}

std::any EvalVisitor::visitNot_test(Python3Parser::Not_testContext *ctx) {
    if (ctx->NOT()) {
        Value val = std::any_cast<Value>(visit(ctx->not_test()));
        return Value(!val.toBool());
    }
    return visit(ctx->comparison());
}

std::any EvalVisitor::visitComparison(Python3Parser::ComparisonContext *ctx) {
    auto arith_exprs = ctx->arith_expr();
    auto comp_ops = ctx->comp_op();

    if (comp_ops.empty()) {
        return visit(arith_exprs[0]);
    }

    // Evaluate all arith_exprs first (each evaluated at most once)
    std::vector<Value> values;
    for (auto ae : arith_exprs) {
        values.push_back(std::any_cast<Value>(visit(ae)));
    }

    // Chain comparisons with and
    for (size_t i = 0; i < comp_ops.size(); i++) {
        Value lhs = values[i];
        Value rhs = values[i + 1];
        Value result;

        auto op = comp_ops[i];
        if (op->LESS_THAN()) {
            result = Value(lhs < rhs);
        } else if (op->GREATER_THAN()) {
            result = Value(lhs > rhs);
        } else if (op->EQUALS()) {
            result = Value(lhs == rhs);
        } else if (op->GT_EQ()) {
            result = Value(lhs >= rhs);
        } else if (op->LT_EQ()) {
            result = Value(lhs <= rhs);
        } else if (op->NOT_EQ_2()) {
            result = Value(lhs != rhs);
        }

        if (!result.toBool()) {
            return Value(false);
        }
    }
    return Value(true);
}

std::any EvalVisitor::visitArith_expr(Python3Parser::Arith_exprContext *ctx) {
    auto terms = ctx->term();
    auto ops = ctx->addorsub_op();

    if (ops.empty()) {
        return visit(terms[0]);
    }

    Value result = std::any_cast<Value>(visit(terms[0]));
    for (size_t i = 0; i < ops.size(); i++) {
        Value rhs = std::any_cast<Value>(visit(terms[i + 1]));
        if (ops[i]->ADD()) {
            result = result + rhs;
        } else {
            result = result - rhs;
        }
    }
    return result;
}

std::any EvalVisitor::visitTerm(Python3Parser::TermContext *ctx) {
    auto factors = ctx->factor();
    auto ops = ctx->muldivmod_op();

    if (ops.empty()) {
        return visit(factors[0]);
    }

    Value result = std::any_cast<Value>(visit(factors[0]));
    for (size_t i = 0; i < ops.size(); i++) {
        Value rhs = std::any_cast<Value>(visit(factors[i + 1]));
        if (ops[i]->STAR()) {
            result = result * rhs;
        } else if (ops[i]->DIV()) {
            result = result / rhs;
        } else if (ops[i]->IDIV()) {
            result = result.floorDiv(rhs);
        } else if (ops[i]->MOD()) {
            result = result.mod(rhs);
        }
    }
    return result;
}

std::any EvalVisitor::visitFactor(Python3Parser::FactorContext *ctx) {
    if (ctx->ADD()) {
        Value val = std::any_cast<Value>(visit(ctx->factor()));
        return val;
    } else if (ctx->MINUS()) {
        Value val = std::any_cast<Value>(visit(ctx->factor()));
        if (val.isInt()) {
            return Value(-val.int_val);
        } else if (val.isFloat()) {
            return Value(-val.float_val);
        }
        throw RuntimeError("Unary minus not supported for this type");
    }
    return visit(ctx->atom_expr());
}

std::any EvalVisitor::visitAtom_expr(Python3Parser::Atom_exprContext *ctx) {
    // If there's a trailer, it's a function call
    if (ctx->trailer()) {
        // Function call
        std::string funcName;
        auto atom = ctx->atom();

        if (atom->NAME()) {
            funcName = atom->NAME()->toString();
        } else {
            throw RuntimeError("Not a function");
        }

        std::vector<Value> args;
        std::map<std::string, Value> kwargs;
        auto arglist = ctx->trailer()->arglist();
        if (arglist) {
            auto arguments = arglist->argument();
            for (auto arg : arguments) {
                if (arg->ASSIGN()) {
                    // Keyword argument
                    std::string key = arg->test(0)->or_test()->and_test(0)->not_test(0)->comparison()
                        ->arith_expr(0)->term(0)->factor(0)->atom_expr()->atom()->NAME()->toString();
                    Value val = std::any_cast<Value>(visit(arg->test(1)));
                    kwargs[key] = val;
                } else {
                    // Positional argument
                    args.push_back(std::any_cast<Value>(visit(arg->test(0))));
                }
            }
        }

        // Check if it's a builtin function
        if (funcName == "print" || funcName == "int" || funcName == "float" ||
            funcName == "str" || funcName == "bool") {
            return callBuiltin(funcName, args);
        }

        // User-defined function
        auto it = functions.find(funcName);
        if (it == functions.end()) {
            throw RuntimeError("Undefined function: " + funcName);
        }

        auto& funcInfo = it->second;
        const auto& params = funcInfo.first;
        auto* suite = funcInfo.second;

        enterScope();

        // Bind parameters
        size_t param_idx = 0;
        for (const auto& param : params) {
            Value val;
            if (param_idx < args.size()) {
                val = args[param_idx];
            } else if (kwargs.find(param) != kwargs.end()) {
                val = kwargs[param];
            } else {
                // Default value - for now use None
                val = Value();
            }
            currentScope->set(param, val);
            param_idx++;
        }

        try {
            visit(suite);
            exitScope();
            return Value();
        } catch (ReturnValue& rv) {
            exitScope();
            return rv.value;
        }
    }

    // No trailer - just evaluate the atom
    return visit(ctx->atom());
}

std::any EvalVisitor::visitAtom(Python3Parser::AtomContext *ctx) {
    if (ctx->NAME()) {
        std::string name = ctx->NAME()->toString();
        return currentScope->get(name);
    } else if (ctx->NUMBER()) {
        std::string numStr = ctx->NUMBER()->toString();
        if (numStr.find('.') != std::string::npos || numStr.find('e') != std::string::npos ||
            numStr.find('E') != std::string::npos) {
            return Value(std::stod(numStr));
        }
        return Value(BigInt(numStr));
    } else if (ctx->NONE()) {
        return Value();
    } else if (ctx->TRUE()) {
        return Value(true);
    } else if (ctx->FALSE()) {
        return Value(false);
    } else if (ctx->format_string()) {
        return visit(ctx->format_string());
    } else if (ctx->OPEN_PAREN()) {
        // Tuple or parenthesized expression
        auto test = ctx->test();
        if (test) {
            return visit(test);
        }
        return Value();
    } else if (!ctx->STRING().empty()) {
        std::string result;
        for (auto s : ctx->STRING()) {
            result += s->toString();
        }
        // Remove quotes
        if (result.length() >= 2) {
            char quote = result[0];
            if (quote == '"' || quote == '\'') {
                if (result[0] == quote && result.back() == quote) {
                    result = result.substr(1, result.length() - 2);
                }
            }
        }
        return Value(result);
    }
    return Value();
}

std::any EvalVisitor::visitFormat_string(Python3Parser::Format_stringContext *ctx) {
    std::string result;

    auto testlists = ctx->testlist();
    size_t test_idx = 0;

    // Process format string - iterate through children
    for (auto child : ctx->children) {
        auto terminal = dynamic_cast<tree::TerminalNode*>(child);

        if (terminal) {
            std::string text = terminal->toString();
            if (text == "{" || text == "}") {
                // Skip - these are delimiters
            } else if (terminal->getSymbol()->getType() == Python3Parser::FORMAT_STRING_LITERAL) {
                result += text;
            }
        } else {
            // This is a testlist (expression inside {})
            Value val = std::any_cast<Value>(visit(testlists[test_idx++]));
            result += val.toString();
        }
    }

    return Value(result);
}

std::any EvalVisitor::visitTestlist(Python3Parser::TestlistContext *ctx) {
    auto tests = ctx->test();
    if (tests.size() == 1) {
        return visit(tests[0]);
    }

    std::vector<Value> values;
    for (auto test : tests) {
        values.push_back(std::any_cast<Value>(visit(test)));
    }
    return Value(values);
}

std::any EvalVisitor::visitArglist(Python3Parser::ArglistContext *ctx) {
    return Value();
}

std::any EvalVisitor::visitArgument(Python3Parser::ArgumentContext *ctx) {
    return Value();
}

std::any EvalVisitor::visitTest(Python3Parser::TestContext *ctx) {
    return visit(ctx->or_test());
}

std::any EvalVisitor::visitSmall_stmt(Python3Parser::Small_stmtContext *ctx) {
    if (ctx->expr_stmt()) {
        return visit(ctx->expr_stmt());
    }
    return visit(ctx->flow_stmt());
}

std::any EvalVisitor::visitSimple_stmt(Python3Parser::Simple_stmtContext *ctx) {
    return visit(ctx->small_stmt());
}

std::any EvalVisitor::visitStmt(Python3Parser::StmtContext *ctx) {
    if (ctx->simple_stmt()) {
        return visit(ctx->simple_stmt());
    }
    return visit(ctx->compound_stmt());
}

std::any EvalVisitor::visitCompound_stmt(Python3Parser::Compound_stmtContext *ctx) {
    if (ctx->if_stmt()) {
        return visit(ctx->if_stmt());
    } else if (ctx->while_stmt()) {
        return visit(ctx->while_stmt());
    } else if (ctx->funcdef()) {
        return visit(ctx->funcdef());
    }
    return Value();
}

std::any EvalVisitor::visitBreak_stmt(Python3Parser::Break_stmtContext *ctx) {
    if (loopDepth <= 0) {
        throw RuntimeError("break outside loop");
    }
    throw BreakException();
}

std::any EvalVisitor::visitContinue_stmt(Python3Parser::Continue_stmtContext *ctx) {
    if (loopDepth <= 0) {
        throw RuntimeError("continue outside loop");
    }
    throw ContinueException();
}

std::any EvalVisitor::visitReturn_stmt(Python3Parser::Return_stmtContext *ctx) {
    auto testlist = ctx->testlist();
    if (testlist) {
        Value val = std::any_cast<Value>(visit(testlist));
        throw ReturnValue(val);
    }
    throw ReturnValue(Value());
}

std::any EvalVisitor::visitParameters(Python3Parser::ParametersContext *ctx) {
    return Value();
}

std::any EvalVisitor::visitTypedargslist(Python3Parser::TypedargslistContext *ctx) {
    return Value();
}

std::any EvalVisitor::visitTfpdef(Python3Parser::TfpdefContext *ctx) {
    return Value();
}

std::any EvalVisitor::visitTrailer(Python3Parser::TrailerContext *ctx) {
    return Value();
}

std::any EvalVisitor::visitComp_op(Python3Parser::Comp_opContext *ctx) {
    return Value();
}

std::any EvalVisitor::visitAddorsub_op(Python3Parser::Addorsub_opContext *ctx) {
    return Value();
}

std::any EvalVisitor::visitMuldivmod_op(Python3Parser::Muldivmod_opContext *ctx) {
    return Value();
}
