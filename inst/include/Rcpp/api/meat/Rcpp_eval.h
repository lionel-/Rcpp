// Copyright (C) 2013 Romain Francois
//
// This file is part of Rcpp.
//
// Rcpp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// Rcpp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Rcpp.  If not, see <http://www.gnu.org/licenses/>.

#ifndef Rcpp_api_meat_Rcpp_eval_h
#define Rcpp_api_meat_Rcpp_eval_h

#include <Rcpp/Interrupt.h>
#include <Rversion.h>


namespace Rcpp { namespace internal {

#ifdef RCPP_USING_UNWIND_PROTECT

struct EvalData {
    SEXP expr;
    SEXP env;
    EvalData(SEXP expr_, SEXP env_) : expr(expr_), env(env_) { }
};

inline SEXP Rcpp_protected_eval(void* eval_data) {
    EvalData* data = static_cast<EvalData*>(eval_data);
    return ::Rf_eval(data->expr, data->env);
}

// This is used internally instead of Rf_eval() to make evaluation safer
inline SEXP Rcpp_eval_impl(SEXP expr, SEXP env) {
    return Rcpp_fast_eval(expr, env);
}

#else // R < 3.5.0

// Fall back to Rf_eval() when the protect-unwind API is unavailable
inline SEXP Rcpp_eval_impl(SEXP expr, SEXP env) {
    return ::Rf_eval(expr, env);
}

#endif

}} // namespace Rcpp::internal


namespace Rcpp {

#ifdef RCPP_USING_UNWIND_PROTECT

inline SEXP Rcpp_fast_eval(SEXP expr, SEXP env) {
    internal::EvalData data(expr, env);
    return unwindProtect(&internal::Rcpp_protected_eval, &data);
}

#else

inline SEXP Rcpp_fast_eval(SEXP expr, SEXP env) {
    return Rcpp_eval(expr, env);
}

#endif

} // namespace Rcpp


namespace Rcpp { namespace internal { namespace {

const char* tryEvalExprSource =
    "tryCatch(eval(expr, env), error = handler, interrupt = handler)";

// Generate and return a child of the base env which will act like a closure
// environment for `tryEvalExpr` calls
const char* tryEvalEnvSource =
    " env <- new.env(parent = baseenv())                             \n"
    "                                                                \n"
    " evalq(envir = env, {                                           \n"
    "     expr <- NULL                                               \n"
    "     env <- NULL                                                \n"
    "                                                                \n"
    "     handler <- function(cnd) {                                 \n"
    "         structure(list(cnd), class = 'Rcpp:::caughtCondition') \n"
    "     }                                                          \n"
    "                                                                \n"
    "     environment()                                              \n"
    " })";

SEXP tryEval(SEXP expr, SEXP env) {
    static RObject tryEvalExpr = internal::parse(tryEvalExprSource);
    static Environment tryEvalEnv = internal::parseEval(tryEvalEnvSource, R_BaseEnv);

    tryEvalEnv["expr"] = expr;
    tryEvalEnv["env"] = env;

    Shield<SEXP> out(internal::Rcpp_eval_impl(tryEvalExpr, tryEvalEnv));

    // Free up `expr` and `env` for gc collection
    tryEvalEnv["expr"] = R_NilValue;
    tryEvalEnv["env"] = R_NilValue;

    return out;
}

}}} // static namespace Rcpp::internal


namespace Rcpp {

inline SEXP Rcpp_eval(SEXP expr, SEXP env) {
    Rcpp::Shelter<SEXP> shelter;
    SEXP res = shelter(internal::tryEval(expr, env));

    // Check for condition results (errors, interrupts)
    if (Rf_inherits(res, "Rcpp:::caughtCondition")) {
        if (TYPEOF(res) != VECSXP && Rf_length(res) != 1)
            stop("Internal error: Corrupt condition sentinel");

        SEXP cnd = VECTOR_ELT(res, 0);
        if (Rf_inherits(cnd, "error")) {
            SEXP conditionMessageCall = shelter(::Rf_lang2(::Rf_install("conditionMessage"), cnd));
            SEXP conditionMessage = shelter(internal::Rcpp_eval_impl(conditionMessageCall, R_BaseEnv));
            throw eval_error(CHAR(STRING_ELT(conditionMessage, 0)));
        } else if (Rf_inherits(cnd, "interrupt")) {
            throw internal::InterruptedException();
        } else {
            stop("Internal error: Unexpected condition class");
        }
    }

    return res;
}

} // namespace Rcpp

#endif
