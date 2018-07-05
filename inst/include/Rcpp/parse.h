// parse.h: Rcpp R/C++ interface class library -- parse() / parseEval()
//
// Copyright (C) 2018 RStudio
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

#ifndef RCPP_PARSE_H
#define RCPP_PARSE_H


namespace Rcpp { namespace internal {

#include <R_ext/Parse.h>

inline SEXP parseImpl(const char *str) {
    Shelter<SEXP> shelter;
    SEXP code = shelter(Rf_mkString(str));

    ParseStatus status;
    SEXP exprs = shelter(R_ParseVector(code, -1, &status, R_NilValue));
    if (status != PARSE_OK || TYPEOF(exprs) != EXPRSXP || LENGTH(exprs) != 1)
        stop("Internal error while parsing Rcpp code");

    return VECTOR_ELT(exprs, 0);
}

inline SEXP parseEvalImpl(const char *str, SEXP env) {
    Shield<SEXP> code(parseImpl(str));
    // Use normal Rf_eval() because we unwind-protect in parseEval()
    return Rf_eval(code, env);
}

struct parseData {
    parseData(const char* str_) : str(str_)
    { }
    const char* str;
};
struct parseEvalData {
    parseEvalData(const char* str_, SEXP env_)
        : str(str_),
          env(env_)
    { }
    const char* str;
    SEXP env;
};

inline SEXP parseCallback(void* cbData) {
    parseData* data = static_cast<parseData*>(cbData);
    return parseImpl(data->str);
}
inline SEXP parseEvalCallback(void* cbData) {
    parseEvalData* data = static_cast<parseEvalData*>(cbData);
    return parseEvalImpl(data->str, data->env);
}

// These include a code path for unprotected invokation. Thus internal
// invokations should only be used with code that is safe to parse/eval.
inline SEXP parse(const char* str) {
#ifdef RCPP_USING_UNWIND_PROTECT
    parseData cbData(str);
    return unwindProtect(&parseCallback, &cbData);
#else
    return parseImpl(str);
#endif
}

inline SEXP parseEval(const char* str, SEXP env) {
#ifdef RCPP_USING_UNWIND_PROTECT
    parseEvalData cbData(str, env);
    return unwindProtect(&parseEvalCallback, &cbData);
#else
    return parseEvalImpl(str, env);
#endif
}

}} // namespace Rcpp::internal


#endif
