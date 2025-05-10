! {"id.inc", _OP} + ([_PUBLIC]) -> ("cat.inc", _UNARY, _BINARY, _DECL_ONE, _DECL_TWO)
! Define the macros to construct procedure names, `_UNARY(<kind_label>)` and `_BINARY(<kind_label_1>,<kind_label_2>)`, for the
! operator `_OP`.

#include "../inc/cat.inc"

#undef _UNARY
#define _UNARY(X) _CAT4(_OP,_,_TYPE_LABEL,X)
#undef _BINARY
#define _BINARY(X,Y) _CAT7(_TYPE_LABEL1,X,_,_OP,_,_TYPE_LABEL2,Y)
#undef _DECL_ONE
#undef _DECL_TWO
#ifdef _PUBLIC
#  define _DECL_ONE(X) public :: _UNARY(X)
#  define _DECL_TWO(X,Y) public :: _BINARY(X,Y)
#else
#  define _DECL_ONE(X) module procedure :: _UNARY(X)
#  define _DECL_TWO(X,Y) module procedure :: _BINARY(X,Y)
#endif
