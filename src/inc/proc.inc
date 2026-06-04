! {"id.inc", _OP} + ([_PUBLIC]) -> ("cat.inc", _UNARY, _BINARY, _DECL_ONE, _DECL_TWO)
! Define the macros to construct procedure names, `_UNARY(<kind-label>)` and `_BINARY(<kind-label-1>,<kind-label-2>)`, for the
! operator `_OP`.

#define _(X) X

#undef _UNARY
#define _UNARY(X) _(_OP)_(_)_(_TYPE_LABEL)_(X)
#undef _BINARY
#define _BINARY(X,Y) _(_TYPE_LABEL1)_(X)_(_)_(_OP)_(_)_(_TYPE_LABEL2)_(Y)
#undef _DECL_ONE
#undef _DECL_TWO
#ifdef _PUBLIC
#  undef _PUBLIC
#  define _DECL_ONE(X) public :: _UNARY(X)
#  define _DECL_TWO(X,Y) public :: _BINARY(X,Y)
#else
#  define _DECL_ONE(X) module procedure :: _UNARY(X)
#  define _DECL_TWO(X,Y) module procedure :: _BINARY(X,Y)
#endif
