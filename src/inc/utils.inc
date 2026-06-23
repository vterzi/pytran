#define _(X) X
#define _CAT2(X,Y) _(X)_(_)_(Y)
#define _CAT3(X,Y,Z) _(X)_(_)_(Y)_(_)_(Z)
#define _LABEL _(_TYPE_LABEL)_(_KIND_LABEL)
#define _LABEL1 _(_TYPE_LABEL1)_(_KIND_LABEL1)
#define _LABEL2 _(_TYPE_LABEL2)_(_KIND_LABEL2)
#define _UNARY_OP(X) _CAT2(X,_LABEL)
#define _BINARY_OP(X) _CAT3(_LABEL1,X,_LABEL2)
