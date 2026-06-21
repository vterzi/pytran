#define _(X) X
#define _CAT2(X,Y) _(X)_(_)_(Y)
#define _CAT3(X,Y,Z) _(X)_(_)_(Y)_(_)_(Z)
#define _LABEL _(_TYPE_LABEL)_(_KIND_LABEL)
