! () -> (_, _CAT, _CAT3, _CAT4, _CAT5, _CAT6, _CAT7, _CAT8, _CAT9)
! Define the concatenation macros for up to 9 arguments.  The macro for two
! arguments is `_CAT(<arg-1>,<arg-2>)`.  The macros for more arguments contain
! the number of arguments as a suffix in their names.

#undef _
#define _(X) X
#undef _CAT
#define _CAT(A,B) _(A)_(B)
#undef _CAT3
#define _CAT3(A,B,C) _(A)_(B)_(C)
#undef _CAT4
#define _CAT4(A,B,C,D) _(A)_(B)_(C)_(D)
#undef _CAT5
#define _CAT5(A,B,C,D,E) _(A)_(B)_(C)_(D)_(E)
#undef _CAT6
#define _CAT6(A,B,C,D,E,F) _(A)_(B)_(C)_(D)_(E)_(F)
#undef _CAT7
#define _CAT7(A,B,C,D,E,F,G) _(A)_(B)_(C)_(D)_(E)_(F)_(G)
#undef _CAT8
#define _CAT8(A,B,C,D,E,F,G,H) _(A)_(B)_(C)_(D)_(E)_(F)_(G)_(H)
#undef _CAT9
#define _CAT9(A,B,C,D,E,F,G,H,I) _(A)_(B)_(C)_(D)_(E)_(F)_(G)_(H)_(I)
