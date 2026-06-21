#undef _EQ
#undef _NE
#if _TYPE_ID == _LOGICAL
#  define _EQ(X,Y) X .eqv. Y
#  define _NE(X,Y) X .neqv. Y
#else
#  define _EQ(X,Y) X == Y
#  define _NE(X,Y) X /= Y
#endif
