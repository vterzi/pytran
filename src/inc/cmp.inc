#undef _EQ
#undef _NE
#if _TYPE_ID == _LOGICAL
#  define _EQ .eqv.
#  define _NE .neqv.
#else
#  define _EQ ==
#  define _NE /=
#endif
