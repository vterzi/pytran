#undef _EQ
#undef _NE
#undef _LT
#undef _LE
#undef _GT
#undef _GE
#if _TYPE_ID == _LOGICAL
#  define _EQ(X,Y) ((X) .eqv. (Y))
#  define _NE(X,Y) ((X) .neqv. (Y))
#  define _LT(X,Y) ((.not. (X)) .and. (Y))
#  define _LE(X,Y) ((.not. (X)) .or. (Y))
#  define _GT(X,Y) ((X) .and. (.not. (Y)))
#  define _GE(X,Y) ((X) .or. (.not. (Y)))
#else
#  define _EQ(X,Y) ((X) == (Y))
#  define _NE(X,Y) ((X) /= (Y))
#  if _TYPE_ID == _COMPLEX
#    define _LT(X,Y) (abs(X) < abs(Y))
#    define _LE(X,Y) (abs(X) <= abs(Y))
#    define _GT(X,Y) (abs(X) > abs(Y))
#    define _GE(X,Y) (abs(X) >= abs(Y))
#  else
#    define _LT(X,Y) ((X) < (Y))
#    define _LE(X,Y) ((X) <= (Y))
#    define _GT(X,Y) ((X) > (Y))
#    define _GE(X,Y) ((X) >= (Y))
#  endif
#endif
