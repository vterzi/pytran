#undef _TYPE_LEN
#undef _ELEM_TYPE
#undef _ARG_TYPE
#undef _RES_TYPE
#if _TYPE_ID == _CHARACTER
#  define _TYPE_LEN(X) _TYPE(len=X, kind=_KIND)
#  define _ELEM_TYPE _TYPE_LEN(:), allocatable
#  define _ARG_TYPE _TYPE_LEN(*)
#  define _RES_TYPE _ELEM_TYPE
#else
#  define _TYPE_LEN(X) _TYPE(_KIND)
#  define _ELEM_TYPE _TYPE_LEN()
#  define _ARG_TYPE _ELEM_TYPE
#  define _RES_TYPE _ELEM_TYPE
#endif
