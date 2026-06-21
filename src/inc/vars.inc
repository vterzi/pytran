#undef _TYPE_LEN1
#undef _ELEM_TYPE1
#undef _ARG_TYPE1
#undef _RES_TYPE1
#if _TYPE_ID1 == _CHARACTER
#  define _TYPE_LEN1(X) _TYPE1(len=X, kind=_KIND1)
#  define _ELEM_TYPE1 _TYPE_LEN1(:), allocatable
#  define _ARG_TYPE1 _TYPE_LEN1(*)
#  define _RES_TYPE1 _ELEM_TYPE1
#else
#  define _TYPE_LEN1(X) _TYPE1(_KIND1)
#  define _ELEM_TYPE1 _TYPE_LEN1()
#  define _ARG_TYPE1 _ELEM_TYPE1
#  define _RES_TYPE1 _ELEM_TYPE1
#endif
#undef _TYPE_LEN2
#undef _ELEM_TYPE2
#undef _ARG_TYPE2
#undef _RES_TYPE2
#if _TYPE_ID2 == _CHARACTER
#  define _TYPE_LEN2(X) _TYPE2(len=X, kind=_KIND2)
#  define _ELEM_TYPE2 _TYPE_LEN2(:), allocatable
#  define _ARG_TYPE2 _TYPE_LEN2(*)
#  define _RES_TYPE2 _ELEM_TYPE2
#else
#  define _TYPE_LEN2(X) _TYPE2(_KIND2)
#  define _ELEM_TYPE2 _TYPE_LEN2()
#  define _ARG_TYPE2 _ELEM_TYPE2
#  define _RES_TYPE2 _ELEM_TYPE2
#endif
#undef _TYPE_LEN0
#undef _ELEM_TYPE0
#undef _ARG_TYPE0
#undef _RES_TYPE0
#if (_TYPE_ID1 | _KIND1) >= (_TYPE_ID2 | _KIND2)
#  define _TYPE_LEN0 _TYPE_LEN1
#  define _ELEM_TYPE0 _ELEM_TYPE1
#  define _ARG_TYPE0 _ARG_TYPE1
#  define _RES_TYPE0 _RES_TYPE1
#else
#  define _TYPE_LEN0 _TYPE_LEN2
#  define _ELEM_TYPE0 _ELEM_TYPE2
#  define _ARG_TYPE0 _ARG_TYPE2
#  define _RES_TYPE0 _RES_TYPE2
#endif
