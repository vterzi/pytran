#define _METHOD(T,P) procedure :: P => _CAT(T,P)
#define _PRIVATE_METHOD(T,P) procedure, private :: P => _CAT(T,P)
#if _TYPE_ID == _CHARACTER
#  define _ELEM_TYPE _TYPE(len=:, kind=_KIND), allocatable
#else
#  define _ELEM_TYPE _TYPE(_KIND)
#endif

type _ITEM
    private
    _ELEM_TYPE :: val
    type(_ITEM), pointer :: next => null()
end type _ITEM

type, public :: _LIST
    private
    integer :: size = 0
    type(_ITEM), pointer :: first => null(), last => null()
contains
    _METHOD(_LIST,clear)
    final :: _CAT(_LIST,final)
end type _LIST

interface size
    module procedure :: _CAT(size,_LIST)
end interface size

#if _TYPE_ID == _CHARACTER
interface len
    module procedure :: _CAT(len,_LIST)
end interface len
#endif

#undef _METHOD
#undef _PRIVATE_METHOD
#undef _ELEM_TYPE
