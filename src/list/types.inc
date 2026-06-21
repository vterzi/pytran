#undef _METHOD
#undef _PRIVATE_METHOD
#undef _ELEM_TYPE
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
    _PRIVATE_METHOD(_LIST,getitem_index)
    _PRIVATE_METHOD(_LIST,getitem_slice)
    generic :: getitem => getitem_index, getitem_slice
    _METHOD(_LIST,append)
    _METHOD(_LIST,clear)
    _METHOD(_LIST,copy)
    _METHOD(_LIST,count)
    _PRIVATE_METHOD(_LIST,extend_list)
    _PRIVATE_METHOD(_LIST,extend_array)
    generic :: extend => extend_list, extend_array
    _METHOD(_LIST,index)
    _PRIVATE_METHOD(_LIST,assign_list)
    _PRIVATE_METHOD(_LIST,assign_array)
    generic :: assignment(=) => assign_list, assign_array
    final :: _CAT(_LIST,final)
end type _LIST

interface assignment(=)
    module procedure :: _CAT(array_assign,_LIST)
end interface assignment(=)

interface size
    module procedure :: _CAT(size,_LIST)
end interface size

#if _TYPE_ID == _LOGICAL
interface all
    module procedure :: _CAT(all,_LIST)
end interface all

interface any
    module procedure :: _CAT(any,_LIST)
end interface any

#elif _TYPE_ID == _CHARACTER

interface len
    module procedure :: _CAT(len,_LIST)
end interface len
#endif

type, public :: _ITER
    private
    type(_ITEM), pointer :: item => null()
contains
    _METHOD(_ITER,getitem)
    _METHOD(_ITER,setitem)
    _METHOD(_ITER,next)
end type _ITER

interface assignment(=)
    module procedure :: _(_ITER)_(_assign_)_(_LIST)
    module procedure :: _(_ITER)_(_assign_)_(_ITER)
end interface assignment(=)
