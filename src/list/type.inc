#define _METHOD(T,P) procedure :: P => _PROC(T,P)
#define _PRIVATE_METHOD(T,P) procedure, private :: P => _PROC(T,P)
#ifdef _ARRAY
#  define _ARRAY_LABEL Array
#  define _NODE_TYPE _ELEM_TYPE, dimension(:), allocatable
#else
#  define _ARRAY_LABEL
#  define _NODE_TYPE _ELEM_TYPE
#endif


type _NODE
    private
    _NODE_TYPE :: val
    type(_NODE), pointer :: next => null()
end type _NODE

type, public :: _ITER
    private
#ifdef _ARRAY
    integer :: dimension_
#endif
    type(_NODE), pointer :: node => null()
contains
    _METHOD(_ITER,Get)
    _METHOD(_ITER,Set)
    _METHOD(_ITER,Step)
    _METHOD(_ITER,Next)
end type _ITER

interface _ITER
    module procedure :: _CONSTRUCTOR(_ITER)
end interface _ITER

type, public :: _LIST
    private
#ifdef _ARRAY
    integer :: dimension_ = 0
#endif
    integer :: size_ = 0
    type(_NODE), pointer :: first => null(), last => null()
contains
#ifdef _ARRAY
    _METHOD(_LIST,Dimension)
#elif _TYPE == _CHARACTER
    _METHOD(_LIST,Len)
#endif
    _METHOD(_LIST,Size)
    _PRIVATE_METHOD(_LIST,Index)
    _PRIVATE_METHOD(_LIST,Node)
    _METHOD(_LIST,Get)
    _METHOD(_LIST,Set)
#define _GENERIC_NAME Append
#define _PREFIX _PROC(_LIST,_GENERIC_NAME)
#define _ARG1 Element
#define _ARG2 Array
#define _ARG3 List
#include "../list/type_generic.inc"
#define _GENERIC_NAME Insert
#define _PREFIX _PROC(_LIST,_GENERIC_NAME)
#define _ARG1 Element
#define _ARG2 Array
#define _ARG3 List
#include "../list/type_generic.inc"
#define _GENERIC_NAME Delete
#define _PREFIX _PROC(_LIST,_GENERIC_NAME)
#define _ARG1 Element
#define _ARG2 Slice
#include "../list/type_generic.inc"
#if defined(_NUMBER)
#  ifndef _ARRAY
    _METHOD(_LIST,Add)
    _METHOD(_LIST,Multiply)
#  else
#    define _GENERIC_NAME Add
#    define _PREFIX _PROC(_LIST,_GENERIC_NAME)
#    define _ARG1 Scalar
#    define _ARG2 Array
#    include "../list/type_generic.inc"
#    define _GENERIC_NAME Multiply
#    define _PREFIX _PROC(_LIST,_GENERIC_NAME)
#    define _ARG1 Scalar
#    define _ARG2 Array
#    include "../list/type_generic.inc"
#  endif
#elif _TYPE == _CHARACTER
    _METHOD(_LIST,Concatenate)
#endif
    _METHOD(_LIST,Find)
    _METHOD(_LIST,Contains)
    _METHOD(_LIST,Extend)
    _METHOD(_LIST,ToArray)
    _METHOD(_LIST,Empty)
#define _PREFIX _PROC(_LIST,Assign)
#define _GENERIC_NAME assignment(=)
#define _ARG1 Array
#define _ARG2 List
#include "../list/type_generic.inc"
    final :: _PROC(_LIST,Final)
end type _LIST

#ifdef _ARRAY
interface _LIST
    module procedure :: _CONSTRUCTOR(_LIST)
end interface _LIST
#endif


#undef _METHOD
#undef _PRIVATE_METHOD
#undef _GENERIC2_PROCS
#undef _GENERIC_2
#undef _GENERIC2
#undef _GENERIC3_PROCS
#undef _GENERIC_3
#undef _GENERIC3
#undef _ASSIGNMENT2
#undef _ARRAY_LABEL
#undef _NODE_TYPE
