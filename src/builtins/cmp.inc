#include "../inc/vars.inc"


#define _PROC _CAT3(_LABEL1,eq,_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _RES_TYPE0 :: res

    res = arg1 .eqv. arg2
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL1,ne,_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _RES_TYPE0 :: res

    res = arg1 .neqv. arg2
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL1,lt,_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _RES_TYPE0 :: res

    res = (.not. arg1) .and. arg2
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL1,le,_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _RES_TYPE0 :: res

    res = (.not. arg1) .or. arg2
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL1,gt,_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _RES_TYPE0 :: res

    res = arg1 .and. (.not. arg2)
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL1,ge,_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _RES_TYPE0 :: res

    res = arg1 .or. (.not. arg2)
end function _PROC
#undef _PROC
