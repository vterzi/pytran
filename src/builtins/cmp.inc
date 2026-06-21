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
