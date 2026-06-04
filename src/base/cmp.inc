! {"id.inc"(_ID1, _ID2)} + () -(_PROC)> ()
! Define equality and inequality operators for kinds of the logical type.  `_ID1` and `_ID2` must be `_LOGICAL`.


#define _PROC _(_LABEL1)_(_eq_)_(_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _ELEM_TYPE0 :: res

    res = _EQ0(arg1, arg2)
end function _PROC
#undef _PROC


#define _PROC _(_LABEL1)_(_ne_)_(_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _ELEM_TYPE0 :: res

    res = _NE0(arg1, arg2)
end function _PROC
#undef _PROC
