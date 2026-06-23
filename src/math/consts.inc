#define _PROC _UNARY_OP(inf)
elemental function _PROC(x) result(res)
    _TYPE(_KIND), intent(in) :: x
    _TYPE(_KIND) :: res

    res = _CAT3(_TYPE,_KIND_LABEL,INF)
end function _PROC
#undef _PROC


#define _PROC _UNARY_OP(nan)
elemental function _PROC(x) result(res)
    _TYPE(_KIND), intent(in) :: x
    _TYPE(_KIND) :: res

    res = _CAT3(_TYPE,_KIND_LABEL,NAN)
end function _PROC
#undef _PROC
