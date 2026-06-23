#define _PROC _CAT2(inf,_LABEL)
elemental function _PROC(x) result(res)
    _TYPE(_KIND), intent(in) :: x
    _TYPE(_KIND) :: res

    res = _CAT3(_TYPE,_KIND_LABEL,INF)
end function _PROC
#undef _PROC


#define _PROC _CAT2(nan,_LABEL)
elemental function _PROC(x) result(res)
    _TYPE(_KIND), intent(in) :: x
    _TYPE(_KIND) :: res

    res = _CAT3(_TYPE,_KIND_LABEL,NAN)
end function _PROC
#undef _PROC
