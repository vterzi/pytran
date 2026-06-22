#define _PROC _CAT2(isfinite,_LABEL)
elemental function _PROC(x) result(res)
    _TYPE(_KIND), intent(in) :: x
    logical :: res

#if _TYPE_ID == _REAL
    res = ieee_is_finite(x)
#else
    res = .true.
#endif
end function _PROC
#undef _PROC


#define _PROC _CAT2(isnan,_LABEL)
elemental function _PROC(x) result(res)
    _TYPE(_KIND), intent(in) :: x
    logical :: res

#if _TYPE_ID == _REAL
    res = ieee_is_nan(x)
#else
    res = .false.
#endif
end function _PROC
#undef _PROC


#define _PROC _CAT2(isinf,_LABEL)
elemental function _PROC(x) result(res)
    _TYPE(_KIND), intent(in) :: x
    logical :: res

#if _TYPE_ID == _REAL
    res = .not. ieee_is_nan(x) .and. .not. ieee_is_finite(x)
#else
    res = .false.
#endif
end function _PROC
#undef _PROC
