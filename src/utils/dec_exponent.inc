#define _OP dec_exponent
elemental function _PROC(arg) result(res)
    _TYPE(_KIND), intent(in) :: arg
    integer :: res

    if (ieee_is_nan(arg)) then
        res = -huge(res) - 1
    else if (.not. ieee_is_finite(arg)) then
        res = huge(res)
    else if (arg == 0) then
        res = -huge(res)
    else
        res = floor(log10(abs(arg)))
    end if
end function _PROC
#undef _OP
