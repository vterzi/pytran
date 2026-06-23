#define _PROC _CAT2(dec_exponent,_LABEL)
elemental function _PROC(arg) result(res)
    _TYPE(_KIND), intent(in) :: arg
    integer :: res

    integer, parameter :: &
        decimals = _CAT3(REAL,_KIND_LABEL,DEC_DIGITS), &
        expwidth = _CAT3(REAL,_KIND_LABEL,EXP_WIDTH), &
        width = decimals + expwidth + 5

    integer :: idx
    _TYPE(_KIND) :: num
    character(len=width) :: string

    if (.not. ieee_is_finite(arg)) then
        res = huge(res)
    else if (num == 0) then
        res = 0
    else if (.not. ieee_is_normal(arg)) then
        write( &
            string, &
            ( &
                cat("(es", width) &
                // cat(".", decimals) &
                // cat("e", expwidth) &
                // ")" &
            ) &
        ) arg
        idx = width - expwidth + 1
        read(string(idx:), *) res
    else
        num = abs(arg)
        res = floor((exponent(num) - 1) * log10(real(2, _KIND)))
        idx = res + 1
        if (num >= _CAT3(_TYPE,_KIND_LABEL,DEC_POWERS)(idx)) res = idx
    end if
end function _PROC
#undef _PROC
