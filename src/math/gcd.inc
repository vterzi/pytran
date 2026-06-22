#define _PROC _CAT2(gcd,_LABEL)
elemental function _PROC(i, j) result(n)
    _TYPE(_KIND), intent(in) :: i, j
    _TYPE(_KIND) :: n

    _TYPE(_KIND) :: d, r

    n = abs(i)
    d = abs(j)
    do while (d /= 0)
        r = mod(n, d)
        n = d
        d = r
    end do
end function _PROC
#undef _PROC
