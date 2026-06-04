! {"id.inc"(_ID)} + () -(_PROC, _ARR)> ()
! Define functions that determine the decimal exponent of real numbers.

#define _ARR _(REAL_)_(_KIND_LABEL)_(_DEC_POWERS)
#define _PROC _(dec_exponent_)_(_LABEL)
elemental function _PROC(num) result(res)
    _ARG_TYPE, intent(in) :: num
    integer :: res

    _ELEM_TYPE :: n

    ! `res = floor(log10(abs(num)))` is numerically less stable.
    n = abs(num)
    if (n == 0) then
        res = -huge(res)
    else if (n == _(REAL_)_(_KIND_LABEL)_(_INF)) then
        res = huge(res)
    else
        res = 0
        if (n < 1) then
            do while (n < _ARR(res))
                res = res - 1
            end do
        else
            do while (n >= _ARR(res + 1))
                res = res + 1
            end do
        end if
    end if
end function _PROC
#undef _PROC
#undef _ARR
