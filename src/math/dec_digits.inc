! {"id.inc"(_ID)} + () -(_PROC)> ()
! Define functions that determine the number of decimal digits of integers.


#define _PROC _(dec_digits_)_(_LABEL)
elemental function _PROC(num) result(res)
    _ARG_TYPE, intent(in) :: num
    integer :: res

    integer :: n

    res = 1
    if (num /= 0) then
        n = num / 10
        do while (n /= 0)
        n = n / 10
        res = res + 1
        end do
    end if
end function _PROC
#undef _PROC
