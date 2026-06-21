#define _OP dec_digits
elemental function _PROC(arg) result(res)
    _TYPE(_KIND), intent(in) :: arg
    integer :: res

    integer :: n

    n = abs(arg)
    if (n < _(10_)_(_KIND)) then
        res = 1
    else if (n < _(100_)_(_KIND)) then
        res = 2
#if _KIND == _K1
    else
        res = 3
#else
    else if (n < _(1000_)_(_KIND)) then
        res = 3
    else if (n < _(10000_)_(_KIND)) then
        res = 4
#  if _KIND == _K2
    else
        res = 5
#  else
    else if (n < _(100000_)_(_KIND)) then
        res = 5
    else if (n < _(1000000_)_(_KIND)) then
        res = 6
    else if (n < _(10000000_)_(_KIND)) then
        res = 7
    else if (n < _(100000000_)_(_KIND)) then
        res = 8
    else if (n < _(1000000000_)_(_KIND)) then
        res = 9
#    if _KIND == _K4
    else
        res = 10
#    else
    else if (n < _(10000000000_)_(_KIND)) then
        res = 10
    else if (n < _(100000000000_)_(_KIND)) then
        res = 11
    else if (n < _(1000000000000_)_(_KIND)) then
        res = 12
    else if (n < _(10000000000000_)_(_KIND)) then
        res = 13
    else if (n < _(100000000000000_)_(_KIND)) then
        res = 14
    else if (n < _(1000000000000000_)_(_KIND)) then
        res = 15
    else if (n < _(10000000000000000_)_(_KIND)) then
        res = 16
    else if (n < _(100000000000000000_)_(_KIND)) then
        res = 17
    else if (n < _(1000000000000000000_)_(_KIND)) then
        res = 18
#      if _KIND == _K8
    else
        res = 19
#      else
    else if (n < _(10000000000000000000_)_(_KIND)) then
        res = 19
    else if (n < _(100000000000000000000_)_(_KIND)) then
        res = 20
    else if (n < _(1000000000000000000000_)_(_KIND)) then
        res = 21
    else if (n < _(10000000000000000000000_)_(_KIND)) then
        res = 22
    else if (n < _(100000000000000000000000_)_(_KIND)) then
        res = 23
    else if (n < _(1000000000000000000000000_)_(_KIND)) then
        res = 24
    else if (n < _(10000000000000000000000000_)_(_KIND)) then
        res = 25
    else if (n < _(100000000000000000000000000_)_(_KIND)) then
        res = 26
    else if (n < _(1000000000000000000000000000_)_(_KIND)) then
        res = 27
    else if (n < _(10000000000000000000000000000_)_(_KIND)) then
        res = 28
    else if (n < _(100000000000000000000000000000_)_(_KIND)) then
        res = 29
    else if (n < _(1000000000000000000000000000000_)_(_KIND)) then
        res = 30
    else if (n < _(10000000000000000000000000000000_)_(_KIND)) then
        res = 31
    else if (n < _(100000000000000000000000000000000_)_(_KIND)) then
        res = 32
    else if (n < _(1000000000000000000000000000000000_)_(_KIND)) then
        res = 33
    else if (n < _(10000000000000000000000000000000000_)_(_KIND)) then
        res = 34
    else if (n < _(100000000000000000000000000000000000_)_(_KIND)) then
        res = 35
    else if (n < _(1000000000000000000000000000000000000_)_(_KIND)) then
        res = 36
    else if (n < _(10000000000000000000000000000000000000_)_(_KIND)) then
        res = 37
    else if (n < _(100000000000000000000000000000000000000_)_(_KIND)) then
        res = 38
    else
        res = 39
#      endif
#    endif
#  endif
#endif
    end if
end function _PROC
#undef _OP
