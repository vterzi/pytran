#define _PROC _CAT2(num_digits,_LABEL)
elemental function _PROC(arg) result(res)
    _TYPE(_KIND), intent(in) :: arg
    integer :: res

    integer :: n

    n = abs(arg)
    if (n < _CAT2(10,_KIND)) then
        res = 1
    else if (n < _CAT2(100,_KIND)) then
        res = 2
#if _KIND == _K1
    else
        res = 3
#else
    else if (n < _CAT2(1000,_KIND)) then
        res = 3
    else if (n < _CAT2(10000,_KIND)) then
        res = 4
#  if _KIND == _K2
    else
        res = 5
#  else
    else if (n < _CAT2(100000,_KIND)) then
        res = 5
    else if (n < _CAT2(1000000,_KIND)) then
        res = 6
    else if (n < _CAT2(10000000,_KIND)) then
        res = 7
    else if (n < _CAT2(100000000,_KIND)) then
        res = 8
    else if (n < _CAT2(1000000000,_KIND)) then
        res = 9
#    if _KIND == _K4
    else
        res = 10
#    else
    else if (n < _CAT2(10000000000,_KIND)) then
        res = 10
    else if (n < _CAT2(100000000000,_KIND)) then
        res = 11
    else if (n < _CAT2(1000000000000,_KIND)) then
        res = 12
    else if (n < _CAT2(10000000000000,_KIND)) then
        res = 13
    else if (n < _CAT2(100000000000000,_KIND)) then
        res = 14
    else if (n < _CAT2(1000000000000000,_KIND)) then
        res = 15
    else if (n < _CAT2(10000000000000000,_KIND)) then
        res = 16
    else if (n < _CAT2(100000000000000000,_KIND)) then
        res = 17
    else if (n < _CAT2(1000000000000000000,_KIND)) then
        res = 18
#      if _KIND == _K8
    else
        res = 19
#      else
    else if (n < _CAT2(10000000000000000000,_KIND)) then
        res = 19
    else if (n < _CAT2(100000000000000000000,_KIND)) then
        res = 20
    else if (n < _CAT2(1000000000000000000000,_KIND)) then
        res = 21
    else if (n < _CAT2(10000000000000000000000,_KIND)) then
        res = 22
    else if (n < _CAT2(100000000000000000000000,_KIND)) then
        res = 23
    else if (n < _CAT2(1000000000000000000000000,_KIND)) then
        res = 24
    else if (n < _CAT2(10000000000000000000000000,_KIND)) then
        res = 25
    else if (n < _CAT2(100000000000000000000000000,_KIND)) then
        res = 26
    else if (n < _CAT2(1000000000000000000000000000,_KIND)) then
        res = 27
    else if (n < _CAT2(10000000000000000000000000000,_KIND)) then
        res = 28
    else if (n < _CAT2(100000000000000000000000000000,_KIND)) then
        res = 29
    else if (n < _CAT2(1000000000000000000000000000000,_KIND)) then
        res = 30
    else if (n < _CAT2(10000000000000000000000000000000,_KIND)) then
        res = 31
    else if (n < _CAT2(100000000000000000000000000000000,_KIND)) then
        res = 32
    else if (n < _CAT2(1000000000000000000000000000000000,_KIND)) then
        res = 33
    else if (n < _CAT2(10000000000000000000000000000000000,_KIND)) then
        res = 34
    else if (n < _CAT2(100000000000000000000000000000000000,_KIND)) then
        res = 35
    else if (n < _CAT2(1000000000000000000000000000000000000,_KIND)) then
        res = 36
    else if (n < _CAT2(10000000000000000000000000000000000000,_KIND)) then
        res = 37
    else if (n < _CAT2(100000000000000000000000000000000000000,_KIND)) then
        res = 38
    else
        res = 39
#      endif
#    endif
#  endif
#endif
    end if
end function _PROC
#undef _PROC
