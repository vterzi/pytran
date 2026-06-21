#define _PROC _CAT2(character,_LABEL)
pure function _PROC(arg, width) result(res)
    _TYPE(len=*, kind=_KIND), intent(in) :: arg
    integer, intent(in), optional :: width
    character(len=:), allocatable :: res

    character(len=MAX_LEN_INTEGER_STR) :: width_str

    if (present(width)) then
        write(width_str, *) max(0, width)
        allocate(character(len=width) :: res)
        write(res, '(a' // trim(adjustl(width_str)) // ')') arg
    else
        res = arg
    end if
end function _PROC
#undef _PROC
