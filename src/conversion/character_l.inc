#define _PROC _UNARY_OP(character)
pure function _PROC(arg, width) result(res)
    _TYPE(_KIND), intent(in) :: arg
    integer, intent(in), optional :: width
    character(len=:), allocatable :: res

    integer :: width_
    character(len=MAX_LEN_LOGICAL_STR) :: buf

    if (present(width)) then
        width_ = max(0, width)
        allocate(character(len=width_) :: res)
        write(res, "(l" // str(width_) // ")") arg
    else
        write(buf, *) arg
        res = trim(adjustl(buf))
    end if
end function _PROC
#undef _PROC
