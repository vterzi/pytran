#define _PROC _UNARY_OP(character)
pure function _PROC(arg, width) result(res)
    _TYPE(len=*, kind=_KIND), intent(in) :: arg
    integer, intent(in), optional :: width
    character(len=:), allocatable :: res

    integer :: width_

    if (present(width)) then
        width_ = max(0, width)
        allocate(character(len=width_) :: res)
        write(res, "(a" // str(width_) // ")") arg
    else
        res = arg
    end if
end function _PROC
#undef _PROC
