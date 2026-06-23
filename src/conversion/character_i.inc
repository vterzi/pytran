#define _PROC _CAT2(character,_LABEL)
pure function _PROC(arg, width, minimum, sign) result(res)
    _TYPE(_KIND), intent(in) :: arg
    integer, intent(in), optional :: width, minimum
    logical, intent(in), optional :: sign
    character(len=:), allocatable :: res

    logical :: present_width, present_minimum, present_sign, sign_
    integer :: width_
    character(len=:), allocatable :: fmt
    character(len=_CAT3(MAX_LEN_INTEGER,_KIND_LABEL,STR)) :: buf

    present_width = present(width)
    present_minimum = present(minimum)
    present_sign = present(sign)
    if (present_width .or. present_minimum .or. present_sign) then
        fmt = "("
        if (present_sign) then
            sign_ = sign
        else
            sign_ = .false.
        end if
        if (sign_) fmt = fmt // "sp,"
        if (present_width) then
            width_ = max(0, width)
        else
            width_ = num_digits(arg)
            if (arg < 0 .or. sign_) width_ = width_ + 1
        end if
        fmt = fmt // "i" // str(width_)
        if (present_minimum) fmt = ( &
            fmt // "." // str(min(max(minimum, 0), width_)) &
        )
        fmt = fmt // ")"
        allocate(character(len=width_) :: res)
        write(res, fmt) arg
    else
        write(buf, *) arg
        res = trim(adjustl(buf))
    end if
end function _PROC
#undef _PROC
