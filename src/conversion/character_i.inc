#define _PROC _CAT2(character,_LABEL)
pure function _PROC(arg, width, minimum, sign) result(res)
    _TYPE(_KIND), intent(in) :: arg
    integer, intent(in), optional :: width, minimum
    logical, intent(in), optional :: sign
    character(len=:), allocatable :: res

    logical :: present_width, present_minimum, present_sign, sign_
    integer :: width_
    character(len=MAX_LEN_INTEGER_STR) :: width_str, minimum_str
    character(len=3) :: sign_str
    character(len=_(MAX_LEN_INTEGER_)_(_KIND_LABEL)_(_STR)) :: buffer

    present_width = present(width)
    present_minimum = present(minimum)
    present_sign = present(sign)
    if (present_width .or. present_minimum .or. present_sign) then
        if (present_sign) then
            sign_ = sign
        else
            sign_ = .false.
        end if
        if (sign_) then
            sign_str = 'sp,'
        else
            sign_str = ''
        end if
        if (present_width) then
            width_ = max(0, width)
        else
            width_ = dec_digits(arg)
            if (arg < 0 .or. sign_) width_ = width_ + 1
        end if
        if (present_minimum) then
            write(minimum_str, *) min(max(0, minimum), width_)
            minimum_str = '.' // adjustl(minimum_str)
        else
            minimum_str = ''
        end if
        write(width_str, *) width_
        allocate(character(len=width_) :: res)
        write( &
            res, &
            '(' // trim(sign_str) // 'i' // trim(adjustl(width_str)) &
            // trim(minimum_str) // ')' &
        ) arg
    else
        write(buffer, *) arg
        res = trim(adjustl(buffer))
    end if
end function _PROC
#undef _PROC
