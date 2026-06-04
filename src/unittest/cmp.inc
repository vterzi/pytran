#define _PROC _(assert_equal_)_(_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (_EQ(first, second))) then
        if (present(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '/=', second
        if (present(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _(assert_not_equal_)_(_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (_NE(first, second))) then
        if (present(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '==', second
        if (present(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#undef _PROC


#if _TYPE == _INTEGER | _TYPE == _REAL | _TYPE == _CHARACTER
#  define _PROC _(assert_greater_)_(_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (first > second)) then
        if (present(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '<=', second
        if (present(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#  undef _PROC


#  define _PROC _(assert_greater_equal_)_(_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (first >= second)) then
        if (present(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '<', second
        if (present(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#  undef _PROC


#  define _PROC _(assert_less_)_(_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (first < second)) then
        if (present(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '>=', second
        if (present(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#  undef _PROC


#  define _PROC _(assert_less_equal_)_(_LABEL)
subroutine _PROC(first, second, msg, success)
    _ARG_TYPE, intent(in) :: first, second
    character(len=*), intent(in), optional :: msg
    logical, intent(inout), optional :: success

    if (.not. (first <= second)) then
        if (present(msg)) then
            write(*, *) msg
        end if
        write(*, *) first, '>', second
        if (present(success)) then
            success = success .and. .false.
        else
            stop 1
        end if
    end if
end subroutine _PROC
#  undef _PROC
#endif


#undef _EQ
#undef _NE
