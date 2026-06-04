! {"id.inc"(_ID)} + () -(_PROC)> ()
! Define conversion functions from the logical type to the character type.


#define _PROC _(to_character_)_(_LABEL)
pure function _PROC(arg, width) result(res)
    _ARG_TYPE, intent(in) :: arg
    integer, intent(in), optional :: width
    character(len=:), allocatable :: res

    character(len=MAX_LEN_INTEGER_STR) :: width_str
    character(len=MAX_LEN_LOGICAL_STR) :: buffer

    if (present(width)) then
        write(width_str, *) max(0, width)
        allocate(character(len=width) :: res)
        write(res, '(l' // trim(adjustl(width_str)) // ')') arg
    else
        write(buffer, *) arg
        res = trim(adjustl(buffer))
    end if
end function _PROC
#undef _PROC
