! {"id.inc"(_ID)} + () -(_PROC)> ()
! Define conversion functions from the character type to other basic data
! types.


#define _PROC _(to_)_(_TYPE_NAME)_(_KIND_LABEL)
elemental function _PROC(arg) result(res)
    character(len=*), intent(in) :: arg
    _ELEM_TYPE :: res

    read(arg, *) res
end function _PROC
#undef _PROC
