! {"id.inc"(_ID)} + () -(_PROC)> ()
! Define subroutines for swapping the values of two variables.


#define _PROC _(swap_)_(_LABEL)
elemental subroutine _PROC(arg1, arg2)
    _ARG_TYPE, intent(inout) :: arg1, arg2

    _ELEM_TYPE :: var

    var = arg1
    arg1 = arg2
    arg2 = var
end subroutine _PROC
#undef _PROC
