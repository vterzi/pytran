#include "../inc/var.inc"


#define _PROC _CAT2(assign_optional,_LABEL)
elemental subroutine _PROC(var, val, opt)
    _ARG_TYPE, intent(out) :: var
    _ARG_TYPE, intent(in) :: val
    _ARG_TYPE, intent(in), optional :: opt

    if (present(opt)) then
        var = opt
    else
        var = val
    end if
end subroutine _PROC
#undef _PROC
