! {"id.inc"(_ID)} + () -(_PROC)> ()
! Define assignment subroutines for assignment of default values for optional arguments.


#define _PROC _(assign_optional_)_(_LABEL)
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
