module pytran_utils
    implicit none

    private
    public :: str

contains
    pure function str(arg) result(res)
        integer, intent(in) :: arg
        character(len=:), allocatable :: res

        character(len=_MAX_LEN_INTEGER_STR) :: buf

        write(buf, '(i0)') arg
        res = trim(buf)
    end function str
end module pytran_utils
