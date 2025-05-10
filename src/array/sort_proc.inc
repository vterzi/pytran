! {"id.inc"} + (_SUBPROC, _BACK) -(_OP1, _OP2)> ()
! Define subprocedures for sorting procedures.


#ifndef _BACK
#  define _OP1 >
#  define _OP2 <
#else
#  define _OP1 <
#  define _OP2 >
#endif


pure recursive subroutine _SUBPROC(array)
    _ARG_TYPE, intent(inout) :: array(:)

    integer :: length, low, high
    _TYPE_LEN(len(array)) :: pivot

    length = size(array)
    if (length <= SIMPLE_SORT_SIZE) then  ! interchange sort
        do low = 1, length - 1
            do high = low + 1, length
                if (array(low) _OP1 array(high)) call swap(array(low), array(high))
            end do
        end do
    else  ! quick sort (Hoare partition scheme)
        pivot = array(length / 2)
        low = 0
        high = length + 1
        do
            do
                low = low + 1
                if (array(low) _OP1= pivot) exit
            end do
            do
                high = high - 1
                if (array(high) _OP2= pivot) exit
            end do
            if (low < high) then
                call swap(array(low), array(high))
            else if (low == high) then
                low = low + 1
                exit
            else
                exit
            end if
        end do
        if (1 < low) call _SUBPROC(array(:low))
        if (high < length) call _SUBPROC(array(high:))
    end if
end subroutine _SUBPROC


#undef _OP1
#undef _OP2

#undef _SUBPROC
#undef _BACK
