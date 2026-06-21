#include "../inc/init.inc"
#include "../inc/utils.inc"

#define _ITEM _(Item)_(_LABEL)
#define _LIST _(List)_(_LABEL)
#define _ITER _(Iter)_(_LABEL)

module pytran_list
    use :: pytran_slice, only: slice

    implicit none

    private
    public :: assignment(=), all, any, len, size

#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX | _CHARACTER)
#define _FILE "../list/types.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

contains
    elemental function wraparound_index( &
        index, size, check &
    ) result(wrapped_index)
        integer, intent(in) :: index, size
        logical, intent(in) :: check
        integer :: wrapped_index

        if (index >= 0) then
            wrapped_index = index
        else
            wrapped_index = index + size + 1
        end if
        if (check) then
            if ( &
                index < 1 .or. index > size &
            ) error stop "index out of range"  ! IndexError
        end if
    end function wraparound_index


#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX | _CHARACTER)
#define _FILE "../list/procs.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

end module pytran_list
