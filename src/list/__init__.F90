#include "../inc/init.inc"

#define _(X) X

#define _CAT(X,Y) _(X)_(_)_(Y)
#define _ITEM _(Item)_(_TYPE_LABEL)_(_KIND_LABEL)
#define _LIST _(List)_(_TYPE_LABEL)_(_KIND_LABEL)

module pytran_list
    use pytran_slice, only: slice

    implicit none

    private
    public :: assignment(=), all, any, len, size

#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX | _CHARACTER)
#define _FILE "../list/types.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

contains
    elemental function wraparound_index(index, size) result(wrapped_index)
        integer, intent(in) :: index, size
        integer :: wrapped_index

        if (index > 0 .and. index <= size) then
            wrapped_index = index
        else if (index < 0 .and. index >= -size) then
            wrapped_index = index + size + 1
        else
            wrapped_index = 0
            error stop "list index out of range"  ! IndexError
        end if
    end function wraparound_index


#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX | _CHARACTER)
#define _FILE "../list/procs.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

end module pytran_list
