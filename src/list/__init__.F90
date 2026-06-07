#include "../inc/init.inc"

#define _(X) X

#define _CAT(X,Y) _(X)_(_)_(Y)
#define _ITEM _(Item)_(_TYPE_LABEL)_(_KIND_LABEL)
#define _LIST _(List)_(_TYPE_LABEL)_(_KIND_LABEL)

module stdlib_list
    implicit none

    private
    public :: len, size

    type, public :: slice
        integer :: start = 1, stop = 0, step = 1
    end type slice

    interface slice
        module procedure :: new_slice_one
        module procedure :: new_slice_two
    end interface slice

#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX | _CHARACTER)
#define _FILE "../list/types.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

contains
    elemental function new_slice_one(stop) result(self)
        integer, intent(in) :: stop
        type(slice) :: self

        self%stop = stop
    end function new_slice_one


    elemental function new_slice_two(start, stop) result(self)
        integer, intent(in) :: start, stop
        type(slice) :: self

        self%start = start
        self%stop = stop
    end function new_slice_two


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

end module stdlib_list
