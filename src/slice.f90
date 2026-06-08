module stdlib_slice
    implicit none

    private

    type, public :: slice
        integer :: start = 1, stop = 0, step = 1
    end type slice

    interface slice
        module procedure :: new_slice_one
        module procedure :: new_slice_two
    end interface slice

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
end module stdlib_slice
