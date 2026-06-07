#if _TYPE_ID == _CHARACTER
#  define _IN_TYPE _TYPE(len=*, kind=_KIND)
#  define _OUT_TYPE _TYPE(len=:, kind=_KIND), allocatable
#else
#  define _IN_TYPE _TYPE(_KIND)
#  define _OUT_TYPE _TYPE(_KIND)
#endif
#if _TYPE_ID == _LOGICAL
#  define _EQ .eqv.
#else
#  define _EQ ==
#endif


#define _PROC _CAT(_LIST,clear)
elemental subroutine _PROC(self)
    class(_LIST), intent(inout) :: self

    type(_ITEM), pointer :: node, next

    node => self%first
    do while (associated(node))
        next => node%next
        deallocate(node)
        node => next
    end do
    nullify(self%first, self%last)
    self%size = 0
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(_LIST,final)
elemental subroutine _PROC(self)
    type(_LIST), intent(inout) :: self

    call self%clear()
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(size,_LIST)
elemental function _PROC(self) result(result)
    class(_LIST), intent(in) :: self
    integer :: result

    result = self%size
end function _PROC
#undef _PROC


#if _TYPE_ID == _CHARACTER
#  define _PROC _CAT(len,_LIST)
function _PROC(self) result(result)
    class(_LIST), intent(in) :: self
    integer :: result

    integer :: length
    type(_ITEM), pointer :: node

    result = 0
    node => self%first
    do while (associated(node))
        length = len(node%val)
        if (result < length) result = length
        node => node%next
    end do
end function _PROC
#  undef _PROC
#endif


#undef _IN_TYPE
#undef _OUT_TYPE
#undef _EQ
