#ifdef _ARG_ARRAY
#  define _PROC_NAME _SUBPROC(_LIST,Assign,Array)
#  define _ARG_IN_TYPE _IN_TYPE_ARR
#  define _ARG_DIM size(arg, 1)
#  define _ARG_SIZE _ARR_SIZE(arg)
#  define _ARG_ELEM(X) arg(_ARR_IDX(X))
#else
#  define _PROC_NAME _SUBPROC(_LIST,Assign,List)
#  define _ARG_IN_TYPE class(_LIST)
#  define _ARG_DIM arg%Dimension()
#  define _ARG_SIZE arg%Size()
#  define _ARG_ELEM(X) iter%Next()
#endif


subroutine _PROC_NAME(this, arg)
    class(_LIST), intent(inout) :: this
    _ARG_IN_TYPE, intent(in) :: arg

    integer :: i, li, size_
    type(_NODE), pointer :: node
#ifndef _ARG_ARRAY
    type(_ITER) :: iter

    iter = _ITER(arg)
#endif

#if defined(_LOGICAL_ARRAY) | defined(_NUMBER_ARRAY)
    this%dimension_ = _ARG_DIM
#endif
    size_ = _ARG_SIZE
    li = min(this%size_, size_)
    node => this%first
    do i = 1, li
        node%val = _ARG_ELEM(i)
        node => node%next
    end do
    if (this%size_ < size_) then
        call this%Append(_ARG_ELEM(li + 1))
        node => this%last
        do i = li + 2, size_
            allocate(node%next)
            node => node%next
            node%val = _ARG_ELEM(i)
        end do
        this%last => node
    else if (li < this%size_) then
        call this%Delete(li + 1, this%size_)
    end if
    this%size_ = size_
end subroutine _PROC_NAME


#undef _PROC_NAME
#undef _ARG_IN_TYPE
#undef _ARG_DIM
#undef _ARG_SIZE
#undef _ARG_ELEM

#undef _ARG_ARRAY
