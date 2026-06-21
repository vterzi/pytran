#include "../inc/vars.inc"
#include "../inc/cmp.inc"


#define _PROC _CAT2(_LIST,getitem_index)
function _PROC(self, i) result(val)
    class(_LIST), intent(in) :: self
    integer, intent(in) :: i
    _RES_TYPE :: val

    integer :: idx
    type(_ITEM), pointer :: item

    idx = wraparound_index(i, self%size)
    item => self%first
    do while (idx > 1)
        item => item%next
        idx = idx - 1
    end do
    val = item%val
end function _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,getitem_slice)
function _PROC(self, s) result(list)
    class(_LIST), intent(in) :: self
    type(slice), intent(in) :: s
    type(_LIST) :: list

    integer :: n, lo, up, st, idx, i
    type(_ITEM), pointer :: item, prev, next

    n = self%size
    lo = wraparound_index(s%start, n)
    up = wraparound_index(s%stop, n)
    st = s%step
    if (st == 0) then
        error stop "slice step cannot be zero"  ! ValueError
    else if (st > 0 .and. lo <= up) then
        item => self%first
        idx = 1
        do while (idx < lo)
            item => item%next
            idx = idx + 1
        end do
        allocate(next)
        next%val = item%val
        list%first => next
        idx = idx + st
        do while (idx <= up)
            do i = 1, st
                item => item%next
            end do
            prev => next
            allocate(next)
            next%val = item%val
            prev%next => next
            idx = idx + st
        end do
        list%last => next
    else if (st < 0 .and. lo >= up) then
        st = -st
        idx = lo
        lo = lo - ((lo - up) / st) * st
        up = idx
        item => self%first
        idx = 1
        do while (idx < lo)
            item => item%next
            idx = idx + 1
        end do
        allocate(prev)
        prev%val = item%val
        list%last => prev
        idx = idx + st
        do while (idx <= up)
            do i = 1, st
                item => item%next
            end do
            next => prev
            allocate(prev)
            prev%val = item%val
            prev%next => next
            idx = idx + st
        end do
        list%first => prev
    end if
end function _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,append)
elemental subroutine _PROC(self, object)
    class(_LIST), intent(inout) :: self
    _ARG_TYPE, intent(in) :: object

    type(_ITEM), pointer :: new

    allocate(new)
    new%val = object
    if (associated(self%last)) then
        self%last%next => new
    else
        self%first => new
    end if
    self%last => new
    self%size = self%size + 1
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,clear)
elemental subroutine _PROC(self)
    class(_LIST), intent(inout) :: self

    type(_ITEM), pointer :: item, next

    item => self%first
    do while (associated(item))
        next => item%next
        deallocate(item)
        item => next
    end do
    nullify(self%first, self%last)
    self%size = 0
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,copy)
subroutine _PROC(self, other)
    class(_LIST), intent(in) :: self
    type(_LIST), intent(out) :: other

    type(_ITEM), pointer :: item, new

    item => self%first
    if (associated(item)) then
        allocate(new)
        new%val = item%val
        other%first => new
        item => item%next
        do while (associated(item))
            allocate(new%next)
            new => new%next
            new%val = item%val
            item => item%next
        end do
        other%last => new
        other%size = self%size
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,count)
function _PROC(self, val) result(count)
    class(_LIST), intent(in) :: self
    _ARG_TYPE, intent(in) :: val
    integer :: count

    type(_ITEM), pointer :: item

    count = 0
    item => self%first
    do while (associated(item))
        if (item%val _EQ val) count = count + 1
        item => item%next
    end do
end function _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,extend_list)
subroutine _PROC(self, iterable)
    class(_LIST), intent(inout) :: self
    class(_LIST), intent(in) :: iterable

    type(_ITEM), pointer :: item, new

    item => iterable%first
    if (associated(item)) then
        allocate(new)
        new%val = item%val
        if (associated(self%last)) then
            self%last%next => new
        else
            self%first => new
        end if
        item => item%next
        do while (associated(item))
            allocate(new%next)
            new => new%next
            new%val = item%val
            item => item%next
        end do
        self%last => new
        self%size = self%size + iterable%size
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,extend_array)
pure subroutine _PROC(self, iterable)
    class(_LIST), intent(inout) :: self
    _ARG_TYPE, dimension(:), intent(in) :: iterable

    integer :: n, i
    type(_ITEM), pointer :: new

    n = size(iterable)
    if (n > 0) then
        allocate(new)
        new%val = iterable(1)
        if (associated(self%last)) then
            self%last%next => new
        else
            self%first => new
        end if
        do i = 2, n
            allocate(new%next)
            new => new%next
            new%val = iterable(i)
        end do
        self%last => new
        self%size = self%size + n
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,index)
function _PROC(self, val, start, stop) result(index)
    class(_LIST), intent(in) :: self
    _ARG_TYPE, intent(in) :: val
    integer, intent(in), optional :: start, stop
    integer :: index

    integer :: lo, up, i
    type(_ITEM), pointer :: item

    if (present(start)) then
        lo = start
    else
        lo = 1
    end if
    if (present(stop)) then
        up = stop
    else
        up = self%size
    end if
    index = 0
    item => self%first
    i = 1
    do while (associated(item))
        if (i > up) then
            exit
        else if (i >= lo .and. item%val _EQ val) then
            index = i
            return
        end if
        item => item%next
        i = i + 1
    end do
end function _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,assign_list)
subroutine _PROC(self, other)
    class(_LIST), intent(out) :: self
    class(_LIST), intent(in) :: other

    call other%copy(self)
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,assign_array)
subroutine _PROC(self, other)
    class(_LIST), intent(out) :: self
    _ARG_TYPE, dimension(:), intent(in) :: other

    integer :: n, i
    type(_ITEM), pointer :: new

    n = size(other)
    if (n > 0) then
        allocate(new)
        new%val = other(1)
        self%first => new
        do i = 2, n
            allocate(new%next)
            new => new%next
            new%val = other(i)
        end do
        self%last => new
        self%size = self%size
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_LIST,final)
elemental subroutine _PROC(self)
    type(_LIST), intent(inout) :: self

    call self%clear()
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(array_assign,_LIST)
subroutine _PROC(self, other)
    _TYPE_LEN(:), dimension(:), allocatable, intent(out) :: self
    class(_LIST), intent(in) :: other

    integer :: n, i
    type(_ITEM), pointer :: item

    n = size(other)
#if _TYPE_ID == _CHARACTER
    allocate(character(len=len(other)) :: self(n))
#else
    allocate(self(n))
#endif
    item => other%first
    do i = 1, n
        self(i) = item%val
        item => item%next
    end do
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(size,_LIST)
elemental function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    integer :: res

    res = self%size
end function _PROC
#undef _PROC


#if _TYPE_ID == _LOGICAL
#  define _PROC _CAT2(all,_LIST)
function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    logical :: res

    type(_ITEM), pointer :: item

    res = .true.
    item => self%first
    do while (associated(item))
        if (.not. item%val) then
            res = .false.
            return
        end if
        item => item%next
    end do
end function _PROC
#  undef _PROC


#  define _PROC _CAT2(any,_LIST)
function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    logical :: res

    type(_ITEM), pointer :: item

    res = .false.
    item => self%first
    do while (associated(item))
        if (item%val) then
            res = .true.
            return
        end if
        item => item%next
    end do
end function _PROC
#  undef _PROC

#elif _TYPE_ID == _CHARACTER

#  define _PROC _CAT2(len,_LIST)
function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    integer :: res

    integer :: length
    type(_ITEM), pointer :: item

    res = 0
    item => self%first
    do while (associated(item))
        length = len(item%val)
        if (res < length) res = length
        item => item%next
    end do
end function _PROC
#  undef _PROC
#endif


#define _PROC _CAT2(_ITER,getitem)
pure function _PROC(self) result(res)
    class(_ITER), intent(in) :: self
    _RES_TYPE :: res

    if (.not. associated(self%item)) error stop  ! StopIteration
    res = self%item%val
end function _PROC
#undef _PROC


#define _PROC _CAT2(_ITER,setitem)
elemental subroutine _PROC(self, val)
    class(_ITER), intent(inout) :: self
    _ARG_TYPE, intent(in) :: val

    if (.not. associated(self%item)) error stop  ! StopIteration
    self%item%val = val
end subroutine _PROC
#undef _PROC


#define _PROC _CAT2(_ITER,next)
elemental subroutine _PROC(self)
    class(_ITER), intent(inout) :: self

    if (.not. associated(self%item)) error stop  ! StopIteration
    self%item => self%item%next
end subroutine _PROC
#undef _PROC


#define _PROC _CAT3(_ITER,assign,_LIST)
subroutine _PROC(self, other)
    class(_ITER), intent(out) :: self
    class(_LIST), intent(in) :: other

    self%item => other%first
end subroutine _PROC
#undef _PROC


#define _PROC _CAT3(_ITER,assign,_ITER)
subroutine _PROC(self, other)
    class(_ITER), intent(out) :: self
    class(_ITER), intent(in) :: other

    self%item => other%item
end subroutine _PROC
#undef _PROC
