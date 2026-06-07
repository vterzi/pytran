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


#define _PROC _CAT(_LIST,getitem_index)
function _PROC(self, i) result(value)
    class(_LIST), intent(in) :: self
    integer, intent(in) :: i
    _OUT_TYPE :: value

    integer :: idx
    type(_ITEM), pointer :: node

    idx = wraparound_index(i, self%size)
    node => self%first
    do while (idx > 1)
        node => node%next
        idx = idx - 1
    end do
    value = node%val
end function _PROC
#undef _PROC


#define _PROC _CAT(_LIST,getitem_slice)
function _PROC(self, s) result(list)
    class(_LIST), intent(in) :: self
    type(slice), intent(in) :: s
    type(_LIST) :: list

    integer :: n, lo, up, st, idx, i
    type(_ITEM), pointer :: node, prev, next

    n = self%size
    lo = wraparound_index(s%start, n)
    up = wraparound_index(s%stop, n)
    st = s%step
    if (st == 0) then
        error stop "slice step cannot be zero"  ! ValueError
    else if (st > 0 .and. lo <= up) then
        node => self%first
        idx = 1
        do while (idx < lo)
            node => node%next
            idx = idx + 1
        end do
        allocate(next)
        next%val = node%val
        list%first => next
        idx = idx + st
        do while (idx <= up)
            do i = 1, st
                node => node%next
            end do
            prev => next
            allocate(next)
            next%val = node%val
            prev%next => next
            idx = idx + st
        end do
        list%last => next
    else if (st < 0 .and. lo >= up) then
        st = -st
        idx = lo
        lo = lo - ((lo - up) / st) * st
        up = idx
        node => self%first
        idx = 1
        do while (idx < lo)
            node => node%next
            idx = idx + 1
        end do
        allocate(prev)
        prev%val = node%val
        list%last => prev
        idx = idx + st
        do while (idx <= up)
            do i = 1, st
                node => node%next
            end do
            next => prev
            allocate(prev)
            prev%val = node%val
            prev%next => next
            idx = idx + st
        end do
        list%first => prev
    end if
end function _PROC
#undef _PROC


#define _PROC _CAT(_LIST,append)
elemental subroutine _PROC(self, object)
    class(_LIST), intent(inout) :: self
    _IN_TYPE, intent(in) :: object

    type(_ITEM), pointer :: appended

    allocate(appended)
    appended%val = object
    if (associated(self%last)) then
        self%last%next => appended
    else
        self%first => appended
    end if
    self%last => appended
    self%size = self%size + 1
end subroutine _PROC
#undef _PROC


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


#define _PROC _CAT(_LIST,copy)
subroutine _PROC(self, other)
    class(_LIST), intent(in) :: self
    type(_LIST), intent(out) :: other

    type(_ITEM), pointer :: node, copied

    node => self%first
    if (associated(node)) then
        allocate(copied)
        copied%val = node%val
        other%first => copied
        node => node%next
        do while (associated(node))
            allocate(copied%next)
            copied => copied%next
            copied%val = node%val
            node => node%next
        end do
        other%last => copied
        other%size = self%size
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(_LIST,count)
function _PROC(self, value) result(count)
    class(_LIST), intent(in) :: self
    _IN_TYPE, intent(in) :: value
    integer :: count

    type(_ITEM), pointer :: node

    count = 0
    node => self%first
    do while (associated(node))
        if (node%val _EQ value) count = count + 1
        node => node%next
    end do
end function _PROC
#undef _PROC


#define _PROC _CAT(_LIST,extend_list)
subroutine _PROC(self, iterable)
    class(_LIST), intent(inout) :: self
    class(_LIST), intent(in) :: iterable

    type(_ITEM), pointer :: node, appended

    node => iterable%first
    if (associated(node)) then
        allocate(appended)
        appended%val = node%val
        if (associated(self%last)) then
            self%last%next => appended
        else
            self%first => appended
        end if
        node => node%next
        do while (associated(node))
            allocate(appended%next)
            appended => appended%next
            appended%val = node%val
            node => node%next
        end do
        self%last => appended
        self%size = self%size + iterable%size
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(_LIST,extend_array)
pure subroutine _PROC(self, iterable)
    class(_LIST), intent(inout) :: self
    _IN_TYPE, dimension(:), intent(in) :: iterable

    integer :: n, i
    type(_ITEM), pointer :: appended

    n = size(iterable)
    if (n > 0) then
        allocate(appended)
        appended%val = iterable(1)
        if (associated(self%last)) then
            self%last%next => appended
        else
            self%first => appended
        end if
        do i = 2, n
            allocate(appended%next)
            appended => appended%next
            appended%val = iterable(i)
        end do
        self%last => appended
        self%size = self%size + n
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(_LIST,index)
function _PROC(self, value, start, stop) result(index)
    class(_LIST), intent(in) :: self
    _IN_TYPE, intent(in) :: value
    integer, intent(in), optional :: start, stop
    integer :: index

    integer :: lo, up, i
    type(_ITEM), pointer :: node

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
    node => self%first
    i = 1
    do while (associated(node))
        if (i > up) then
            exit
        else if (i >= lo .and. node%val _EQ value) then
            index = i
            return
        end if
        node => node%next
        i = i + 1
    end do
end function _PROC
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
