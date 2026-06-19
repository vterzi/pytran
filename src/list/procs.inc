#if _TYPE_ID == _CHARACTER
#  define _TYPE_LEN(X) _TYPE(len=X, kind=_KIND)
#  define _IN_TYPE _TYPE_LEN(*)
#  define _OUT_TYPE _TYPE_LEN(:), allocatable
#else
#  define _TYPE_LEN(X) _TYPE(_KIND)
#  define _IN_TYPE _TYPE_LEN()
#  define _OUT_TYPE _TYPE_LEN()
#endif
#if _TYPE_ID == _LOGICAL
#  define _EQ .eqv.
#else
#  define _EQ ==
#endif


#define _PROC _CAT(_LIST,getitem_index)
function _PROC(self, i) result(val)
    class(_LIST), intent(in) :: self
    integer, intent(in) :: i
    _OUT_TYPE :: val

    integer :: idx
    type(_ITEM), pointer :: node

    idx = wraparound_index(i, self%size)
    node => self%first
    do while (idx > 1)
        node => node%next
        idx = idx - 1
    end do
    val = node%val
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

    type(_ITEM), pointer :: node, new

    node => self%first
    if (associated(node)) then
        allocate(new)
        new%val = node%val
        other%first => new
        node => node%next
        do while (associated(node))
            allocate(new%next)
            new => new%next
            new%val = node%val
            node => node%next
        end do
        other%last => new
        other%size = self%size
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(_LIST,count)
function _PROC(self, val) result(count)
    class(_LIST), intent(in) :: self
    _IN_TYPE, intent(in) :: val
    integer :: count

    type(_ITEM), pointer :: node

    count = 0
    node => self%first
    do while (associated(node))
        if (node%val _EQ val) count = count + 1
        node => node%next
    end do
end function _PROC
#undef _PROC


#define _PROC _CAT(_LIST,extend_list)
subroutine _PROC(self, iterable)
    class(_LIST), intent(inout) :: self
    class(_LIST), intent(in) :: iterable

    type(_ITEM), pointer :: node, new

    node => iterable%first
    if (associated(node)) then
        allocate(new)
        new%val = node%val
        if (associated(self%last)) then
            self%last%next => new
        else
            self%first => new
        end if
        node => node%next
        do while (associated(node))
            allocate(new%next)
            new => new%next
            new%val = node%val
            node => node%next
        end do
        self%last => new
        self%size = self%size + iterable%size
    end if
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(_LIST,extend_array)
pure subroutine _PROC(self, iterable)
    class(_LIST), intent(inout) :: self
    _IN_TYPE, dimension(:), intent(in) :: iterable

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


#define _PROC _CAT(_LIST,index)
function _PROC(self, val, start, stop) result(index)
    class(_LIST), intent(in) :: self
    _IN_TYPE, intent(in) :: val
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
        else if (i >= lo .and. node%val _EQ val) then
            index = i
            return
        end if
        node => node%next
        i = i + 1
    end do
end function _PROC
#undef _PROC


#define _PROC _CAT(_LIST,assign_list)
subroutine _PROC(self, other)
    class(_LIST), intent(out) :: self
    class(_LIST), intent(in) :: other

    call other%copy(self)
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(_LIST,assign_array)
subroutine _PROC(self, other)
    class(_LIST), intent(out) :: self
    _IN_TYPE, dimension(:), intent(in) :: other

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


#define _PROC _CAT(_LIST,final)
elemental subroutine _PROC(self)
    type(_LIST), intent(inout) :: self

    call self%clear()
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(array_assign,_LIST)
subroutine _PROC(self, other)
    _TYPE_LEN(:), dimension(:), allocatable, intent(out) :: self
    class(_LIST), intent(in) :: other

    integer :: n, i
    type(_ITEM), pointer :: node

    n = size(other)
#if _TYPE_ID == _CHARACTER
    allocate(character(len=len(other)) :: self(n))
#else
    allocate(self(n))
#endif
    node => other%first
    do i = 1, n
        self(i) = node%val
        node => node%next
    end do
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(size,_LIST)
elemental function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    integer :: res

    res = self%size
end function _PROC
#undef _PROC


#if _TYPE_ID == _LOGICAL
#  define _PROC _CAT(all,_LIST)
function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    logical :: res

    type(_ITEM), pointer :: node

    res = .true.
    node => self%first
    do while (associated(node))
        if (.not. node%val) then
            res = .false.
            return
        end if
        node => node%next
    end do
end function _PROC
#  undef _PROC


#  define _PROC _CAT(any,_LIST)
function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    logical :: res

    type(_ITEM), pointer :: node

    res = .false.
    node => self%first
    do while (associated(node))
        if (node%val) then
            res = .true.
            return
        end if
        node => node%next
    end do
end function _PROC
#  undef _PROC

#elif _TYPE_ID == _CHARACTER

#  define _PROC _CAT(len,_LIST)
function _PROC(self) result(res)
    class(_LIST), intent(in) :: self
    integer :: res

    integer :: length
    type(_ITEM), pointer :: node

    res = 0
    node => self%first
    do while (associated(node))
        length = len(node%val)
        if (res < length) res = length
        node => node%next
    end do
end function _PROC
#  undef _PROC
#endif


#undef _TYPE_LEN
#undef _IN_TYPE
#undef _OUT_TYPE
#undef _EQ
