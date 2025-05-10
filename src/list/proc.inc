#ifdef _ARRAY
#  define _ARRAY_LABEL Array
#  define _IN_TYPE _ARG_TYPE, dimension(this%dimension_)
#  define _IN_TYPE_ARR _ARG_TYPE, dimension(:, :)
#  define _OUT_TYPE _IN_TYPE
#  define _OUT_TYPE_ARR _ELEM_TYPE, dimension(this%dimension_, this%size_)
#  define _ARR_IDX(X) :, X
#  define _ARR_SIZE(X) size(X, 2)
#elif _TYPE == _CHARACTER
#  define _ARRAY_LABEL
#  define _IN_TYPE _ARG_TYPE
#  define _IN_TYPE_ARR _IN_TYPE, dimension(:)
#  define _OUT_TYPE _ELEM_TYPE
#  define _OUT_TYPE_ARR _OUT_TYPE, dimension(:)
#  define _ARR_IDX(X) X
#  define _ARR_SIZE(X) size(X)
#else
#  define _ARRAY_LABEL
#  define _IN_TYPE _ARG_TYPE
#  define _IN_TYPE_ARR _IN_TYPE, dimension(:)
#  define _OUT_TYPE _IN_TYPE
#  define _OUT_TYPE_ARR _OUT_TYPE, dimension(this%size_)
#  define _ARR_IDX(X) X
#  define _ARR_SIZE(X) size(X)
#endif

#if _TYPE == _LOGICAL
#  ifndef _ARRAY
#    define _EQUAL(X,Y) X .eqv. Y
#  else
#    define _EQUAL(X,Y) all(X .eqv. Y)
#  endif
#elif defined(_NUMBER) & defined(_ARRAY)
#  define _EQUAL(X,Y) all(X == Y)
#else
#  define _EQUAL(X,Y) X == Y
#endif


function _CONSTRUCTOR(_ITER)(list, idx) result(this)
    type(_LIST), intent(in) :: list
    integer, intent(in), optional :: idx
    type(_ITER) :: this

    integer :: idx_

    idx_ = 1
    if (present(idx)) idx_ = idx

#ifdef _ARRAY
    this%dimension_ = list%Dimension()
#endif
    this%node => list%Node(idx_)
end function _CONSTRUCTOR(_ITER)


pure function _PROC(_ITER,Get)(this) result(val)
    class(_ITER), intent(in) :: this
    _OUT_TYPE :: val

    if (.not. associated(this%node)) error stop 'end of list reached'
    val = this%node%val
end function _PROC(_ITER,Get)


subroutine _PROC(_ITER,Set)(this, val)
    class(_ITER), intent(in) :: this
    _IN_TYPE :: val

    if (.not. associated(this%node)) error stop 'end of list reached'
    this%node%val = val
end subroutine _PROC(_ITER,Set)


subroutine _PROC(_ITER,Step)(this)
    class(_ITER), intent(inout) :: this

    if (.not. associated(this%node)) error stop 'end of list reached'
    this%node => this%node%next
end subroutine _PROC(_ITER,Step)


function _PROC(_ITER,Next)(this) result(val)
    class(_ITER), intent(inout) :: this
    _OUT_TYPE :: val

    if (.not. associated(this%node)) error stop 'end of list reached'
    val = this%node%val
    this%node => this%node%next
end function _PROC(_ITER,Next)


#ifdef _ARRAY
elemental function _CONSTRUCTOR(_LIST)(dimension_) result(this)
    integer, intent(in) :: dimension_
    type(_LIST) :: this

    this%dimension_ = dimension_
end function _CONSTRUCTOR(_LIST)


elemental function _PROC(_LIST,Dimension)(this) result(dmension_)
    class(_LIST), intent(in) :: this
    integer :: dmension_

    dmension_ = this%dimension_
end function _PROC(_LIST,Dimension)
#endif


elemental function _PROC(_LIST,Size)(this) result(size_)
    class(_LIST), intent(in) :: this
    integer :: size_

    size_ = this%size_
end function _PROC(_LIST,Size)


#if _TYPE == _CHARACTER
function _PROC(_LIST,Len)(this) result(maxLen)
    class(_LIST), intent(in) :: this
    integer :: maxLen

    integer :: i, newLen
    type(_NODE), pointer :: node

    maxLen = 0
    node => this%first
    do i = 1, this%size_
        newLen = len(node%val)
        if (maxLen < newLen) maxLen = newLen
        node => node%next
    end do
end function _PROC(_LIST,Len)
#endif


subroutine _SUBPROC(_LIST,Append,Element)(this, val)
    class(_LIST), intent(inout) :: this
    _IN_TYPE, intent(in) :: val

    type(_NODE), pointer :: appended

    allocate(appended)
    appended%val = val
    if (this%size_ > 0) then
        this%last%next => appended
    else
        this%first => appended
    end if
    this%last => appended
    this%size_ = this%size_ + 1
end subroutine _SUBPROC(_LIST,Append,Element)


#define _ARG_ARRAY
#include "../list/proc_append.inc"

#include "../list/proc_append.inc"


elemental function _PROC(_LIST,Index)(this, idx) result(idx_)
    class(_LIST), intent(in) :: this
    integer, intent(in) :: idx
    integer :: idx_

    idx_ = idx
    if (idx_ < 0) idx_ = this%size_ + idx_ + 1
    if (idx_ <= 0 .or. this%size_ < idx_) error stop 'index out of range'
end function _PROC(_LIST,Index)


function _PROC(_LIST,Node)(this, idx) result(node)
    class(_LIST), intent(in) :: this
    integer, intent(in) :: idx
    type(_NODE), pointer :: node

    integer :: i

    node => this%first
    do i = 1, idx - 1
        node => node%next
    end do
end function _PROC(_LIST,Node)


subroutine _SUBPROC(_LIST,Insert,Element)(this, idx, val)
    class(_LIST), intent(inout) :: this
    integer, intent(in) :: idx
    _IN_TYPE, intent(in) :: val

    integer :: idx_
    type(_NODE), pointer :: inserted, node

    if (idx > this%size_) then
        call this%Append(val)
    else
        if (idx < - this%size_ .or. idx == 0) then
            idx_ = 1
        else
            idx_ = this%Index(idx)
        end if
        allocate(inserted)
        inserted%val = val
        if (idx_ > 1) then
            node => this%Node(idx_ - 1)
            inserted%next => node%next
            node%next => inserted
        else
            inserted%next => this%first
            this%first => inserted
        end if
        this%size_ = this%size_ + 1
    end if
end subroutine _SUBPROC(_LIST,Insert,Element)


#define _ARG_ARRAY
#include "../list/proc_insert.inc"

#include "../list/proc_insert.inc"


subroutine _SUBPROC(_LIST,Delete,Element)(this, idx)
    class(_LIST), intent(inout) :: this
    integer, intent(in) :: idx

    integer :: idx_
    type(_NODE), pointer :: deleted, node

    idx_ = this%Index(idx)
    if (idx_ > 1) then
        node => this%Node(idx_ - 1)
        deleted => node%next
        node%next => deleted%next
        if (idx_ == this%size_) this%last => node
    else
        deleted => this%first
        this%first => deleted%next
        if (idx_ == this%size_) this%last => this%first
    end if
    deallocate(deleted)
    this%size_ = this%size_ - 1
end subroutine _SUBPROC(_LIST,Delete,Element)


subroutine _SUBPROC(_LIST,Delete,Slice)(this, first, last, step)
    class(_LIST), intent(inout) :: this
    integer, intent(in) :: first, last
    integer, intent(in), optional :: step

    integer :: first_, last_, step_, n, i, ii
    type(_NODE), pointer :: deleted, node, zeroth

    step_ = 1
    if (present(step)) step_ = step

    first_ = this%Index(first)
    last_ = this%Index(last)
    n = (last_ - first_) / step_
    if (n >= 0) then
        last_ = first_ + n * step_
        if (step_ < 0) then
            step_ = - step_
            i = first_
            first_ = last_
            last_ = i
        else if (step_ == 0) then
            error stop 'zero step'
        end if
        n = n + 1
        if (first_ > 1) then
            node => this%Node(first_ - 1)
        else
            allocate(zeroth)
            zeroth%next => this%first
            node => zeroth
        end if
        do i = 1, n
            deleted => node%next
            node%next => deleted%next
            deallocate(deleted)
            if (i < n) then
                do ii = 2, step_
                    node => node%next
                end do
            end if
        end do
        if (last_ == this%size_) this%last => node
        if (first_ == 1) then
            this%first => zeroth%next
            deallocate(zeroth)
        end if
        this%size_ = this%size_ - n
    end if
end subroutine _SUBPROC(_LIST,Delete,Slice)


function _PROC(_LIST,Get)(this, idx) result(val)
    class(_LIST), intent(in) :: this
    integer, intent(in) :: idx
    _OUT_TYPE :: val

    type(_NODE), pointer :: node

    node => this%Node(this%Index(idx))
    val = node%val
end function _PROC(_LIST,Get)


subroutine _PROC(_LIST,Set)(this, idx, val)
    class(_LIST), intent(inout) :: this
    integer, intent(in) :: idx
    _IN_TYPE, intent(in) :: val

    type(_NODE), pointer :: node

    node => this%Node(this%Index(idx))
    node%val = val
end subroutine _PROC(_LIST,Set)


#ifdef _NUMBER
#  ifndef _ARRAY
#    define _PROC_SUFFIX Add
#    define _OP +
#    define _ARG_IN_TYPE _IN_TYPE
#    include "../list/proc_op.inc"
#    define _PROC_SUFFIX Multiply
#    define _OP *
#    define _ARG_IN_TYPE _IN_TYPE
#    include "../list/proc_op.inc"
#  else
#    define _PROC_SUFFIX _CAT(Add,Scalar)
#    define _OP +
#    define _ARG_IN_TYPE _ARG_TYPE
#    include "../list/proc_op.inc"
#    define _PROC_SUFFIX _CAT(Multiply,Scalar)
#    define _OP *
#    define _ARG_IN_TYPE _ARG_TYPE
#    include "../list/proc_op.inc"
#    define _PROC_SUFFIX _CAT(Add,Array)
#    define _OP +
#    define _ARG_IN_TYPE _IN_TYPE
#    include "../list/proc_op.inc"
#    define _PROC_SUFFIX _CAT(Multiply,Array)
#    define _OP *
#    define _ARG_IN_TYPE _IN_TYPE
#    include "../list/proc_op.inc"
#  endif
#elif _TYPE == _CHARACTER
#  define _PROC_SUFFIX Concatenate
#  define _OP //
#  define _ARG_IN_TYPE _IN_TYPE
#  include "../list/proc_op.inc"
#endif


function _PROC(_LIST,Find)(this, val, back) result(idx)
    class(_LIST), intent(in) :: this
    _IN_TYPE, intent(in) :: val
    logical, intent(in), optional :: back
    integer :: idx

    logical :: back_
    integer :: i
    type(_NODE), pointer :: node

    back_ = .false.
    if (present(back)) back_ = back

    idx = 0
    node => this%first
    do i = 1, this%size_
        if (_EQUAL(val, node%val)) then
            idx = i
            if (.not. back_) exit
        end if
        node => node%next
    end do
end function _PROC(_LIST,Find)


function _PROC(_LIST,Contains)(this, val) result(found)
    class(_LIST), intent(inout) :: this
    _IN_TYPE, intent(in) :: val
    logical :: found

    found = this%Find(val) /= 0
end function _PROC(_LIST,Contains)


subroutine _PROC(_LIST,Extend)(this, val)
    class(_LIST), intent(inout) :: this
    _IN_TYPE, intent(in) :: val

    if (.not. this%Contains(val)) call this%Append(val)
end subroutine _PROC(_LIST,Extend)


function _PROC(_LIST,ToArray)(this) result(arr)
    class(_LIST), intent(in) :: this
    _OUT_TYPE_ARR :: arr

    integer :: i
    type(_NODE), pointer :: node
#if _TYPE == _CHARACTER
    integer :: maxLen

    maxLen = this%Len()
    allocate(_TYPE_LEN(maxLen) :: arr(this%size_))
#endif

    node => this%first
    do i = 1, this%size_
        arr(_ARR_IDX(i)) = node%val
        node => node%next
    end do
end function _PROC(_LIST,ToArray)


subroutine _PROC(_LIST,Empty)(this)
    class(_LIST), intent(inout) :: this

    integer :: i
    type(_NODE), pointer :: node, next

    node => this%first
    do i = 1, this%size_ - 1
        next => node%next
        deallocate(node)
        node => next
    end do
    nullify(this%first, this%last)
    this%size_ = 0
end subroutine _PROC(_LIST,Empty)


#define _ARG_ARRAY
#include "../list/proc_assign.inc"

#include "../list/proc_assign.inc"


subroutine _PROC(_LIST,Final)(this)
    type(_LIST), intent(inout) :: this

    call this%Empty()
end subroutine _PROC(_LIST,Final)


#undef _IN_TYPE
#undef _IN_TYPE_ARR
#undef _OUT_TYPE
#undef _OUT_TYPE_ARR
#undef _ARR_IDX
#undef _ARR_SIZE
#undef _EQUAL
#undef _ARRAY_LABEL
