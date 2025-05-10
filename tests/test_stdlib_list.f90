program test_stdlib_list
    use stdlib_unittest
    use stdlib_list

    implicit none

    logical :: success
    character(len=:), allocatable :: msg
    integer :: emptyIntegerArr(0)
    character(len=0) :: emptyCharacterArr(0)

    success = .true.
    call test_Dimension()
    call test_Len()
    call test_Size()
    call test_AppendElement()
    call test_AppendArray()
    call test_AppendList()
    call test_InsertElement()
    call test_InsertArray()
    call test_InsertList()
    call test_DeleteElement()
    call test_DeleteSlice()
    call test_Get()
    call test_Set()
    call test_Op()
    call test_Find()
    call test_Contains()
    call test_Extend()
    call test_ToArray()
    call test_Empty()
    if (.not. success) stop 1

contains
    subroutine test_Dimension()
        type(IntegerArrayList) :: list

        msg = 'Dimension'
        list = IntegerArrayList(3)
        call assert_equal(3, list%Dimension(), msg, success)
        list = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        call assert_equal(2, list%Dimension(), msg, success)
    end subroutine test_Dimension


    subroutine test_Len()
        type(CharacterList) :: list

        msg = 'Len'
        call assert_equal(0, list%Len(), msg, success)
        call list%Append('123')
        call assert_equal(3, list%Len(), msg, success)
        call list%Append('4567')
        call assert_equal(4, list%Len(), msg, success)
        call list%Append('89')
        call assert_equal(4, list%Len(), msg, success)
        call list%Delete(1)
        call assert_equal(4, list%Len(), msg, success)
        call list%Delete(1)
        call assert_equal(2, list%Len(), msg, success)
    end subroutine test_Len


    subroutine test_Size()
        type(IntegerList) :: list

        msg = 'Size'
        list = [-1, -2, -3, -4, -5]
        call assert_equal(5, list%Size(), msg, success)
        call list%Append(-6)
        call assert_equal(6, list%Size(), msg, success)
        call list%Append(-7)
        call assert_equal(7, list%Size(), msg, success)
        call list%Delete(1)
        call assert_equal(6, list%Size(), msg, success)
        call list%Delete(1)
        call assert_equal(5, list%Size(), msg, success)
        call list%Delete(1)
        call assert_equal(4, list%Size(), msg, success)
    end subroutine test_Size


    subroutine test_AppendElement()
        type(IntegerList) :: list

        msg = 'AppendElement'
        call list%Append(1)
        call assert_true(ALL([1] == list%ToArray()), msg, success)
        call list%Append(2)
        call assert_true(ALL([1, 2] == list%ToArray()), msg, success)
        call list%Append(3)
        call assert_true(ALL([1, 2, 3] == list%ToArray()), msg, success)
    end subroutine test_AppendElement


    subroutine test_AppendArray()
        type(IntegerList) :: list

        msg = 'AppendArray'
        call list%Append([1, 2])
        call assert_true(ALL([1, 2] == list%ToArray()), msg, success)
        call list%Append([3, 4])
        call assert_true(ALL([1, 2, 3, 4] == list%ToArray()), msg, success)
        call list%Append(5)
        call assert_true(ALL([1, 2, 3, 4, 5] == list%ToArray()), msg, success)
    end subroutine test_AppendArray


    subroutine test_AppendList()
        type(IntegerList) :: list, list_

        msg = 'AppendList'
        list_ = [1, 2]
        call list%Append(list_)
        call assert_true(ALL([1, 2] == list%ToArray()), msg, success)
        list_ = [3, 4]
        call list%Append(list_)
        call assert_true(ALL([1, 2, 3, 4] == list%ToArray()), msg, success)
        call list%Append(5)
        call assert_true(ALL([1, 2, 3, 4, 5] == list%ToArray()), msg, success)
    end subroutine test_AppendList


    subroutine test_InsertElement()
        type(IntegerList) :: list

        msg = 'InsertElement'
        call list%Insert(1, 5)
        call assert_true(ALL([5] == list%ToArray()), msg, success)
        call list%Insert(0, 2)
        call assert_true(ALL([2, 5] == list%ToArray()), msg, success)
        call list%Insert(-3, 1)
        call assert_true(ALL([1, 2, 5] == list%ToArray()), msg, success)
        call list%Insert(3, 4)
        call assert_true(ALL([1, 2, 4, 5] == list%ToArray()), msg, success)
        call list%Insert(-2, 3)
        call assert_true(ALL([1, 2, 3, 4, 5] == list%ToArray()), msg, success)
        call list%Insert(7, 6)
        call assert_true(ALL([1, 2, 3, 4, 5, 6] == list%ToArray()), msg, success)
        call list%Append(7)
        call assert_true(ALL([1, 2, 3, 4, 5, 6, 7] == list%ToArray()), msg, success)
    end subroutine test_InsertElement


    subroutine test_InsertArray()
        type(IntegerList) :: list

        msg = 'InsertArray'
        call list%Insert(1, [9, 10])
        call assert_true(ALL([9, 10] == list%ToArray()), msg, success)
        call list%Insert(0, [3, 4])
        call assert_true(ALL([3, 4, 9, 10] == list%ToArray()), msg, success)
        call list%Insert(-5, [1, 2])
        call assert_true(ALL([1, 2, 3, 4, 9, 10] == list%ToArray()), msg, success)
        call list%Insert(5, [7, 8])
        call assert_true(ALL([1, 2, 3, 4, 7, 8, 9, 10] == list%ToArray()), msg, success)
        call list%Insert(-4, [5, 6])
        call assert_true(ALL([1, 2, 3, 4, 5, 6, 7, 8, 9, 10] == list%ToArray()), msg, success)
        call list%Insert(12, [11, 12])
        call assert_true(ALL([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12] == list%ToArray()), msg, success)
        call list%Append(13)
        call assert_true(ALL([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13] == list%ToArray()), msg, success)
    end subroutine test_InsertArray


    subroutine test_InsertList()
        type(IntegerList) :: list, list_

        msg = 'InsertList'
        list_ = [9, 10]
        call list%Insert(1, list_)
        call assert_true(ALL([9, 10] == list%ToArray()), msg, success)
        list_ = [3, 4]
        call list%Insert(0, list_)
        call assert_true(ALL([3, 4, 9, 10] == list%ToArray()), msg, success)
        list_ = [1, 2]
        call list%Insert(-5, list_)
        call assert_true(ALL([1, 2, 3, 4, 9, 10] == list%ToArray()), msg, success)
        list_ = [7, 8]
        call list%Insert(5, list_)
        call assert_true(ALL([1, 2, 3, 4, 7, 8, 9, 10] == list%ToArray()), msg, success)
        list_ = [5, 6]
        call list%Insert(-4, list_)
        call assert_true(ALL([1, 2, 3, 4, 5, 6, 7, 8, 9, 10] == list%ToArray()), msg, success)
        list_ = [11, 12]
        call list%Insert(12, list_)
        call assert_true(ALL([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12] == list%ToArray()), msg, success)
        call list%Append(13)
        call assert_true(ALL([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13] == list%ToArray()), msg, success)
    end subroutine test_InsertList


    subroutine test_DeleteElement()
        type(IntegerList) :: list

        msg = 'DeleteElement'
        list = [1, 2, 3, 4, 5, 6, 7]
        call list%Delete(1)
        call assert_true(ALL([2, 3, 4, 5, 6, 7] == list%ToArray()), msg, success)
        call list%Delete(3)
        call assert_true(ALL([2, 3, 5, 6, 7] == list%ToArray()), msg, success)
        call list%Delete(-2)
        call assert_true(ALL([2, 3, 5, 7] == list%ToArray()), msg, success)
        call list%Delete(4)
        call assert_true(ALL([2, 3, 5] == list%ToArray()), msg, success)
        call list%Delete(-1)
        call assert_true(ALL([2, 3] == list%ToArray()), msg, success)
        call list%Append(8)
        call assert_true(ALL([2, 3, 8] == list%ToArray()), msg, success)
        call list%Delete(3)
        call list%Delete(1)
        call list%Delete(1)
        call assert_true(ALL(emptyIntegerArr == list%ToArray()), msg, success)
        call list%Append(9)
        call assert_true(ALL([9] == list%ToArray()), msg, success)
    end subroutine test_DeleteElement


    subroutine test_DeleteSlice()
        type(IntegerList) :: list

        msg = 'DeleteSlice'
        list = [1, 2, 3, 4, 5, 6, 7]
        call list%Delete(1, 7, -2)
        call assert_true(ALL([1, 2, 3, 4, 5, 6, 7] == list%ToArray()), msg, success)
        call list%Delete(6, 2)
        call assert_true(ALL([1, 2, 3, 4, 5, 6, 7] == list%ToArray()), msg, success)
        call list%Delete(2, 2)
        call assert_true(ALL([1, 3, 4, 5, 6, 7] == list%ToArray()), msg, success)
        call list%Delete(1, 3)
        call assert_true(ALL([5, 6, 7] == list%ToArray()), msg, success)
        call list%Delete(-2, -1)
        call assert_true(ALL([5] == list%ToArray()), msg, success)
        call list%Append(8)
        call assert_true(ALL([5, 8] == list%ToArray()), msg, success)
        call list%Delete(1, 2, 1)
        call assert_true(ALL(emptyIntegerArr == list%ToArray()), msg, success)
        call list%Append(9)
        call assert_true(ALL([9] == list%ToArray()), msg, success)
        list = [1, 2, 3, 4, 5, 6, 7]
        call list%Delete(1, 7, 3)
        call assert_true(ALL([2, 3, 5, 6] == list%ToArray()), msg, success)
        call list%Delete(-1, -4, -2)
        call assert_true(ALL([2, 5] == list%ToArray()), msg, success)
        call list%Append(8)
        call assert_true(ALL([2, 5, 8] == list%ToArray()), msg, success)
        call list%Delete(-1, -3, -1)
        call assert_true(ALL(emptyIntegerArr == list%ToArray()), msg, success)
        call list%Append(9)
        call assert_true(ALL([9] == list%ToArray()), msg, success)
    end subroutine test_DeleteSlice


    subroutine test_Get()
        type(IntegerList) :: list

        msg = 'Get'
        list = [1, 2, 3, 4, 5, 6, 7]
        call assert_equal(1, list%Get(1), msg, success)
        call assert_equal(7, list%Get(7), msg, success)
        call assert_equal(3, list%Get(3), msg, success)
        call assert_equal(6, list%Get(-2), msg, success)
    end subroutine test_Get


    subroutine test_Set()
        type(IntegerList) :: list

        msg = 'Set'
        list = [1, 2, 3, 4, 5, 6, 7]
        call list%Set(1, -1)
        call assert_true(ALL([-1, 2, 3, 4, 5, 6, 7] == list%ToArray()), msg, success)
        call list%Set(7, -7)
        call assert_true(ALL([-1, 2, 3, 4, 5, 6, -7] == list%ToArray()), msg, success)
        call list%Set(3, -3)
        call assert_true(ALL([-1, 2, -3, 4, 5, 6, -7] == list%ToArray()), msg, success)
        call list%Set(-2, -6)
        call assert_true(ALL([-1, 2, -3, 4, 5, -6, -7] == list%ToArray()), msg, success)
    end subroutine test_Set


    subroutine test_Op()
        type(IntegerList) :: iList
        type(IntegerArrayList) :: iaList
        type(CharacterList) :: cList

        msg = 'Op'
        iList = [1, 2, 3, 4, 5, 6, 7]
        call iList%Add(1, 8)
        call assert_true(ALL([9, 2, 3, 4, 5, 6, 7] == iList%ToArray()), msg, success)
        call iList%Add(7, 9)
        call assert_true(ALL([9, 2, 3, 4, 5, 6, 16] == iList%ToArray()), msg, success)
        call iList%Add(3, 10)
        call assert_true(ALL([9, 2, 13, 4, 5, 6, 16] == iList%ToArray()), msg, success)
        call iList%Add(-2, 11)
        call assert_true(ALL([9, 2, 13, 4, 5, 17, 16] == iList%ToArray()), msg, success)
        iList = [1, 2, 3, 4, 5, 6, 7]
        call iList%Multiply(1, 8)
        call assert_true(ALL([8, 2, 3, 4, 5, 6, 7] == iList%ToArray()), msg, success)
        call iList%Multiply(7, 9)
        call assert_true(ALL([8, 2, 3, 4, 5, 6, 63] == iList%ToArray()), msg, success)
        call iList%Multiply(3, 10)
        call assert_true(ALL([8, 2, 30, 4, 5, 6, 63] == iList%ToArray()), msg, success)
        call iList%Multiply(-2, 11)
        call assert_true(ALL([8, 2, 30, 4, 5, 66, 63] == iList%ToArray()), msg, success)

        iaList = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], [2, 7])
        call iaList%Add(1, 8)
        call assert_true(ALL(reshape([9, 10, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], [2, 7]) == iaList%ToArray()), &
            msg, success)
        call iaList%Add(7, [9, -9])
        call assert_true(ALL(reshape([9, 10, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 22, 5], [2, 7]) == iaList%ToArray()), &
            msg, success)
        call iaList%Add(3, 10)
        call assert_true(ALL(reshape([9, 10, 3, 4, 15, 16, 7, 8, 9, 10, 11, 12, 22, 5], [2, 7]) == iaList%ToArray()), &
            msg, success)
        call iaList%Add(-2, [11, -11])
        call assert_true(ALL(reshape([9, 10, 3, 4, 15, 16, 7, 8, 9, 10, 22, 1, 22, 5], [2, 7]) == iaList%ToArray()), &
            msg, success)
        iaList = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], [2, 7])
        call iaList%Multiply(1, 8)
        call assert_true(ALL(reshape([8, 16, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], [2, 7]) == iaList%ToArray()), &
            msg, success)
        call iaList%Multiply(7, [9, -9])
        call assert_true(ALL(reshape([8, 16, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 117, -126], [2, 7]) == iaList%ToArray()), &
            msg, success)
        call iaList%Multiply(3, 10)
        call assert_true(ALL(reshape([8, 16, 3, 4, 50, 60, 7, 8, 9, 10, 11, 12, 117, -126], [2, 7]) == iaList%ToArray()), &
            msg, success)
        call iaList%Multiply(-2, [11, -11])
        call assert_true(ALL(reshape([8, 16, 3, 4, 50, 60, 7, 8, 9, 10, 121, -132, 117, -126], [2, 7]) == iaList%ToArray()), &
            msg, success)

        call cList%Append('123')
        call cList%Append('4567')
        call cList%Append('89')
        call cList%Concatenate(1, 'ab')
        call assert_equal('123ab', cList%Get(1), msg, success)
        call assert_equal('4567', cList%Get(2), msg, success)
        call assert_equal('89', cList%Get(3), msg, success)
        call cList%Concatenate(-2, 'de')
        call assert_equal('123ab', cList%Get(1), msg, success)
        call assert_equal('4567de', cList%Get(2), msg, success)
        call assert_equal('89', cList%Get(3), msg, success)
        call cList%Concatenate(3, 'fgh')
        call assert_equal('123ab', cList%Get(1), msg, success)
        call assert_equal('4567de', cList%Get(2), msg, success)
        call assert_equal('89fgh', cList%Get(3), msg, success)
    end subroutine test_Op


    subroutine test_Find()
        type(IntegerList) :: list

        msg = 'Find'
        list = [-1, -2, -3, -2, -1]
        call assert_equal(1, list%Find(-1), msg, success)
        call assert_equal(1, list%Find(-1, .false.), msg, success)
        call assert_equal(5, list%Find(-1, .true.), msg, success)
        call assert_equal(3, list%Find(-3), msg, success)
        call assert_equal(3, list%Find(-3, .true.), msg, success)
        call assert_equal(0, list%Find(-4), msg, success)
    end subroutine test_Find


    subroutine test_Contains()
        type(IntegerList) :: list

        msg = 'Contains'
        list = [-1, -2, -3, -4, -5]
        call assert_true(list%Contains(-1), msg, success)
        call assert_true(list%Contains(-3), msg, success)
        call assert_true(list%Contains(-5), msg, success)
        call assert_false(list%Contains(-6), msg, success)
    end subroutine test_Contains


    subroutine test_Extend()
        type(IntegerList) :: list

        msg = 'Extend'
        list = [1, 2, 3, 4]
        call list%Extend(5)
        call assert_true(ALL([1, 2, 3, 4, 5] == list%ToArray()), msg, success)
        call list%Extend(1)
        call assert_true(ALL([1, 2, 3, 4, 5] == list%ToArray()), msg, success)
        call list%Extend(2)
        call assert_true(ALL([1, 2, 3, 4, 5] == list%ToArray()), msg, success)
        call list%Extend(6)
        call assert_true(ALL([1, 2, 3, 4, 5, 6] == list%ToArray()), msg, success)
        call list%Append(7)
        call assert_true(ALL([1, 2, 3, 4, 5, 6, 7] == list%ToArray()), msg, success)
    end subroutine test_Extend


    subroutine test_ToArray()
        type(IntegerList) :: list

        msg = 'ToArray'
        list = [1, 2, 3, 4, 5]
        call assert_true(ALL([1, 2, 3, 4, 5] == list%ToArray()), msg, success)
    end subroutine test_ToArray


    subroutine test_Empty()
        type(IntegerList) :: list

        msg = 'Empty'
        list = [1, 2, 3, 4, 5]
        call list%Empty()
        call assert_true(ALL(emptyIntegerArr == list%ToArray()), msg, success)
    end subroutine test_Empty
end program test_stdlib_list
