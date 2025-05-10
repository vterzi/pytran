module stdlib_io
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END, IOSTAT_EOR
    use stdlib_kinds
    use stdlib_string, only: LF, WHITESPACE, strip
    use stdlib_list, only: CharacterList

    implicit none

    private
    public :: read_line, read_unit, read_file, split_lines, split, tokenize

    integer, parameter :: LEN_BUFFER = 256

contains
    subroutine read_line(unit, line, eof, success)
        integer, intent(in) :: unit
        logical, intent(out), optional :: eof, success
        character(len=:), allocatable, intent(out) :: line

        integer :: size_, iostat
        character(len=LEN_BUFFER) :: buffer

        if (present(eof)) eof = .false.
        if (present(success)) success = .true.
        iostat = 0
        line = ''
        do while (iostat /= IOSTAT_EOR)
            read(unit, '(a)', advance='no', size=size_, iostat=iostat) buffer
            if (iostat > 0) then
                if (present(success)) success = .false.
                return
            else if (iostat == IOSTAT_END) then
                if (present(eof)) eof = .true.
                return
            end if
            line = line // buffer(:size_)
        end do
    end subroutine read_line


    function read_unit(unit) result(string)
        integer, intent(in) :: unit
        character(len=:), allocatable :: string

        logical :: eof
        character(len=:), allocatable :: line

        string = ''
        do
            call read_line(unit, line, eof)
            string = string // line
            if (eof) exit
            string = string // LF
        end do
    end function read_unit


    function read_file(filename, delete) result(string)
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: delete
        character(len=:), allocatable :: string

        logical :: delete_
        integer :: unit

        delete_ = .false.
        if (present(delete)) delete_ = delete

        open(newunit=unit, file=filename)
        string = read_unit(unit)
        if (delete_) then
            close(unit, status='delete')
        else
            close(unit)
        end if
    end function read_file


    function split_lines(string, crop, feed, discard) result(lines)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: crop, feed
        character(len=*), intent(in), optional :: discard
        type(CharacterList) :: lines

        logical :: crop_
        integer :: i
        character(len=:), allocatable :: buffer, line

        crop_ = .false.
        if (present(crop)) crop_ = crop

        buffer = string
        do while (len(buffer) > 0)
            i = scan(buffer, LF)
            if (i == 0) i = len(buffer) + 1
            line = buffer(: i-1)
            buffer = buffer(i+1 :)
            if (present(discard)) then
                i = scan(line, discard)
                if (i > 0) line = line(: i-1)
            end if
            if (crop_) line = strip(line)
            call lines%Append(line)
        end do
        if (crop_) then
            do i = -1, 1, 2
                do while (len_trim(lines%Get(i)) == 0)
                    call lines%Delete(i)
                end do
            end do
        end if
        if (present(feed)) then
            if (feed) call lines%Append('')
        end if
    end function split_lines


    function split(string, delim) result(tokens)
        character(len=*), intent(in) :: string, delim
        type(CharacterList) :: tokens

        integer :: len_delim, i
        character(len=:), allocatable :: buffer

        len_delim = len(delim)
        buffer = string
        do
            i = index(buffer, delim)
            if (i == 0) exit
            call tokens%Append(buffer(: i-1))
            buffer = buffer(i+len_delim :)
        end do
        call tokens%Append(buffer)
    end function split


    function tokenize(string, delim) result(tokens)
        character(len=*), intent(in) :: string
        character(len=*), intent(in), optional :: delim
        type(CharacterList) :: tokens

        integer :: i
        character(len=:), allocatable :: buffer, delim_

        delim_ = WHITESPACE
        if (present(delim)) delim_ = delim

        buffer = string
        do
            i = verify(buffer, delim_)
            if (i == 0) exit
            buffer = buffer(i:)
            i = scan(buffer, delim_)
            if (i == 0) i = len(buffer) + 1
            call tokens%Append(buffer(: i-1))
            buffer = buffer(i:)
        end do
    end function tokenize
end module stdlib_io
