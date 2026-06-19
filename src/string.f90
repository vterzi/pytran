module pytran_string
    implicit none

    private

    character(len=*), parameter, public :: &
        NUL = achar(0), &  ! null
        SOH = achar(1), &  ! start of heading
        STX = achar(2), &  ! start of text
        ETX = achar(3), &  ! end of text
        EOT = achar(4), &  ! end of transmission
        ENQ = achar(5), &  ! enquiry
        ACK = achar(6), &  ! acknowledge
        BEL = achar(7), &  ! bell
        BS = achar(8), &  ! backspace
        TAB = achar(9), &  ! horizontal tab
        LF = achar(10), &  ! line feed
        VT = achar(11), &  ! vertical tab
        FF = achar(12), &  ! form feed
        CR = achar(13), &  ! carriage return
        SO = achar(14), &  ! shift out
        SI = achar(15), &  ! shift in
        DLE = achar(16), &  ! data link escape
        DC1 = achar(17), &  ! device control 1
        DC2 = achar(18), &  ! device control 2
        DC3 = achar(19), &  ! device control 3
        DC4 = achar(20), &  ! device control 4
        NAK = achar(21), &  ! negative acknowledge
        SYN = achar(22), &  ! synchronous idle
        ETB = achar(23), &  ! end of transmission block
        CAN = achar(24), &  ! cancel
        EM = achar(25), &  ! end of medium
        SUB = achar(26), &  ! substitute
        ESC = achar(27), &  ! escape
        FS = achar(28), &  ! file separator
        GS = achar(29), &  ! group separator
        RS = achar(30), &  ! record separator
        US = achar(31), &  ! unit separator
        DEL = achar(127), &  ! delete
        DIGITS = "0123456789", &
        BINDIGITS = DIGITS(:2), &
        OCTDIGITS = DIGITS(:8), &
        ASCII_LOWERCASE = "abcdefghijklmnopqrstuvwxyz", &
        ASCII_UPPERCASE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ", &
        ASCII_LETTERS = ASCII_LOWERCASE // ASCII_UPPERCASE, &
        HEXDIGITS = DIGITS // ASCII_LOWERCASE(:5) // ASCII_UPPERCASE(:5), &
        PUNCTUATION = '!"' // "#$%&'()*+,-./:;<=>?@[" // achar(92) &
            // "]^_`{|}~", &
        WHITESPACE = " " // TAB// LF // CR // VT // FF, &
        PRINTABLE = DIGITS // ASCII_LETTERS // PUNCTUATION // WHITESPACE, &
        NONPRINTABLE = NUL // SOH // STX // ETX // EOT // ENQ // ACK // BEL &
            // BS // SO // SI // DLE // DC1 // DC2 // DC3 // DC4 // NAK &
            // SYN // ETB // CAN // EM // SUB // ESC // FS // GS // RS // US &
            // DEL, &
        WORDCHARS = DIGITS // ASCII_LETTERS // "_"

    integer, parameter :: &
        CASESHIFT = iachar(ASCII_LOWERCASE(:1)) - iachar(ASCII_UPPERCASE(:1))

contains
    elemental function isspace(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 .and. verify(arg, WHITESPACE) == 0
    end function isspace


    elemental function isdigit(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 .and. verify(arg, DIGITS) == 0
    end function isdigit


    elemental function isalpha(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 .and. verify(arg, ASCII_LETTERS) == 0
    end function isalpha


    elemental function isalnum(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 .and. verify(arg, DIGITS // ASCII_LETTERS) == 0
    end function isalnum


    elemental function isidentifier(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 &
            .and. verify(arg, WORDCHARS) == 0 &
            .and. scan(arg(1:1), DIGITS // "_") == 0
    end function isidentifier


    elemental function isprintable(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = verify(arg, PRINTABLE) == 0
    end function isprintable


    elemental function isascii(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = verify(arg, PRINTABLE // NONPRINTABLE) == 0
    end function isascii


    elemental function isupper(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = ( &
            scan(arg, ASCII_UPPERCASE) > 0 &
            .and. verify(arg, ASCII_LOWERCASE) == 0 &
        )
    end function isupper


    elemental function islower(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = ( &
            scan(arg, ASCII_LOWERCASE) > 0 &
            .and. verify(arg, ASCII_UPPERCASE) == 0 &
        )
    end function islower


    elemental function upper(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=len(arg)) :: res

        integer :: i

        res = arg
        i = 0
        do
            i = scan(res(i + 1 :), ASCII_LOWERCASE)
            if (i == 0) exit
            res(i:i) = achar(iachar(res(i:i)) - CASESHIFT)
        end do
    end function upper


    elemental function lower(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=len(arg)) :: res

        integer :: i

        res = arg
        i = 0
        do
            i = scan(res(i + 1 :), ASCII_UPPERCASE)
            if (i == 0) exit
            res(i:i) = achar(iachar(res(i:i)) + CASESHIFT)
        end do
    end function lower


    pure function strip(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: res

        res = trim(adjustl(arg))
    end function strip


    elemental function is_logical(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        integer :: n, i

        n = len(arg)
        res = n > 0
        if (.not. res) return
        i = 1
        if (arg(i:i) == ".") i = i + 1
        res = i <= n
        if (.not. res) return
        res = verify(arg(i:i), "TtFf") == 0
    end function is_logical


    elemental function is_sign(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = arg == "+" .or. arg == "-"
    end function is_sign


    elemental function is_unsigned_integer(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 .and. verify(arg, DIGITS) == 0
    end function is_unsigned_integer


    elemental function is_integer(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        integer :: i

        res = len(arg) > 0
        if (.not. res) return
        i = 1
        if (is_sign(arg(i:i))) i = i + 1
        res = is_unsigned_integer(arg(i:))
    end function is_integer


    elemental function is_unsigned_fixed_real(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        integer :: i

        i = index(arg, ".")
        if (i > 0) then
            res = is_unsigned_integer(arg(: i - 1) // arg(i + 1 :))
        else
            res = is_unsigned_integer(arg)
        end if
    end function is_unsigned_fixed_real


    elemental function is_fixed_real(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        integer :: i

        res = len(arg) > 0
        if (.not. res) return
        i = 1
        if (is_sign(arg(i:i))) i = i + 1
        res = is_unsigned_fixed_real(arg(i:))
    end function is_fixed_real


    elemental function is_unsigned_float_real(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        integer :: i
        character(len=len(arg)) :: temp

        temp = lower(arg)
        if (temp == "nan" .or. temp == "inf" .or. temp == "infinity") then
            res = .true.
        else
            i = scan(arg, "EeDd")
            if (i > 0) then
                res = ( &
                    is_unsigned_fixed_real(arg(: i - 1)) &
                    .and. is_integer(arg(i + 1 :)) &
                )
            else
                res = is_unsigned_fixed_real(arg)
            end if
        end if
    end function is_unsigned_float_real


    elemental function is_float_real(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        integer :: i

        res = len(arg) > 0
        if (.not. res) return
        i = 1
        if (is_sign(arg(i:i))) i = i + 1
        res = is_unsigned_float_real(arg(i:))
    end function is_float_real
end module pytran_string
