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
        WORDCHARS = DIGITS // ASCII_LETTERS // '_'
end module pytran_string
