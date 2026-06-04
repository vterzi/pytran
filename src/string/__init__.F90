! ASCII characters


module stdlib_string
    use stdlib_kinds
    use stdlib_base, only: assign_optional
    use stdlib_math, only: dec_digits, dec_exponent

    implicit none

    private
    public :: &
        isspace, isdigit, isdecimal, isnumeric, isalpha, isalnum, &
        isidentifier, isprintable, isascii, isupper, islower, &
        upper, lower, casefold, capitalize, strip, center, startswith, endswith

    !--------------------------------------------------------------------------

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
        UPPERCASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
        LOWERCASE = 'abcdefghijklmnopqrstuvwxyz', &
        LETTERS = UPPERCASE // LOWERCASE, &
        DIGITS = '0123456789', &
        BINDIGITS = DIGITS(:2), &
        OCTDIGITS = DIGITS(:8), &
        HEXDIGITS = DIGITS // UPPERCASE(:5) // LOWERCASE(:5), &
        PUNCTUATION = '!"#$%&' // "'" // '()*+,-./:;<=>?@[' // achar(92) &
            // ']^_`{|}~', &
        WHITESPACE = TAB // LF // VT // FF // CR // ' ', &
        PRINTABLE = LETTERS // DIGITS // PUNCTUATION // WHITESPACE, &
        NONPRINTABLE = NUL // SOH // STX // ETX // EOT // ENQ // ACK // BEL &
            // BS // SO // SI // DLE // DC1 // DC2 // DC3 // DC4 // NAK &
            // SYN // ETB // CAN // EM // SUB // ESC // FS // GS // RS // US &
            // DEL, &
        WORDCHARS = LETTERS // '_' // DIGITS

    !--------------------------------------------------------------------------

#ifdef _K1
    integer, parameter :: MAX_LEN_INTEGER_K1_STR = _MAX_LEN_INTEGER_K1_STR
#endif
#ifdef _K2
    integer, parameter :: MAX_LEN_INTEGER_K2_STR = _MAX_LEN_INTEGER_K2_STR
#endif
#ifdef _K4
    integer, parameter :: MAX_LEN_INTEGER_K4_STR = _MAX_LEN_INTEGER_K4_STR
#endif
#ifdef _K8
    integer, parameter :: MAX_LEN_INTEGER_K8_STR = _MAX_LEN_INTEGER_K8_STR
#endif
#ifdef _K16
    integer, parameter :: MAX_LEN_INTEGER_K16_STR = _MAX_LEN_INTEGER_K16_STR
#endif

    ! REAL_K_DEC_DIGITS = ceiling(digits(0._K) * log10(2.))
    ! REAL_K_EXP_WIDTH = ceiling(log10(log10(huge(0._K))))
#ifdef _HP
    integer, parameter :: &
        REAL_HP_DEC_DIGITS = 4, &
        REAL_HP_EXP_WIDTH = 1, &
        MAX_LEN_REAL_HP_STR = _MAX_LEN_REAL_HP_STR, &
        MAX_LEN_COMPLEX_HP_STR = _MAX_LEN_COMPLEX_HP_STR
#endif
#ifdef _SP
    integer, parameter :: &
        REAL_SP_DEC_DIGITS = 8, &
        REAL_SP_EXP_WIDTH = 2, &
        MAX_LEN_REAL_SP_STR = _MAX_LEN_REAL_SP_STR, &
        MAX_LEN_COMPLEX_SP_STR = _MAX_LEN_COMPLEX_SP_STR
#endif
#ifdef _DP
    integer, parameter :: &
        REAL_DP_DEC_DIGITS = 16, &
        REAL_DP_EXP_WIDTH = 3, &
        MAX_LEN_REAL_DP_STR = _MAX_LEN_REAL_DP_STR, &
        MAX_LEN_COMPLEX_DP_STR = _MAX_LEN_COMPLEX_DP_STR
#endif
#ifdef _XDP
    integer, parameter :: &
        REAL_XDP_DEC_DIGITS = 20, &
        REAL_XDP_EXP_WIDTH = 4, &
        MAX_LEN_REAL_XDP_STR = _MAX_LEN_REAL_XDP_STR, &
        MAX_LEN_COMPLEX_XDP_STR = _MAX_LEN_COMPLEX_XDP_STR
#endif
#ifdef _QP
    integer, parameter :: &
        REAL_QP_DEC_DIGITS = 35, &
        REAL_QP_EXP_WIDTH = 4, &
        MAX_LEN_REAL_QP_STR = _MAX_LEN_REAL_QP_STR, &
        MAX_LEN_COMPLEX_QP_STR = _MAX_LEN_COMPLEX_QP_STR
#endif

    integer, parameter :: &
        MAX_LEN_LOGICAL_STR = _MAX_LEN_LOGICAL_STR, &
        MAX_LEN_INTEGER_STR = _MAX_LEN_INTEGER_STR, &
        MAX_LEN_REAL_STR = _MAX_LEN_REAL_STR, &
        MAX_LEN_COMPLEX_STR = _MAX_LEN_COMPLEX_STR

    !--------------------------------------------------------------------------

    integer, parameter, public :: &
        MODE_GENERAL = 0, &  ! `g`
        MODE_FIXED = 1, &  ! `f`
        MODE_EXPONENTIAL = 2, &  ! `e`
        MODE_SCIENTIFIC = 3, &  ! `es`
        MODE_ENGINEERING = 4, &  ! `en`
        MODE_EXPONENTIAL_D = 5  ! `d`
    integer, parameter :: &
        ! leading digit + decimal point
        FIXED_EXTRA_WIDTH = 2, &
        ! ... + exponent delimiter + exponent sign
        SCIENTIFIC_EXTRA_WIDTH = FIXED_EXTRA_WIDTH + 2, &
        ! ... + leading zero
        EXPONENTIAL_EXTRA_WIDTH = SCIENTIFIC_EXTRA_WIDTH + 1, &
        ! left parenthesis + comma + right parenthesis
        COMPLEX_EXTRA_WIDTH = 3

    !--------------------------------------------------------------------------

    integer, parameter :: &
        CASESHIFT = iachar(LOWERCASE(:1)) - iachar(UPPERCASE(:1))

    !--------------------------------------------------------------------------

#include "../inc/proc.inc"

#define _DECL(X) public :: _(to_)_(_TYPE_NAME)_(X)
#define _ID _LOGICAL
#define _DEFAULT
#include "../inc/decl.inc"
#define _ID _INTEGER
#define _DEFAULT
#include "../inc/decl.inc"
#define _ID _REAL
#define _DEFAULT
#include "../inc/decl.inc"
#define _ID _COMPLEX
#define _DEFAULT
#include "../inc/decl.inc"
#undef _DECL

#define _OP to_character
    public :: _OP
    interface _OP
#define _ID _LOGICAL
#include "../inc/decls.inc"
#define _ID _INTEGER
#include "../inc/decls.inc"
#define _ID _REAL
#include "../inc/decls.inc"
#define _ID _COMPLEX
#include "../inc/decls.inc"
#define _ID _CHARACTER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#define _OP cat
    public :: operator(//)
    interface operator(//)
#define _ID1 _LOGICAL
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _LOGICAL
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _INTEGER
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _INTEGER
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _REAL
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _REAL
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _COMPLEX
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _COMPLEX
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#if defined(_UCS4) & defined(_ASCII) & (_UCS4 != _ASCII)
        module procedure :: sa_cat_su
        module procedure :: su_cat_sa
#endif
    end interface
#undef _OP

#define _OP add
    public :: operator(+)
    interface operator(+)
#define _ID1 _CHARACTER
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY1
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _LOGICAL
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _LOGICAL
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _INTEGER
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _INTEGER
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _REAL
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _REAL
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _COMPLEX
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _COMPLEX
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
    end interface
#undef _OP

#define _OP mul
    public :: operator(*)
    interface operator(*)
#define _ID1 _INTEGER
#define _ID2 _CHARACTER
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _INTEGER
#include "../inc/decls.inc"
    end interface
#undef _OP

    interface isdecimal
        module procedure isdigit
    end interface isdecimal

    interface isnumeric
        module procedure isdigit
    end interface isnumeric

    interface casefold
        module procedure lower
    end interface casefold

    interface operator(.in.)
        module procedure in
    end interface

    !--------------------------------------------------------------------------
contains
    !--------------------------------------------------------------------------

#define _FILE "../string/to_character_l.inc"
#define _ID _LOGICAL
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../string/to_character_i.inc"
#define _ID _INTEGER
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../string/to_character_r.inc"
#define _ID _REAL
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../string/to_character_c.inc"
#define _ID _COMPLEX
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../string/to_character_s.inc"
#define _ID _CHARACTER
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../string/to_type.inc"
#define _ID _LOGICAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _INTEGER
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _REAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _COMPLEX
#define _DEFAULT
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../string/cat.inc"
#define _ID1 _CHARACTER
#define _ID2 _LOGICAL
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _INTEGER
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _REAL
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _COMPLEX
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#undef _FILE


#if defined(_UCS4) & defined(_ASCII) & (_UCS4 != _ASCII)
    function sa_cat_su(arg1, arg2) result(res)
        character(len=*, kind=_ASCII), intent(in) :: arg1
        character(len=*, kind=_UCS4), intent(in) :: arg2
        character(len=(len(arg1) + len(arg2)), kind=_UCS4) :: res

        integer :: i

        i = len(arg1)
        res(:i) = arg1
        res(i + 1 :) = arg2
    end function sa_cat_su


    function su_cat_sa(arg1, arg2) result(res)
        character(len=*, kind=_UCS4), intent(in) :: arg1
        character(len=*, kind=_ASCII), intent(in) :: arg2
        character(len=(len(arg1) + len(arg2)), kind=_UCS4) :: res

        integer :: i

        i = len(arg1)
        res(:i) = arg1
        res(i + 1 :) = arg2
    end function su_cat_sa
#endif


#define _FILE "../string/add_s.inc"
#define _ID _CHARACTER
#define _DEFAULT_ONLY
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../string/add.inc"
#define _ID1 _CHARACTER
#define _ID2 _LOGICAL
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _INTEGER
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _REAL
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _COMPLEX
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../string/mul.inc"
#define _ID1 _INTEGER
#define _ID2 _CHARACTER
#include "../inc/defs.inc"
#undef _FILE

    !--------------------------------------------------------------------------
    ! Iteration over characters may be faster than `verify` on some compilers
    ! but slower on others.  Therefore, a simpler implementation that uses
    ! intrinsic procedures is preferred.  An example of the alternative code
    ! for a slice of the ASCII collating sequence `CHARACTERS`:
    ! ```
    ! integer :: i
    ! character(len=1) :: chr
    ! res = .false.
    ! do i = 1, len(arg)
    !     chr = arg(i:i)
    !     res = CHARACTERS(:1) <= chr .and. chr <= CHARACTERS(len(CHARACTERS):)
    !     if (.not. res) exit
    ! end do
    ! ```

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

        res = len(arg) > 0 .and. verify(arg, LETTERS) == 0
    end function isalpha


    elemental function isalnum(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 .and. verify(arg, LETTERS // DIGITS) == 0
    end function isalnum


    elemental function isidentifier(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = len(arg) > 0 &
            .and. verify(arg, WORDCHARS) == 0 &
            .and. scan(arg(1:1), DIGITS // '_') == 0
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

        res = scan(arg, UPPERCASE) > 0 .and. verify(arg, LOWERCASE) == 0
    end function isupper


    elemental function islower(arg) result(res)
        character(len=*), intent(in) :: arg
        logical :: res

        res = scan(arg, LOWERCASE) > 0 .and. verify(arg, UPPERCASE) == 0
    end function islower

    !--------------------------------------------------------------------------

    elemental function upper(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=len(arg)) :: res

        integer :: i

        res = arg
        i = 0
        do
            i = scan(res(i + 1 :), LOWERCASE)
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
            i = scan(res(i + 1 :), UPPERCASE)
            if (i == 0) exit
            res(i:i) = achar(iachar(res(i:i)) + CASESHIFT)
        end do
    end function lower


    elemental function capitalize(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=len(arg)) :: res

        res = lower(arg)
        if (len(res) > 0) res(1:1) = upper(res(1:1))
    end function capitalize

    !--------------------------------------------------------------------------

    pure function strip(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: res

        ! `trim(adjustl(arg))` only works on spaces.
        res = arg(verify(arg, WHITESPACE) : verify(arg, WHITESPACE, .true.))
    end function strip


    elemental function center(str, width, fillchar) result(res)
        character(len=*), intent(in) :: str
        integer, intent(in) :: width
        character(len=1), intent(in), optional :: fillchar
        character(len=max(len(str), width)) :: res

        integer :: pad
        character(len=1) :: chr

        call assign_optional(chr, ' ', fillchar)
        pad = width - len(str)
        if (pad > 0) then
            res = repeat(chr, pad / 2) // str // repeat(chr, (pad + 1) / 2)
        else
            res = str
        end if
    end function center

    !--------------------------------------------------------------------------

    elemental function in(substr, str) result(res)
        character(len=*), intent(in) :: substr, str
        logical :: res

        res = len(substr) == 0 .or. index(str, substr) > 0
    end function in


    elemental function startswith(str, substr) result(res)
        character(len=*), intent(in) :: str, substr
        logical :: res

        res = str(:len(substr)) == substr
    end function startswith


    elemental function endswith(str, substr) result(res)
        character(len=*), intent(in) :: str, substr
        logical :: res

        res = str(len(str) - len(substr) + 1 : ) == substr
    end function endswith

    !--------------------------------------------------------------------------
end module stdlib_string
