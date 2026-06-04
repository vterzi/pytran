! {"id.inc"(_ID)} + () -(_PROC)> ()
! Define conversion functions from the real type to the character type.


#define _PROC _(to_character_)_(_LABEL)
pure function _PROC(arg, mode, width, decimals, expwidth, sign) result(res)
    _ARG_TYPE, intent(in) :: arg
    integer, intent(in), optional :: mode, width, decimals, expwidth
    logical, intent(in), optional :: sign
    character(len=:), allocatable :: res

    logical :: present_mode, present_width, present_decimals, &
        present_expwidth, present_sign, sign_
    integer :: mode_, width_, decimals_, expwidth_, extra_width
    character(len=2) :: mode_str
    character(len=MAX_LEN_INTEGER_STR) :: width_str, decimals_str, expwidth_str
    character(len=3) :: sign_str
    character(len=_(MAX_LEN_REAL_)_(_KIND_LABEL)_(_STR)) :: buffer

    present_mode = present(mode)
    present_width = present(width)
    present_decimals = present(decimals)
    present_expwidth = present(expwidth)
    present_sign = present(sign)
    if ( &
        present_mode &
        .or. present_width &
        .or. present_decimals &
        .or. present_expwidth &
        .or. present_sign &
    ) then
        if (present_mode) then
            mode_ = mode
        else
            mode_ = MODE_GENERAL
        end if
        select case (mode_)
        case (MODE_FIXED)
            mode_str = 'f'
            present_expwidth = .false.
            extra_width = FIXED_EXTRA_WIDTH + max(0, dec_exponent(arg))
        case (MODE_EXPONENTIAL)
            mode_str = 'e'
            extra_width = EXPONENTIAL_EXTRA_WIDTH
        case (MODE_SCIENTIFIC)
            mode_str = 'es'
            extra_width = SCIENTIFIC_EXTRA_WIDTH
        case (MODE_ENGINEERING)
            mode_str = 'en'
            extra_width = SCIENTIFIC_EXTRA_WIDTH + modulo(dec_exponent(arg), 3)
        case (MODE_EXPONENTIAL_D)
            mode_str = 'd'
            extra_width = EXPONENTIAL_EXTRA_WIDTH
        case default
            mode_str = 'g'
            extra_width = EXPONENTIAL_EXTRA_WIDTH
        end select
        if (present_sign) then
            sign_ = sign
        else
            sign_ = .false.
        end if
        if (sign_) then
            sign_str = 'sp,'
        else
            sign_str = ''
        end if
        if (present_decimals) then
            decimals_ = max(0, decimals)
        else
            decimals_ = _(REAL_)_(_KIND_LABEL)_(_DEC_DIGITS)
        end if
        write(decimals_str, *) decimals_
        if (present_expwidth) then
            expwidth_ = max(1, expwidth)
            write(expwidth_str, *) expwidth_
            expwidth_str = 'e' // adjustl(expwidth_str)
        else
            expwidth_ = _(REAL_)_(_KIND_LABEL)_(_EXP_WIDTH)
            expwidth_str = ''
        end if
        if (present_width) then
            width_ = max(1, width)
        else
            width_ = decimals_ + extra_width
            if (mode /= MODE_FIXED) width_ = width_ + expwidth_
            if (arg < 0 .or. sign_) width_ = width_ + 1
        end if
        write(width_str, *) width_
        allocate(character(len=width_) :: res)
        write( &
            res, &
            '(' // trim(sign_str) // trim(mode_str) &
            // trim(adjustl(width_str)) // '.' // trim(adjustl(decimals_str)) &
            // trim(expwidth_str) // ')' &
        ) arg
    else
        write(buffer, *) arg
        res = trim(adjustl(buffer))
    end if
end function _PROC
#undef _PROC
