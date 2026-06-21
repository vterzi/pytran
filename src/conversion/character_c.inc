#define _OP character
pure function _PROC(arg, mode, width, decimals, expwidth, sign) result(res)
    _TYPE(_KIND), intent(in) :: arg
    integer, intent(in), optional :: mode, width, decimals, expwidth
    logical, intent(in), optional :: sign
    character(len=:), allocatable :: res

    logical :: present_mode, present_width, present_decimals, &
        present_expwidth, present_sign, sign_
    integer :: mode_, decimals_, expwidth_, &
        width_real, width_imag, extra_width_real, extra_width_imag
    real(_KIND) :: real_part, imag_part
    character(len=2) :: mode_str
    character(len=MAX_LEN_INTEGER_STR) :: decimals_str, expwidth_str, &
        width_real_str, width_imag_str
    character(len=3) :: sign_str
    character(len=_(MAX_LEN_COMPLEX_)_(_KIND_LABEL)_(_STR)) :: buffer

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
        real_part = real(arg)
        imag_part = aimag(arg)
        if (present_mode) then
            mode_ = mode
        else
            mode_ = MODE_GENERAL
        end if
        select case (mode_)
        case (MODE_FIXED)
            mode_str = 'f'
            present_expwidth = .false.
            extra_width_real = FIXED_EXTRA_WIDTH &
                + max(0, dec_exponent(real_part))
            extra_width_imag = FIXED_EXTRA_WIDTH &
                + max(0, dec_exponent(imag_part))
        case (MODE_EXPONENTIAL)
            mode_str = 'e'
            extra_width_real = EXPONENTIAL_EXTRA_WIDTH
            extra_width_imag = EXPONENTIAL_EXTRA_WIDTH
        case (MODE_SCIENTIFIC)
            mode_str = 'es'
            extra_width_real = SCIENTIFIC_EXTRA_WIDTH
            extra_width_imag = SCIENTIFIC_EXTRA_WIDTH
        case (MODE_ENGINEERING)
            mode_str = 'en'
            extra_width_real = SCIENTIFIC_EXTRA_WIDTH &
                + modulo(dec_exponent(real_part), 3)
            extra_width_imag = SCIENTIFIC_EXTRA_WIDTH &
                + modulo(dec_exponent(imag_part), 3)
        case (MODE_EXPONENTIAL_D)
            mode_str = 'd'
            extra_width_real = EXPONENTIAL_EXTRA_WIDTH
            extra_width_imag = EXPONENTIAL_EXTRA_WIDTH
        case default
            mode_str = 'g'
            extra_width_real = EXPONENTIAL_EXTRA_WIDTH
            extra_width_imag = EXPONENTIAL_EXTRA_WIDTH
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
            width_real = max(1, width)
            width_imag = width_real
        else
            width_real = decimals_ + extra_width_real
            width_imag = decimals_ + extra_width_imag
            if (mode /= MODE_FIXED) then
                width_real = width_real + expwidth_
                width_imag = width_real
            end if
            if (real_part < 0 .or. sign_) width_real = width_real + 1
            if (imag_part < 0 .or. sign_) width_imag = width_imag + 1
        end if
        write(width_real_str, *) width_real
        write(width_imag_str, *) width_imag
        allocate(character(len=( &
            width_real + width_imag + COMPLEX_EXTRA_WIDTH &
        )) :: res)
        write( &
            res, &
            '(' &
            // trim(sign_str) &
            // '"("' &
            // trim(mode_str) &
            // trim(adjustl(width_real_str)) &
            // '.' &
            // trim(adjustl(decimals_str)) &
            // trim(expwidth_str) &
            // '","' &
            // trim(mode_str) &
            // trim(adjustl(width_imag_str)) &
            // '.' &
            // trim(adjustl(decimals_str)) &
            // trim(expwidth_str) &
            // '")"' &
            // ')' &
        ) arg
    else
        write(buffer, *) arg
        res = trim(adjustl(buffer))
    end if
end function _PROC
#undef _OP
