#define _PROC _CAT2(character,_LABEL)
pure function _PROC(arg, mode, width, decimals, expwidth, sign) result(res)
    _TYPE(_KIND), intent(in) :: arg
    integer, intent(in), optional :: mode, width, decimals, expwidth
    logical, intent(in), optional :: sign
    character(len=:), allocatable :: res

    logical :: present_mode, present_width, present_decimals, &
        present_expwidth, present_sign, sign_
    integer :: mode_, decimals_, expwidth_, width_real, width_imag
    real(_KIND) :: real_part, imag_part
    character(len=:), allocatable :: fmt, fmt_real, fmt_imag
    character(len=_CAT3(MAX_LEN_COMPLEX,_KIND_LABEL,STR)) :: buf

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
        fmt = "("
        if (present_sign) then
            sign_ = sign
        else
            sign_ = .false.
        end if
        if (sign_) fmt = fmt // "sp,"
        if (present_mode) then
            mode_ = mode
        else
            mode_ = MODE_GENERAL
        end if
        select case (mode_)
        case (MODE_FIXED)
            fmt_real = "f"
            fmt_imag = "f"
            present_expwidth = .false.
            width_real = FIXED_EXTRA_WIDTH + max(0, es_exponent(real_part))
            width_imag = FIXED_EXTRA_WIDTH + max(0, es_exponent(imag_part))
        case (MODE_EXPONENTIAL)
            fmt_real = "e"
            fmt_imag = "e"
            width_real = EXPONENTIAL_EXTRA_WIDTH
            width_imag = EXPONENTIAL_EXTRA_WIDTH
        case (MODE_SCIENTIFIC)
            fmt_real = "es"
            fmt_imag = "es"
            width_real = SCIENTIFIC_EXTRA_WIDTH
            width_imag = SCIENTIFIC_EXTRA_WIDTH
        case (MODE_ENGINEERING)
            fmt_real = "en"
            fmt_imag = "en"
            width_real = ( &
                SCIENTIFIC_EXTRA_WIDTH + modulo(es_exponent(real_part), 3) &
            )
            width_imag = ( &
                SCIENTIFIC_EXTRA_WIDTH + modulo(es_exponent(imag_part), 3) &
            )
        case (MODE_EXPONENTIAL_D)
            fmt_real = "d"
            fmt_imag = "d"
            width_real = EXPONENTIAL_EXTRA_WIDTH
            width_imag = EXPONENTIAL_EXTRA_WIDTH
        case default
            fmt_real = "g"
            fmt_imag = "g"
            width_real = EXPONENTIAL_EXTRA_WIDTH
            width_imag = EXPONENTIAL_EXTRA_WIDTH
        end select
        if (present_decimals) then
            decimals_ = max(0, decimals)
        else
            decimals_ = _CAT3(REAL,_KIND_LABEL,DEC_DIGITS)
        end if
        if (present_expwidth) then
            expwidth_ = max(1, expwidth)
        else
            expwidth_ = _CAT3(REAL,_KIND_LABEL,EXP_WIDTH)
        end if
        if (present_width) then
            width_real = max(1, width)
            width_imag = width_real
        else
            width_real = width_real + decimals_
            width_imag = width_imag + decimals_
            if (mode /= MODE_FIXED) then
                width_real = width_real + expwidth_
                width_imag = width_imag + expwidth_
            end if
            if (real_part < 0 .or. sign_) width_real = width_real + 1
            if (imag_part < 0 .or. sign_) width_imag = width_imag + 1
        end if
        fmt_real = fmt_real // str(width_real) // "." // str(decimals_)
        fmt_imag = fmt_imag // str(width_imag) // "." // str(decimals_)
        if (present_expwidth) then
            fmt_real = fmt_real // "e" // str(expwidth_)
            fmt_imag = fmt_imag // "e" // str(expwidth_)
        end if
        fmt = fmt // "'('," // fmt_real // "','," // fmt_imag // "')'"
        fmt = fmt // ")"
        allocate(character(len=( &
            width_real + width_imag + COMPLEX_EXTRA_WIDTH &
        )) :: res)
        write(res, fmt) arg
    else
        write(buf, *) arg
        res = trim(adjustl(buf))
    end if
end function _PROC
#undef _PROC
