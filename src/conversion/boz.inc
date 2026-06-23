#undef _NUM
#undef _NUM_STR
#if _TYPE_ID == _COMPLEX
#  define _NUM 2
#  define _NUM_STR "2"
#else
#  define _NUM 1
#  define _NUM_STR "1"
#endif


#define _PROC _UNARY_OP(bin)
elemental function _PROC(number) result(res)
    _TYPE(_KIND), intent(in) :: number
    character(len=(_NUM * storage_size(number))) :: res

    character(len=MAX_LEN_INTEGER_STR) :: width

    write(width, *) len(res)
    write( &
        res, "(" // _NUM_STR // "b" // width // "." // width // ")" &
    ) number
end function _PROC
#undef _PROC


#define _PROC _UNARY_OP(oct)
elemental function _PROC(number) result(res)
    _TYPE(_KIND), intent(in) :: number
    character(len=(_NUM * storage_size(number) / 4)) :: res

    character(len=MAX_LEN_INTEGER_STR) :: width

    write(width, *) len(res)
    write( &
        res, "(" // _NUM_STR // "o" // width // "." // width // ")" &
    ) number
end function _PROC
#undef _PROC


#define _PROC _UNARY_OP(hex)
elemental function _PROC(number) result(res)
    _TYPE(_KIND), intent(in) :: number
    character(len=(_NUM * storage_size(number) / 8)) :: res

    character(len=MAX_LEN_INTEGER_STR) :: width

    write(width, *) len(res)
    write( &
        res, "(" // _NUM_STR // "z" // width // "." // width // ")" &
    ) number
end function _PROC
#undef _PROC
