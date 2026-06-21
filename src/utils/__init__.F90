#include "../inc/init.inc"
#include "../inc/utils.inc"

#define _PROC _CAT2(_OP,_LABEL)

module pytran_utils
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite

    implicit none

    private

#define _TYPE_IDS _INTEGER
#define _OP dec_digits
#include "../inc/iface.inc"
#undef _TYPE_IDS

#define _TYPE_IDS _REAL
#define _OP dec_exponent
#include "../inc/iface.inc"
#undef _TYPE_IDS

contains

#define _TYPE_IDS _INTEGER
#define _FILE "../utils/dec_digits.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

#define _TYPE_IDS _REAL
#define _FILE "../utils/dec_exponent.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

end module pytran_utils
