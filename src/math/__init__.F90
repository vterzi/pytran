#include "../inc/init.inc"
#include "../inc/utils.inc"

module pytran_math
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan
    use :: pytran_kinds
    use :: pytran_number

    implicit none

    private

#define _PROC _UNARY_OP(_OP)

#define _TYPE_IDS _REAL
#define _OP inf
#include "../inc/iface.inc"
#define _OP nan
#include "../inc/iface.inc"
#undef _TYPE_IDS

#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL)
#define _OP isfinite
#include "../inc/iface.inc"
#define _OP isnan
#include "../inc/iface.inc"
#define _OP isinf
#include "../inc/iface.inc"
#undef _TYPE_IDS

#define _TYPE_IDS _INTEGER
#define _OP gcd
#include "../inc/iface.inc"
#undef _TYPE_IDS

#undef _PROC

contains

#define _TYPE_IDS _REAL
#define _FILE "../math/consts.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL)
#define _FILE "../math/ieee.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

#define _TYPE_IDS _INTEGER
#define _FILE "../math/gcd.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

end module pytran_math
