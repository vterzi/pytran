#include "../inc/init.inc"
#include "../inc/utils.inc"

module pytran_math
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan
    use :: pytran_kinds

    implicit none

    private

#ifdef _HP
    real(_HP), parameter, public :: &
        INF_HP = _REAL_HP_INF, &
        NAN_HP = _REAL_HP_NAN
#endif
#ifdef _SP
    real(_SP), parameter, public :: &
        INF_SP = _REAL_SP_INF, &
        NAN_SP = _REAL_SP_NAN
#endif
#ifdef _DP
    real(_DP), parameter, public :: &
        INF_DP = _REAL_DP_INF, &
        NAN_DP = _REAL_DP_NAN
#endif
#ifdef _XDP
    real(_XDP), parameter, public :: &
        INF_XDP = _REAL_XDP_INF, &
        NAN_XDP = _REAL_XDP_NAN
#endif
#ifdef _QP
    real(_QP), parameter, public :: &
        INF_QP = _REAL_QP_INF, &
        NAN_QP = _REAL_QP_NAN
#endif

#define _PROC _CAT2(_OP,_LABEL)

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
