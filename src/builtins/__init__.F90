#include "../inc/init.inc"
#include "../inc/utils.inc"


module pytran_builtins
    implicit none

    private

    logical, parameter, public :: &
        False = .false., &
        True = .true.

#define _PROC _BINARY_OP(_OP)
#define _IFACE operator(._OP.)
#define _TYPE_IDS1 _LOGICAL
#define _TYPE_IDS2 _LOGICAL
#define _OP eq
#include "../inc/iface.inc"
#define _OP ne
#include "../inc/iface.inc"
#define _OP lt
#include "../inc/iface.inc"
#define _OP le
#include "../inc/iface.inc"
#define _OP gt
#include "../inc/iface.inc"
#define _OP ge
#include "../inc/iface.inc"
#undef _TYPE_IDS1
#undef _TYPE_IDS2
#undef _PROC

#define _PROC _UNARY_OP(_OP)
#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX | _CHARACTER)
#define _OP assign_optional
#include "../inc/iface.inc"
#define _OP swap
#include "../inc/iface.inc"
#undef _TYPE_IDS
#undef _PROC

contains

#define _TYPE_IDS1 _LOGICAL
#define _TYPE_IDS2 _LOGICAL
#define _FILE "../builtins/cmp.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS1
#undef _TYPE_IDS2

#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX | _CHARACTER)
#define _FILE "../builtins/assign_optional.inc"
#include "../inc/types.inc"
#undef _FILE
#define _FILE "../builtins/swap.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

end module pytran_builtins
