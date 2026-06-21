#include "../inc/init.inc"
#include "../inc/utils.inc"


module pytran_builtins
    implicit none

    private

    logical, parameter, public :: &
        False = .false., &
        True = .true.

#define _PROC _CAT3(_LABEL1,_OP_NAME,_LABEL2)
#define _TYPE_IDS1 _LOGICAL
#define _TYPE_IDS2 _LOGICAL
#define _OP operator(==)
#define _OP_NAME eq
#include "../inc/iface.inc"
#define _OP operator(/=)
#define _OP_NAME ne
#include "../inc/iface.inc"
#undef _TYPE_IDS1
#undef _TYPE_IDS2
#undef _PROC

#define _PROC _CAT2(_OP,_LABEL)
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
