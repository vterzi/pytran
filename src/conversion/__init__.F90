#include "../inc/init.inc"

#define _(X) X

#define _CAT(X,Y) _(X)_(_)_(Y)
#define _LABEL _(_TYPE_LABEL)_(_KIND_LABEL)
#define _PROC _CAT(_OP,_LABEL)

module pytran_conversion
    implicit none

    private

#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX)
#define _OP bin
#include "../inc/iface.inc"
#define _OP oct
#include "../inc/iface.inc"
#define _OP hex
#include "../inc/iface.inc"
#undef _TYPE_IDS

contains

#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX)
#define _FILE "../conversion/procs.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

end module pytran_conversion
