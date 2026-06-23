#ifndef _IFACE
#  define _IFACE _OP
#endif
#ifndef _NO_PUBLIC
public :: _IFACE
#endif
interface _IFACE
#define _FILE "../inc/decl.inc"
#include "../inc/types.inc"
#undef _FILE
end interface _IFACE
#undef _IFACE
#undef _OP
#undef _NO_PUBLIC
