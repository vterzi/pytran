! Procedures for unit testing


module stdlib_unittest
    use stdlib_kinds

    implicit none

    private
    public :: &
        assert_true, assert_false, &
        assert_equal, assert_not_equal, assert_greater, assert_greater_equal, assert_less, assert_less_equal

#include "../inc/proc.inc"

#define _OP assert_true
    interface _OP
#define _ID _LOGICAL
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#define _OP assert_false
    interface _OP
#define _ID _LOGICAL
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#define _OP assert_equal
    interface _OP
#define _ID _LOGICAL
#include "../inc/decls.inc"
#define _ID _INTEGER
#include "../inc/decls.inc"
#define _ID _REAL
#include "../inc/decls.inc"
#define _ID _COMPLEX
#include "../inc/decls.inc"
#define _ID _CHARACTER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#define _OP assert_not_equal
    interface _OP
#define _ID _LOGICAL
#include "../inc/decls.inc"
#define _ID _INTEGER
#include "../inc/decls.inc"
#define _ID _REAL
#include "../inc/decls.inc"
#define _ID _COMPLEX
#include "../inc/decls.inc"
#define _ID _CHARACTER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#define _OP assert_greater
    interface _OP
#define _ID _INTEGER
#include "../inc/decls.inc"
#define _ID _REAL
#include "../inc/decls.inc"
#define _ID _CHARACTER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#define _OP assert_greater_equal
    interface _OP
#define _ID _INTEGER
#include "../inc/decls.inc"
#define _ID _REAL
#include "../inc/decls.inc"
#define _ID _CHARACTER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#define _OP assert_less
    interface _OP
#define _ID _INTEGER
#include "../inc/decls.inc"
#define _ID _REAL
#include "../inc/decls.inc"
#define _ID _CHARACTER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#define _OP assert_less_equal
    interface _OP
#define _ID _INTEGER
#include "../inc/decls.inc"
#define _ID _REAL
#include "../inc/decls.inc"
#define _ID _CHARACTER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

contains
#define _FILE "../unittest/logical.inc"
#define _ID _LOGICAL
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../unittest/cmp.inc"
#define _ID _LOGICAL
#include "../inc/defs.inc"
#define _ID _INTEGER
#include "../inc/defs.inc"
#define _ID _REAL
#include "../inc/defs.inc"
#define _ID _COMPLEX
#include "../inc/defs.inc"
#define _ID _CHARACTER
#include "../inc/defs.inc"
#undef _FILE
end module stdlib_unittest
