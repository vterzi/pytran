! Kinds of basic data types as parameters


module stdlib_kinds
    implicit none

    private

    ! logical, integer (K = kind)
#ifdef _K1
    integer, parameter, public :: K1 = _K1
#endif
#ifdef _K2
    integer, parameter, public :: K2 = _K2
#endif
#ifdef _K4
    integer, parameter, public :: K4 = _K4
#endif
#ifdef _K8
    integer, parameter, public :: K8 = _K8
#endif
#ifdef _K16
    integer, parameter, public :: K16 = _K16
#endif

    ! real, complex (P = precision)
#ifdef _HP
    ! half
    integer, parameter, public :: HP = _HP
#endif
#ifdef _SP
    ! single
    integer, parameter, public :: SP = _SP
#endif
#ifdef _DP
    ! double
    integer, parameter, public :: DP = _DP
#endif
#ifdef _XDP
    ! extended double
    integer, parameter, public :: XDP = _XDP
#endif
#ifdef _QP
    ! quadruple
    integer, parameter, public :: QP = _QP
#endif

    ! character
#ifdef _ASCII
    integer, parameter, public :: ASCII = _ASCII
#endif
#ifdef _UCS4
    integer, parameter, public :: UCS4 = _UCS4
#endif

    ! default kinds
    integer, parameter, public :: &
        LK = kind(.false.), &  ! logical
        IK = kind(0), &  ! integer
        RK = kind(0.), &  ! real
        CK = kind((0, 0)), &  ! complex
        SK = kind('')  ! character
end module stdlib_kinds
