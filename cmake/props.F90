program props
    use, intrinsic :: ieee_arithmetic, only: &
        ieee_value, ieee_quiet_nan, ieee_positive_inf

    implicit none

    integer :: n

    print *, "-huge(0)-1=", -huge(0) - 1

    print *, "LOGICAL:"
    print *, "SIZE=", storage_size(.false.) / 8
    print *, "STR=`"
    print *, .false.
    print *, "`"
    print *, "INTEGER:"
    print *, "SIZE=", storage_size(0) / 8
    print *, "STR=`"
    print *, -huge(0) - 1
    print *, "`"
#ifdef _K1
    print *, "LOGICAL_K1:"
    print *, "SIZE=", storage_size(logical(.false., _K1)) / 8
    print *, "STR=`"
    print *, logical(.false., _K1)
    print *, "`"
    print *, "INTEGER_K1:"
    print *, "SIZE=", storage_size(int(0, _K1)) / 8
    print *, "STR=`"
    print *, -huge(int(0, _K1)) - 1
    print *, "`"
#endif
#ifdef _K2
    print *, "LOGICAL_K2:"
    print *, "SIZE=", storage_size(logical(.false., _K2)) / 8
    print *, "STR=`"
    print *, logical(.false., _K2)
    print *, "`"
    print *, "INTEGER_K2:"
    print *, "SIZE=", storage_size(int(0, _K2)) / 8
    print *, "STR=`"
    print *, -huge(int(0, _K2)) - 1
    print *, "`"
#endif
#ifdef _K4
    print *, "LOGICAL_K4:"
    print *, "SIZE=", storage_size(logical(.false., _K4)) / 8
    print *, "STR=`"
    print *, logical(.false., _K4)
    print *, "`"
    print *, "INTEGER_K4:"
    print *, "SIZE=", storage_size(int(0, _K4)) / 8
    print *, "STR=`"
    print *, -huge(int(0, _K4)) - 1
    print *, "`"
#endif
#ifdef _K8
    print *, "LOGICAL_K8:"
    print *, "SIZE=", storage_size(logical(.false., _K8)) / 8
    print *, "STR=`"
    print *, logical(.false., _K8)
    print *, "`"
    print *, "INTEGER_K8:"
    print *, "SIZE=", storage_size(int(0, _K8)) / 8
    print *, "STR=`"
    print *, -huge(int(0, _K8)) - 1
    print *, "`"
#endif
#ifdef _K16
    print *, "LOGICAL_K16:"
    print *, "SIZE=", storage_size(logical(.false., _K16)) / 8
    print *, "STR=`"
    print *, logical(.false., _K16)
    print *, "`"
    print *, "INTEGER_K16:"
    print *, "SIZE=", storage_size(int(0, _K16)) / 8
    print *, "STR=`"
    print *, -huge(int(0, _K16)) - 1
    print *, "`"
#endif

    n = (storage_size(0.) - 1) / storage_size(0) + 1
    print *, "REAL:"
    print *, "SIZE=", storage_size(0.) / 8
    print *, "STR=`"
    print *, -huge(0.)
    print *, "`"
    print *, "INF=`"
    print *, transfer(ieee_value(0., ieee_positive_inf), 0, n)
    print *, "`"
    print *, "NAN=`"
    print *, transfer(ieee_value(0., ieee_quiet_nan), 0, n)
    print *, "`"
    print *, "COMPLEX:"
    print *, "SIZE=", storage_size((0, 0)) / 8
    print *, "STR=`"
    print *, cmplx(-huge(0.), -huge(0.))
    print *, "`"
#ifdef _HP
    n = (storage_size(real(0, _HP)) - 1) / storage_size(0) + 1
    print *, "REAL_HP:"
    print *, "SIZE=", storage_size(real(0, _HP)) / 8
    print *, "STR=`"
    print *, -huge(real(0, _HP))
    print *, "`"
    print *, "INF=`"
    print *, transfer(ieee_value(real(0, _HP), ieee_positive_inf), 0, n)
    print *, "`"
    print *, "NAN=`"
    print *, transfer(ieee_value(real(0, _HP), ieee_quiet_nan), 0, n)
    print *, "`"
    print *, "COMPLEX_HP:"
    print *, "SIZE=", storage_size(cmplx(0, 0, _HP)) / 8
    print *, "STR=`"
    print *, cmplx(-huge(real(0, _HP)), -huge(real(0, _HP)), _HP)
    print *, "`"
#endif
#ifdef _SP
    n = (storage_size(real(0, _SP)) - 1) / storage_size(0) + 1
    print *, "REAL_SP:"
    print *, "SIZE=", storage_size(real(0, _SP)) / 8
    print *, "STR=`"
    print *, -huge(real(0, _SP))
    print *, "`"
    print *, "INF=`"
    print *, transfer(ieee_value(real(0, _SP), ieee_positive_inf), 0, n)
    print *, "`"
    print *, "NAN=`"
    print *, transfer(ieee_value(real(0, _SP), ieee_quiet_nan), 0, n)
    print *, "`"
    print *, "COMPLEX_SP:"
    print *, "SIZE=", storage_size(cmplx(0, 0, _SP)) / 8
    print *, "STR=`"
    print *, cmplx(-huge(real(0, _SP)), -huge(real(0, _SP)), _SP)
    print *, "`"
#endif
#ifdef _DP
    n = (storage_size(real(0, _DP)) - 1) / storage_size(0) + 1
    print *, "REAL_DP:"
    print *, "SIZE=", storage_size(real(0, _DP)) / 8
    print *, "STR=`"
    print *, -huge(real(0, _DP))
    print *, "`"
    print *, "INF=`"
    print *, transfer(ieee_value(real(0, _DP), ieee_positive_inf), 0, n)
    print *, "`"
    print *, "NAN=`"
    print *, transfer(ieee_value(real(0, _DP), ieee_quiet_nan), 0, n)
    print *, "`"
    print *, "COMPLEX_DP:"
    print *, "SIZE=", storage_size(cmplx(0, 0, _DP)) / 8
    print *, "STR=`"
    print *, cmplx(-huge(real(0, _DP)), -huge(real(0, _DP)), _DP)
    print *, "`"
#endif
#ifdef _XDP
    n = (storage_size(real(0, _XDP)) - 1) / storage_size(0) + 1
    print *, "REAL_XDP:"
    print *, "SIZE=", storage_size(real(0, _XDP)) / 8
    print *, "STR=`"
    print *, -huge(real(0, _XDP))
    print *, "`"
    print *, "INF=`"
    print *, transfer(ieee_value(real(0, _XDP), ieee_positive_inf), 0, n)
    print *, "`"
    print *, "NAN=`"
    print *, transfer(ieee_value(real(0, _XDP), ieee_quiet_nan), 0, n)
    print *, "`"
    print *, "COMPLEX_XDP:"
    print *, "SIZE=", storage_size(cmplx(0, 0, _XDP)) / 8
    print *, "STR=`"
    print *, cmplx(-huge(real(0, _XDP)), -huge(real(0, _XDP)), _XDP)
    print *, "`"
#endif
#ifdef _QP
    n = (storage_size(real(0, _QP)) - 1) / storage_size(0) + 1
    print *, "REAL_QP:"
    print *, "SIZE=", storage_size(real(0, _QP)) / 8
    print *, "STR=`"
    print *, -huge(real(0, _QP))
    print *, "`"
    print *, "INF=`"
    print *, transfer(ieee_value(real(0, _QP), ieee_positive_inf), 0, n)
    print *, "`"
    print *, "NAN=`"
    print *, transfer(ieee_value(real(0, _QP), ieee_quiet_nan), 0, n)
    print *, "`"
    print *, "COMPLEX_QP:"
    print *, "SIZE=", storage_size(cmplx(0, 0, _QP)) / 8
    print *, "STR=`"
    print *, cmplx(-huge(real(0, _QP)), -huge(real(0, _QP)), _QP)
    print *, "`"
#endif
end program props
