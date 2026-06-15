#if ( \
    (defined(_TYPE_ID1) && defined(_TYPE_ID2)) \
    && !(defined(_KIND1) && defined(_KIND2)) \
)
#  define _SELF "../inc/kinds.inc"
#  define _KIND2 _KIND
#  define _KIND_LABEL2 _KIND_LABEL
#  if (_TYPE_ID1 == _LOGICAL) || (_TYPE_ID1 == _INTEGER)
#    ifdef _K1
#      define _KIND1 _K1
#      define _KIND_LABEL1 K1
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#    ifdef _K2
#      define _KIND1 _K2
#      define _KIND_LABEL1 K2
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#    ifdef _K4
#      define _KIND1 _K4
#      define _KIND_LABEL1 K4
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#    ifdef _K8
#      define _KIND1 _K8
#      define _KIND_LABEL1 K8
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#    ifdef _K16
#      define _KIND1 _K16
#      define _KIND_LABEL1 K16
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#  elif (_TYPE_ID1 == _REAL) || (_TYPE_ID1 == _COMPLEX)
#    ifdef _HP
#      define _KIND1 _HP
#      define _KIND_LABEL1 HP
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#    ifdef _SP
#      define _KIND1 _SP
#      define _KIND_LABEL1 SP
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#    ifdef _DP
#      define _KIND1 _DP
#      define _KIND_LABEL1 DP
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#    ifdef _XDP
#      define _KIND1 _XDP
#      define _KIND_LABEL1 XDP
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#    ifdef _QP
#      define _KIND1 _QP
#      define _KIND_LABEL1 QP
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#  elif _TYPE_ID1 == _CHARACTER
#    ifdef _ASCII
#      define _KIND1 _ASCII
#      define _KIND_LABEL1 A
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#    ifdef _UCS4
#      define _KIND1 _UCS4
#      define _KIND_LABEL1 U
#      include _SELF
#      undef _KIND1
#      undef _KIND_LABEL1
#    endif
#  endif
#  undef _TYPE_ID
#  undef _KIND2
#  undef _KIND_LABEL2
#  undef _SELF
#elif defined(_TYPE_ID)
#  if (_TYPE_ID == _LOGICAL) || (_TYPE_ID == _INTEGER)
#    ifdef _K1
#      define _KIND _K1
#      define _KIND_LABEL K1
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#    ifdef _K2
#      define _KIND _K2
#      define _KIND_LABEL K2
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#    ifdef _K4
#      define _KIND _K4
#      define _KIND_LABEL K4
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#    ifdef _K8
#      define _KIND _K8
#      define _KIND_LABEL K8
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#    ifdef _K16
#      define _KIND _K16
#      define _KIND_LABEL K16
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#  elif (_TYPE_ID == _REAL) || (_TYPE_ID == _COMPLEX)
#    ifdef _HP
#      define _KIND _HP
#      define _KIND_LABEL HP
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#    ifdef _SP
#      define _KIND _SP
#      define _KIND_LABEL SP
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#    ifdef _DP
#      define _KIND _DP
#      define _KIND_LABEL DP
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#    ifdef _XDP
#      define _KIND _XDP
#      define _KIND_LABEL XDP
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#    ifdef _QP
#      define _KIND _QP
#      define _KIND_LABEL QP
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#  elif _TYPE_ID == _CHARACTER
#    ifdef _ASCII
#      define _KIND _ASCII
#      define _KIND_LABEL A
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#    ifdef _UCS4
#      define _KIND _UCS4
#      define _KIND_LABEL U
#      include _FILE
#      undef _KIND
#      undef _KIND_LABEL
#    endif
#  endif
#endif
