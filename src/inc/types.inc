#if ( \
    (defined(_TYPE_IDS1) && defined(_TYPE_IDS2)) \
    && !(defined(_TYPE_ID1) && defined(_TYPE_ID2)) \
)
#  define _SELF "../inc/types.inc"
#  define _TYPE_IDS _TYPE_IDS2
#  define _TYPE_ID2 _TYPE_ID
#  define _TYPE2 _TYPE
#  define _TYPE_LABEL2 _TYPE_LABEL
#  define _DEFAULT_KIND2 _DEFAULT_KIND
#  if _TYPE_IDS1 & _LOGICAL
#    define _TYPE_ID1 _LOGICAL
#    define _TYPE1 logical
#    define _TYPE_LABEL1 L
#    define _DEFAULT_KIND1 _LK
#    include _SELF
#    undef _TYPE_ID1
#    undef _TYPE1
#    undef _TYPE_LABEL1
#    undef _DEFAULT_KIND1
#  endif
#  if _TYPE_IDS1 & _INTEGER
#    define _TYPE_ID1 _INTEGER
#    define _TYPE1 integer
#    define _TYPE_LABEL1 I
#    define _DEFAULT_KIND1 _IK
#    include _SELF
#    undef _TYPE_ID1
#    undef _TYPE1
#    undef _TYPE_LABEL1
#    undef _DEFAULT_KIND1
#  endif
#  if _TYPE_IDS1 & _REAL
#    define _TYPE_ID1 _REAL
#    define _TYPE1 real
#    define _TYPE_LABEL1 R
#    define _DEFAULT_KIND1 _RK
#    include _SELF
#    undef _TYPE_ID1
#    undef _TYPE1
#    undef _TYPE_LABEL1
#    undef _DEFAULT_KIND1
#  endif
#  if _TYPE_IDS1 & _COMPLEX
#    define _TYPE_ID1 _COMPLEX
#    define _TYPE1 complex
#    define _TYPE_LABEL1 C
#    define _DEFAULT_KIND1 _CK
#    include _SELF
#    undef _TYPE_ID1
#    undef _TYPE1
#    undef _TYPE_LABEL1
#    undef _DEFAULT_KIND1
#  endif
#  if _TYPE_IDS1 & _CHARACTER
#    define _TYPE_ID1 _CHARACTER
#    define _TYPE1 character
#    define _TYPE_LABEL1 S
#    define _DEFAULT_KIND1 _SK
#    include _SELF
#    undef _TYPE_ID1
#    undef _TYPE1
#    undef _TYPE_LABEL1
#    undef _DEFAULT_KIND1
#  endif
#  undef _TYPE_IDS
#  undef _TYPE_ID2
#  undef _TYPE2
#  undef _TYPE_LABEL2
#  undef _DEFAULT_KIND2
#  undef _SELF
#elif defined(_TYPE_IDS)
#  if _TYPE_IDS & _LOGICAL
#    define _TYPE_ID _LOGICAL
#    define _TYPE logical
#    define _TYPE_LABEL L
#    define _DEFAULT_KIND _LK
#    include "../inc/kinds.inc"
#    undef _TYPE_ID
#    undef _TYPE
#    undef _TYPE_LABEL
#    undef _DEFAULT_KIND
#  endif
#  if _TYPE_IDS & _INTEGER
#    define _TYPE_ID _INTEGER
#    define _TYPE integer
#    define _TYPE_LABEL I
#    define _DEFAULT_KIND _IK
#    include "../inc/kinds.inc"
#    undef _TYPE_ID
#    undef _TYPE
#    undef _TYPE_LABEL
#    undef _DEFAULT_KIND
#  endif
#  if _TYPE_IDS & _REAL
#    define _TYPE_ID _REAL
#    define _TYPE real
#    define _TYPE_LABEL R
#    define _DEFAULT_KIND _RK
#    include "../inc/kinds.inc"
#    undef _TYPE_ID
#    undef _TYPE
#    undef _TYPE_LABEL
#    undef _DEFAULT_KIND
#  endif
#  if _TYPE_IDS & _COMPLEX
#    define _TYPE_ID _COMPLEX
#    define _TYPE complex
#    define _TYPE_LABEL C
#    define _DEFAULT_KIND _CK
#    include "../inc/kinds.inc"
#    undef _TYPE_ID
#    undef _TYPE
#    undef _TYPE_LABEL
#    undef _DEFAULT_KIND
#  endif
#  if _TYPE_IDS & _CHARACTER
#    define _TYPE_ID _CHARACTER
#    define _TYPE character
#    define _TYPE_LABEL S
#    define _DEFAULT_KIND _SK
#    include "../inc/kinds.inc"
#    undef _TYPE_ID
#    undef _TYPE
#    undef _TYPE_LABEL
#    undef _DEFAULT_KIND
#  endif
#endif
