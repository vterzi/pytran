procedure, private :: &
#ifdef _ARG3
        _(_PREFIX)_(_ARG3), &
#endif
        _(_PREFIX)_(_ARG2), &
        _(_PREFIX)_(_ARG1)
generic :: _GENERIC_NAME => &
#ifdef _ARG3
        _(_PREFIX)_(_ARG3), &
#endif
        _(_PREFIX)_(_ARG2), &
        _(_PREFIX)_(_ARG1)


#undef _PREFIX
#undef _GENERIC_NAME
#undef _ARG1
#undef _ARG2
#undef _ARG3
