!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue12.f
!*  DATE            : 2010-12-01
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - C main calls Fortran pure procedures, passing pointer to a C function
!*  - dummy arg is type(c_funptr) with value attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

integer(C_INT) pure function foo (cfuncptr)
    use iso_c_binding
    implicit none
    type(C_FUNPTR), value :: cfuncptr
    cfuncptr = c_null_funptr
    foo = 0
end function

pure subroutine sub (cfuncptr)
    use iso_c_binding
    implicit none
    type(C_FUNPTR), value :: cfuncptr
    cfuncptr = c_null_funptr
end subroutine

