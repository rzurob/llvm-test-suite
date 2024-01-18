!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue12.f
!*  TEST CASE TITLE : F2008: VALUE attr allowed for dummy args of PURE proc
!*  PROGRAMMER      : Gaby Baghdadi
!*  DATE            : 2010-12-01
!*  ORIGIN          : XL Fortran Compiler Development, IBM Torolab
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

