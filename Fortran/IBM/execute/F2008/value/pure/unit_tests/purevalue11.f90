!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue11.f
!*  TEST CASE TITLE : F2008: VALUE attr allowed for dummy args of PURE proc
!*  PROGRAMMER      : Gaby Baghdadi
!*  DATE            : 2010-12-01
!*  ORIGIN          : XL Fortran Compiler Development, IBM Torolab
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - C main calls Fortran pure procedures, passing a int*
!*  - dummy arg is type(c_ptr) with value attribute
!*  - procedures attempts to modify target in C's pointer
!*  - C main verifies its value remains unchanged
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

integer(C_INT) pure function foo ( cptr )
    use iso_c_binding
    implicit none
    type(C_PTR), value :: cptr
    integer, pointer :: fptr
    call c_f_pointer(cptr,fptr)
    fptr = 456
    foo = 0
end function

pure subroutine sub ( cptr )
    use iso_c_binding
    implicit none
    type(C_PTR), value :: cptr
    integer, pointer :: fptr
    call c_f_pointer(cptr,fptr)
    fptr = 789
end subroutine

