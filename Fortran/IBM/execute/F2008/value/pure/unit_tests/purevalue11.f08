!******************************************************************************
!*  ===========================================================================
!*
!*  DATE            : 2010-12-01
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

