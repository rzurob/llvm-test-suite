! *********************************************************************
!* ===================================================================
!*
!* DATE                         : October 27, 2013
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Assumed-rank entities not allowed as actual argument of SIZEOF
!*
!* Actual Argument:
!*
!* Dummy Argument:
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine sub(arr)
    use ISO_C_BINDING
    implicit none
    integer :: arr(..)

    print*,  c_sizeof(arr)
    print*,  sizeof(arr)
end subroutine sub
