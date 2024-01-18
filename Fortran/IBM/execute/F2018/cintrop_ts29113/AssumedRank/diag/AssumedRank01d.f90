! *********************************************************************
!* ===================================================================
!*
!* DATE                         : August  25, 2013
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank object
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Assumed rank object can only be a dummy argument
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
real :: a(..)
call foo(a)
contains
    subroutine foo(b)
    real :: b(..)
    end subroutine
end program
