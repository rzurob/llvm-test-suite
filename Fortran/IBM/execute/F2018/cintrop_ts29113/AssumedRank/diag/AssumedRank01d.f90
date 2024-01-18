! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedRank01d.f
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : August  25, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank object
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Assumed rank object can only be a dummy argument 
!*
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
