! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedRank07d
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : October 27, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    : 
!*
!* DESCRIPTION                  : Assumed-rank entities not allowed as actual argument of SIZEOF 
!*
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
