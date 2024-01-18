! *********************************************************************
!* ===================================================================
!*
!* DATE                         : October 27, 2013
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Assumed-rank entities not allowed as actual argument of LOC and SIZEOF
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
subroutine sub(ipac, ita)
    use ISO_C_BINDING
    implicit none
    integer, contiguous, pointer :: ipac(..)
    integer, target              :: ita (..)
    type(C_PTR)                  :: caddr
    integer                      :: laddr

    ipac => ita

    caddr = C_LOC(ita)
    laddr = LOC(ipac)
    laddr = LOC(ita)
end subroutine sub
