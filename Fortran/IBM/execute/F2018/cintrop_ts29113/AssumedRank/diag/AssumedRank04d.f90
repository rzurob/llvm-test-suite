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
!* DESCRIPTION                  : An assumed-rank entity shall be a dummy
!*                                variable that does not have the CODIMENSION
!*                                or VALUE attribute (C535a)
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
subroutine sub(a)
    implicit none
    integer, value :: a(..)

end subroutine sub
