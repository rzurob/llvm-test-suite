! *********************************************************************
!* ===================================================================
!*
!* DATE                         : June  27, 2014
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed Length object
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    : -qdebug=BCASSUMEDLEN (should be removed later)
!*
!* DESCRIPTION                  : type mismatch between dummy and actual arg
!*                                can be captured similar to non-bind(c)
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

interface
  subroutine sub_1(arg1) bind(c)
   character(*)  :: arg1(5)
  end subroutine
end interface

integer :: var(5)

call sub_1(var)

end program
