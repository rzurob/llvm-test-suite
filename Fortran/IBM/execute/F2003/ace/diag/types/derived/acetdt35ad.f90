!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : undefined variable in derived type AC in assignment stmt
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : AC, assignment, undefined variable
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that the compiler correctly handles undefined variables in AC's in
!*  an assignment statement.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetdt35ad

  implicit none
  type ADerived
     real :: rpfield
  end type ADerived
  type (ADerived) :: t(1)
  integer :: j

  t = [(ADerived(rt2), j=1,1)]

end program acetdt35ad
