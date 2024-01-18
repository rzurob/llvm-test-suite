!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt35b1d
!*
!*  DATE                       : 2007-09-07 (from original 2006-11-13)
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : undefined variable in derived type AC in print stmt
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : AC, print, undefined variable
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that the compiler correctly handles undefined variables in AC's in a
!*  print statement.  (Identical to acetdt35bd, with the AC-IMP-DO removed.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetdt35b1d

  implicit none
  type ADerived
     real :: rpfield
  end type ADerived
  type (ADerived) :: t(1)

  print *, [ADerived(rt2)]

end program acetdt35b1d
