!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC in I/O, as output-item, no DTIO
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : DTIO
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Create a derived type with no user-defined DTIO routine and print instances
!*  of it.  This type should not require a user-defined DTIO routine.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt37mod

  implicit none
  type derived
     integer(2)   :: ival
     character(3) :: cval
     real(8)      :: rval
  end type derived

end module acetdt37mod


program acetdt37

  use acetdt37mod
  implicit none
  type (derived) :: dt
  integer :: i

  dt = derived(32767, 'abc', 1./3.)
  print *, dt
  print *, [dt]
  print *, [derived:: dt]
  print *, [derived:: (dt, i=1,2)]

  print *, derived(-32767, 'def', 5.9d0)
  print *, [derived(-32767, 'def', 5.9d0)]
  print *, [derived:: derived(-32767, 'def', 5.9d0)]
  print *, [derived:: (derived(merge(-32767,0,i==1), repeat(achar(72+i),3), 5.9d0 + i - 1), i=1,2)]

end program acetdt37
