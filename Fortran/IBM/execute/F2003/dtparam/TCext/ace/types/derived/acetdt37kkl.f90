!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-15 (original: 2006-11-16)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement AC in I/O,
!*                               as output-item, no DTIO
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
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
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt37mod

  implicit none
  type derived (kderived_1,kderived_2,lderived_1) ! kderived_1,kderived_2,lderived_1=8,2,3
     integer, kind :: kderived_1,kderived_2
     integer, len :: lderived_1
     integer(kderived_2)   :: ival
     character(lderived_1) :: cval
     real(kderived_1)      :: rval
  end type derived

end module acetdt37mod


program acetdt37kkl

  use acetdt37mod
  implicit none
  type (derived(8,2,3)) :: dt ! tcx: (8,2,3)
  integer :: i

  dt = derived(8,2,3)(32767, 'abc', 1./3.) ! tcx: (8,2,3)
  print *, dt
  print *, [dt]
  print *, [derived(8,2,3):: dt] ! tcx: (8,2,3)
  print *, [derived(8,2,3):: (dt, i=1,2)] ! tcx: (8,2,3)

  print *, derived(8,2,3)(-32767, 'def', 5.9d0) ! tcx: (8,2,3)
  print *, [derived(8,2,3)(-32767, 'def', 5.9d0)] ! tcx: (8,2,3)
  print *, [derived(8,2,3):: derived(8,2,3)(-32767, 'def', 5.9d0)] ! tcx: (8,2,3) ! tcx: (8,2,3)
  print *, [derived(8,2,3):: (derived(8,2,3)(merge(-32767,0,i==1), repeat(achar(72+i),3), 5.9d0 + i - 1), i=1,2)] ! tcx: (8,2,3) ! tcx: (8,2,3)

end program acetdt37kkl


! Extensions to introduce derived type parameters:
! type: derived - added parameters (kderived_1,kderived_2,lderived_1) to invoke with (8,2,3)/declare with (8,2,*) - 10 changes
