!**********************************************************************
!* ====================================================================
!*
!*  DATE                       : 2008-06-03
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: ACE: DIAG: Duplicate Diagnostics
!*                               Emitted
!*
!*  DESCRIPTION                :
!*  This is a minor issue.
!*
!*  While working on changes for Defect: d346022.test, I noticed that we
!*  emit the same Diagnostic for the same line twice 1 character apart
!*  (refer to Line 19 below).
!*
!*  A similar situation occurs on Line 20, however in this case we emit
!*  the Diagnostic only once.
!*
!*  I've updated the Verification file "acetdt476bdl.vf" to assume we'll
!*  emit the Diagnostic in a similar position to that used with Line 20.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  implicit none

  type, private :: derived(l1)    ! (20)
      integer, len :: l1
  end type derived

  type (derived(20)), public, save :: dparr0(0), dp1

end module mod

program d352028
  use mod
  implicit none

  integer :: i
  class(*), allocatable :: al(:)

  dparr0  = (/ derived(20):: (dp1,i=1,0) /)     ! Line 19 (now Line 54)
  allocate(al(0), source=(/ derived(20):: (dp1,i=1,0) /))

  deallocate(al)

  dparr0  = [ derived(20):: (dp1,i=1,0) ]       ! Line 24 (now Line 59)
  allocate(al(0), source=[ derived(20):: (dp1,i=1,0) ])

end program d352028
