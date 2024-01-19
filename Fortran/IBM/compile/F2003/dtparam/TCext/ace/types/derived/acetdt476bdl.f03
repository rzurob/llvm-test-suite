! GM DTP extension using:
! ftcx_dtp -qnok -ql /tstdev/F2003/ace/types/derived/acetdt476bd.f

!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-17 (original: 2006-07-19)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : array constructor, accessible, derived type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Constraint C476 on rule R455:
!*  "derived-type-spec is type-name [(type-param-spec-list)]"
!*  requires type-name to be an accessible derived type.  This diagnostic tests
!*  that using an existing private (and therefore inaccessible) derived type
!*  generates a compile error.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  implicit none

  type, private :: derived(l1)    ! (20)
      integer, len :: l1
  end type derived

  type (derived(20)), public, save :: dparr(3), dparr0(0), dp1, dp2, dp3

contains

  ! This is just something to hang an array constructor on:
  subroutine test(arr)
    class (*) :: arr(:)
  end subroutine test

end module mod

program acetdt476bdl

  use mod
  implicit none
  integer :: i
  class(*), allocatable :: al(:)

  dparr0  = (/ derived(20):: /)
  dparr0  = (/ derived(20):: (dp1,i=1,0) /)
  dparr   = (/ derived(20):: dp1, dp2, dp3 /)

  call test((/ derived(20):: /))
  call test((/ derived(20):: dp1 /))

  allocate(al(0), source=(/ derived(20):: /))
  deallocate(al)
  allocate(al(0), source=(/ derived(20):: (dp1,i=1,0) /))
  deallocate(al)
  allocate(al(1), source=(/ derived(20):: (dp1,i=1,1) /))
  deallocate(al)

  ! Also test []:
  call test([ derived(20):: ])
  call test([ derived(20):: dp1 ])
  allocate(al(0), source=[ derived(20):: (dp1,i=1,0) ])
  dparr0  = [ derived(20):: (dp1,i=1,0) ]

end program acetdt476bdl
