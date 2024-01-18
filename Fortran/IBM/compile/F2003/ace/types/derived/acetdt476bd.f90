!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt476bd
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-19
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : C476 (R455) type-name must be accessible derived type
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : array constructor, accessible, derived type
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Constraint C476 on rule R455:
!*     "derived-type-spec is type-name [(type-param-spec-list)]"
!*  requires type-name to be an accessible derived type.  This diagnostic tests
!*  that using an existing private (and therefore inaccessible) derived type
!*  generates a compile error.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module mod

  implicit none

  type, private :: derived
  end type derived

  type (derived), public, save :: dparr(3), dparr0(0), dp1, dp2, dp3

contains

  ! This is just something to hang an array constructor on:
  subroutine test(arr)
    class (*) :: arr(:)
  end subroutine test

end module mod

program acetdt476bd

  use mod
  implicit none
  integer :: i
  class(*), allocatable :: al(:)

  dparr0  = (/ derived:: /)
  dparr0  = (/ derived:: (dp1,i=1,0) /)
  dparr   = (/ derived:: dp1, dp2, dp3 /)

  call test((/ derived:: /))
  call test((/ derived:: dp1 /))

  allocate(al(0), source=(/ derived:: /))
  deallocate(al)
  allocate(al(0), source=(/ derived:: (dp1,i=1,0) /))
  deallocate(al)
  allocate(al(1), source=(/ derived:: (dp1,i=1,1) /))
  deallocate(al)

  ! Also test []:
  call test([ derived:: ])
  call test([ derived:: dp1 ])
  allocate(al(0), source=[ derived:: (dp1,i=1,0) ])
  dparr0  = [ derived:: (dp1,i=1,0) ]

end program acetdt476bd
