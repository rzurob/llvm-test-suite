!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC in SOURCE of ALLOCATE
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : source, allocate, AC
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Allocate several derived type arrays with an AC as the source, printing to
!*  verify the correctness of the content.  Repeat with implied-do's.
!*  (With intrinsics, we also looked at KIND; DTP's are not covered here, so
!*  we do not experiment with KIND - look in dtparam/ace for that.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module acetdt50mod

  implicit none
  type derived
     integer :: ival
  end type derived

end module acetdt50mod


program acetint50

  use acetdt50mod
  use, intrinsic :: ieee_arithmetic
  implicit none

  type (derived), allocatable :: dtarr(:)

  integer :: i

  allocate (dtarr(3), source=[derived:: derived(5), derived(6), derived(7)])
  print *, dtarr
  deallocate (dtarr)
  allocate (dtarr(3), source=[derived:: (derived(i), i=1,3)])
  print *, dtarr

  ! Try a couple of empty arrays:
  deallocate (dtarr)
  allocate (dtarr(0), source=[derived::])
  print *, dtarr

  deallocate (dtarr)
  allocate (dtarr(0), source=[derived:: (derived(i), i=1,0)])
  print *, dtarr

end program acetint50
