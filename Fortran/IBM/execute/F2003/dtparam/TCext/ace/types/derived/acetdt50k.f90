!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetint50
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-22 (original: 2006-11-16)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array
!*                               Constructor Enhancements
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement AC in SOURCE
!*                               of ALLOCATE
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : source, allocate, AC
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Allocate several derived type arrays with an AC as the source, printing
!*  to verify the correctness of the content.  Repeat with implied-do's.
!*  (With intrinsics, we also looked at KIND; DTP's are not covered here,
!*  so we do not experiment with KIND - look in dtparam/ace for that.)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012


module acetdt50mod

  implicit none
  type derived (kderived_1) ! kderived_1=4
     integer, kind :: kderived_1
     integer(kderived_1) :: ival
  end type derived

end module acetdt50mod


program acetint50

  use acetdt50mod
  use, intrinsic :: ieee_arithmetic
  implicit none

  type (derived(4)), allocatable :: dtarr(:) ! tcx: (4)

  integer :: i

  allocate (dtarr(3), source=[derived(4):: derived(4)(5), derived(4)(6), derived(4)(7)]) ! tcx: (4) ! tcx: (4) ! tcx: (4) ! tcx: (4)
  print *, dtarr
  deallocate (dtarr)
  allocate (dtarr(3), source=[derived(4):: (derived(4)(i), i=1,3)]) ! tcx: (4) ! tcx: (4)
  print *, dtarr

  ! Try a couple of empty arrays:
  deallocate (dtarr)
  allocate (dtarr(0), source=[derived(4)::]) ! tcx: (4)
  print *, dtarr

  deallocate (dtarr)
  allocate (dtarr(0), source=[derived(4):: (derived(4)(i), i=1,0)]) ! tcx: (4) ! tcx: (4)
  print *, dtarr

end program acetint50


! Extensions to introduce derived type parameters:
! type: derived - added parameters (kderived_1) to invoke with (4)/declare with (4) - 10 changes
