!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt62kk
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-25 (original: 2006-11-26)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement pointer with
!*                               specified lower bound in AC
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Include references to pointers with specified lower bound on both LHS and
!*  RHS of assignment, within AC's.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt62mod

  implicit none
  type dt (kdt_1,kdt_2) ! kdt_1,kdt_2=4,8
     integer, kind :: kdt_1,kdt_2
     real(kdt_1) :: val
  end type dt

end module acetdt62mod


program acetdt62kk

  use acetdt62mod
  implicit none
  type(dt(4,8)), pointer :: a(:), b(:), c(:) ! tcx: (4,8)
  type(dt(4,8)), allocatable, target :: base(:) ! tcx: (4,8)
  integer :: i

  base = [dt(4,8):: (dt(4,8)(real(i/10.0)), i=11,27)] ! 1.1, 1.2, 1.3, ..., 2.7 - 17 items ! tcx: (4,8) ! tcx: (4,8)

  print *, base

  a     => base(5:)       ! Lower bound of a is 1
  b(5:) => base(5:)       ! Lower bound of b is 5
  print *, [dt(4,8):: base(5:9)] ! tcx: (4,8)
  print *, [dt(4,8):: b(5:9)] ! tcx: (4,8)
  print *, [dt(4,8):: a(1:5)] ! tcx: (4,8)
  a = [dt(4,8):: b] ! tcx: (4,8)
  print *, base
  a = [dt(4,8):: b(ubound(b,1):lbound(b,1):-1)] ! tcx: (4,8)
  print *, base
  print *, [dt(4,8):: (a(i),b(i), i=5,12)] ! tcx: (4,8)

  b ([integer:: (int(10*b(i)%val+0.1)-10, i=5,10)]) = a ([integer:: (int(10*a(i)%val+0.1)-14, i=6,1,-1)])
  print *, base

end program acetdt62kk


! Extensions to introduce derived type parameters:
! type: dt - added parameters (kdt_1,kdt_2) to invoke with (4,8)/declare with (4,8) - 10 changes
