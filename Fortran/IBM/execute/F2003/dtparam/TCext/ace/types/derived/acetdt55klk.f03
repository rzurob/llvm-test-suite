!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-22 (original: 2006-11-23)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array
!*                               Constructor Enhancements
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement ALLOCATE of
!*                               polymorphic entity with AC SOURCE
!*
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
!*  Allocate two sorts of polymorphic entity: unlimited polymorphic and
!*  child/parent polymorphic; attempt several allocations, including
!*  ac-implied-do in source.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt55mod

  implicit none

  type parent (kparent_1,lparent_1) ! kparent_1,lparent_1=4,1
     integer, kind :: kparent_1
     integer, len :: lparent_1
     integer(kparent_1) :: ival
  end type parent

  type, extends (parent) :: child1 (kchild1_1) ! kchild1_1=1
     integer, kind :: kchild1_1
     character(kchild1_1) :: cval
  end type

  type, extends (parent) :: child2
     logical(4) :: lval
  end type

end module acetdt55mod


program acetdt55klk

  use acetdt55mod
  implicit none

  class (parent(4,1)), allocatable :: parr(:) ! tcx: (4,1)
  class (*), allocatable      :: uarr(:)
  integer :: i


  allocate(parr(2), source=[parent(4,1):: parent(4,1)(1), parent(4,1)(2)]) ! tcx: (4,1) ! tcx: (4,1) ! tcx: (4,1)
  call printit(parr)
  deallocate(parr)

  allocate(parr(3), source=[child1(4,1,1):: child1(4,1,1)(1,'a'), child1(4,1,1)(2,'b'), child1(4,1,1)(3,'c')]) ! tcx: (4,1,1) ! tcx: (4,1,1) ! tcx: (4,1,1) ! tcx: (4,1,1)
  call printit(parr)
  deallocate(parr)

  allocate(parr(2), source=[child2(4,1):: child2(4,1)(1,.true.), child2(4,1)(2,.false.)]) ! tcx: (4,1) ! tcx: (4,1) ! tcx: (4,1)
  call printit(parr)
  deallocate(parr)

  allocate(parr(4), source=[parent(4,1):: (parent(4,1)(i), i=5,8)]) ! tcx: (4,1) ! tcx: (4,1)
  call printit(parr)
  deallocate(parr)

  allocate(parr(5), source=[child1(4,1,1):: (child1(4,1,1)(i,char(64+i)), i=9,13)]) ! tcx: (4,1,1) ! tcx: (4,1,1)
  call printit(parr)
  deallocate(parr)

  allocate(parr(3), source=[child2(4,1):: (child2(4,1)(i,i==16), i=15,17)]) ! tcx: (4,1) ! tcx: (4,1)
  call printit(parr)
  deallocate(parr)

  ! Now try unlimited polymorphic:
  allocate(uarr(2), source=[parent(4,1):: parent(4,1)(1), parent(4,1)(2)]) ! tcx: (4,1) ! tcx: (4,1) ! tcx: (4,1)
  call printit(uarr)
  deallocate(uarr)

  allocate(uarr(3), source=[child1(4,1,1):: child1(4,1,1)(1,'a'), child1(4,1,1)(2,'b'), child1(4,1,1)(3,'c')]) ! tcx: (4,1,1) ! tcx: (4,1,1) ! tcx: (4,1,1) ! tcx: (4,1,1)
  call printit(uarr)
  deallocate(uarr)

  allocate(uarr(2), source=[child2(4,1):: child2(4,1)(1,.true.), child2(4,1)(2,.false.)]) ! tcx: (4,1) ! tcx: (4,1) ! tcx: (4,1)
  call printit(uarr)
  deallocate(uarr)

  allocate(uarr(4), source=[parent(4,1):: (parent(4,1)(i), i=5,8)]) ! tcx: (4,1) ! tcx: (4,1)
  call printit(uarr)
  deallocate(uarr)

  allocate(uarr(5), source=[child1(4,1,1):: (child1(4,1,1)(i,char(64+i)), i=9,13)]) ! tcx: (4,1,1) ! tcx: (4,1,1)
  call printit(uarr)
  deallocate(uarr)

  allocate(uarr(3), source=[child2(4,1):: (child2(4,1)(i,i==16), i=15,17)]) ! tcx: (4,1) ! tcx: (4,1)
  call printit(uarr)
  deallocate(uarr)

  ! and round it out with an intrinsic:
  allocate(uarr(12), source=[integer:: (i, i=22,33)])
  call printit(uarr)
  deallocate(uarr)

contains

  subroutine printit(arg)
    class (*) :: arg(:)
    select type (obj => arg)
    type is (child1(4,*,1));       print *, 'child1:', obj ! tcx: (4,*,1)
    type is (child2(4,*));       print *, 'child2:', obj ! tcx: (4,*)
    type is (parent(4,*));       print *, 'parent:', obj ! tcx: (4,*)
    type is (integer);      print *, 'integer:', obj
    type is (character(*)); print *, 'character:', obj
    class default;          print *, 'unknown class'
    end select
  end subroutine printit

end program acetdt55klk


! Extensions to introduce derived type parameters:
! type: parent - added parameters (kparent_1,lparent_1) to invoke with (4,1)/declare with (4,1) - 12 changes
! type: child1 - added parameters (kchild1_1) to invoke with (4,1,1)/declare with (4,1,1) - 13 changes
! type: child2 - added parameters () to invoke with (4,1)/declare with (4,*) - 11 changes
