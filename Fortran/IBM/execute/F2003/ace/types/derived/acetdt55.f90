!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt55
!*
!*  DATE                       : 2006-11-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : ALLOCATE of polymorphic entity with AC SOURCE
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Allocate two sorts of polymorphic entity: unlimited polymorphic and
!*  child/parent polymorphic; attempt several allocations, including
!*  ac-implied-do in source.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt55mod

  implicit none

  type parent
     integer :: ival
  end type parent

  type, extends (parent) :: child1
     character :: cval
  end type

  type, extends (parent) :: child2
     logical :: lval
  end type

end module acetdt55mod


program acetdt55

  use acetdt55mod
  implicit none

  class (parent), allocatable :: parr(:)
  class (*), allocatable      :: uarr(:)
  integer :: i


  allocate(parr(2), source=[parent:: parent(1), parent(2)])
  call printit(parr)
  deallocate(parr)

  allocate(parr(3), source=[child1:: child1(1,'a'), child1(2,'b'), child1(3,'c')])
  call printit(parr)
  deallocate(parr)

  allocate(parr(2), source=[child2:: child2(1,.true.), child2(2,.false.)])
  call printit(parr)
  deallocate(parr)

  allocate(parr(4), source=[parent:: (parent(i), i=5,8)])
  call printit(parr)
  deallocate(parr)

  allocate(parr(5), source=[child1:: (child1(i,char(64+i)), i=9,13)])
  call printit(parr)
  deallocate(parr)

  allocate(parr(3), source=[child2:: (child2(i,i==16), i=15,17)])
  call printit(parr)
  deallocate(parr)

  ! Now try unlimited polymorphic:
  allocate(uarr(2), source=[parent:: parent(1), parent(2)])
  call printit(uarr)
  deallocate(uarr)

  allocate(uarr(3), source=[child1:: child1(1,'a'), child1(2,'b'), child1(3,'c')])
  call printit(uarr)
  deallocate(uarr)

  allocate(uarr(2), source=[child2:: child2(1,.true.), child2(2,.false.)])
  call printit(uarr)
  deallocate(uarr)

  allocate(uarr(4), source=[parent:: (parent(i), i=5,8)])
  call printit(uarr)
  deallocate(uarr)

  allocate(uarr(5), source=[child1:: (child1(i,char(64+i)), i=9,13)])
  call printit(uarr)
  deallocate(uarr)

  allocate(uarr(3), source=[child2:: (child2(i,i==16), i=15,17)])
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
    type is (child1);       print *, 'child1:', obj
    type is (child2);       print *, 'child2:', obj
    type is (parent);       print *, 'parent:', obj
    type is (integer);      print *, 'integer:', obj
    type is (character(*)); print *, 'character:', obj
    class default;          print *, 'unknown class'
    end select
  end subroutine printit

end program acetdt55
