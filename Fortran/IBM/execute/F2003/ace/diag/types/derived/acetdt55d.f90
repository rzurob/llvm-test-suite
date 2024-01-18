!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt55d
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-23
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : ALLOCATE of polymorphic entity with AC SOURCE
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Attempt to allocate two sorts of polymorphic entity: unlimited polymorphic
!*  and child/parent polymorphic, but mismatch types.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt55dmod

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

end module acetdt55dmod


program acetdt55d

  use acetdt55dmod
  implicit none

  class (parent), allocatable :: parr(:)
  class (*), allocatable      :: uarr(:)
  integer :: i


  ! Baseline: these are okay:
  allocate(parr(2), source=[parent:: parent(1), parent(2)])
  deallocate(parr)

  allocate(parr(2), source=[child1:: child1(1,'a'), child1(2,'b')])
  deallocate(parr)

  allocate(parr(2), source=[child2:: child2(1,.true.), child2(2,.false.)])
  deallocate(parr)

  allocate(uarr(2), source=[child2:: child2(1,.true.), child2(2,.false.)])
  deallocate(uarr)


  ! Now try mixed parent/child, including in implied-do's (child1 and child2
  ! are derived from parent, but these constructs are not allowed):

  allocate(parr(4), source=[parent:: (child1(i,'z'), i=15,18)])
  deallocate(parr)

  allocate(parr(2), source=[child1:: child2(1,.true.), child1(2,'b')])
  deallocate(parr)

  ! not even unlimited polymorphic can be used:
  allocate(uarr(4), source=[parent:: (child1(i,'z'), i=15,18)])
  deallocate(uarr)

  allocate(uarr(2), source=[child1:: child2(1,.true.), child1(2,'b')])
  deallocate(uarr)

  ! Try implied-do:
  allocate(parr(5), source=[parent:: (child1(i,char(64+i)), i=9,13)])
  deallocate(parr)

  allocate(parr(3), source=[parent:: (child2(i,i==16), i=15,17)])
  deallocate(parr)

  allocate(parr(4), source=[parent:: (child1(2*i,char(64+i)), child2(2*i+1,i==22), i=21,22)])
  deallocate(parr)

  ! Repeat with unlimited polymorphic:
  allocate(uarr(5), source=[parent:: (child1(i,char(64+i)), i=9,13)])
  deallocate(uarr)

  allocate(uarr(3), source=[parent:: (child2(i,i==16), i=15,17)])
  deallocate(uarr)

  allocate(uarr(4), source=[parent:: (child1(2*i,char(64+i)), child2(2*i+1,i==22), i=21,22)])
  deallocate(uarr)

end program acetdt55d
