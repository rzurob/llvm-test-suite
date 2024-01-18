! GM DTP extension using:
! ftcx_dtp -qck -qnol -qreuse=base /tstdev/F2003/ace/diag/types/derived/acetdt55d.f

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt55dc_rb
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt55d
!*                               by David Forster)
!*  DATE                       : 2007-11-30 (original: 2006-11-23)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
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
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt55dc_rbmod

  implicit none

  type parent(k1)    ! (4)
     integer, kind :: k1
     integer(k1)   :: ival
  end type parent

  type, extends (parent) :: child1(k2,l1)    ! (4,1,1)
     integer, kind             :: k2
     integer, len              :: l1
     character(kind=k2,len=l1) :: cval
  end type

  type, extends (parent) :: child2    ! (4)
     logical(k1) :: lval
  end type

end module acetdt55dc_rbmod


program acetdt55dc_rb

  use acetdt55dc_rbmod
  implicit none

  class (parent(4)), allocatable :: parr(:)
  class (*), allocatable      :: uarr(:)
  integer :: i


  ! Baseline: these are okay:
  allocate(parr(2), source=[parent:: parent(4)(1), parent(4)(2)])
  deallocate(parr)

  allocate(parr(2), source=[child1:: child1(4,1,1)(1,'a'), child1(4,1,1)(2,'b')])
  deallocate(parr)

  allocate(parr(2), source=[child2:: child2(4)(1,.true.), child2(4)(2,.false.)])
  deallocate(parr)

  allocate(uarr(2), source=[child2:: child2(4)(1,.true.), child2(4)(2,.false.)])
  deallocate(uarr)


  ! Now try mixed parent/child, including in implied-do's (child1 and child2
  ! are derived from parent, but these constructs are not allowed):

  allocate(parr(4), source=[parent:: (child1(4,1,1)(i,'z'), i=15,18)])
  deallocate(parr)

  allocate(parr(2), source=[child1:: child2(4)(1,.true.), child1(4,1,1)(2,'b')])
  deallocate(parr)

  ! not even unlimited polymorphic can be used:
  allocate(uarr(4), source=[parent:: (child1(4,1,1)(i,'z'), i=15,18)])
  deallocate(uarr)

  allocate(uarr(2), source=[child1:: child2(4)(1,.true.), child1(4,1,1)(2,'b')])
  deallocate(uarr)

  ! Try implied-do:
  allocate(parr(5), source=[parent:: (child1(4,1,1)(i,char(64+i)), i=9,13)])
  deallocate(parr)

  allocate(parr(3), source=[parent:: (child2(4)(i,i==16), i=15,17)])
  deallocate(parr)

  allocate(parr(4), source=[parent:: (child1(4,1,1)(2*i,char(64+i)), child2(4)(2*i+1,i==22), i=21,22)])
  deallocate(parr)

  ! Repeat with unlimited polymorphic:
  allocate(uarr(5), source=[parent:: (child1(4,1,1)(i,char(64+i)), i=9,13)])
  deallocate(uarr)

  allocate(uarr(3), source=[parent:: (child2(4)(i,i==16), i=15,17)])
  deallocate(uarr)

  allocate(uarr(4), source=[parent:: (child1(4,1,1)(2*i,char(64+i)), child2(4)(2*i+1,i==22), i=21,22)])
  deallocate(uarr)

end program acetdt55dc_rb
