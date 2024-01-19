!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : using used modules
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Several levels of module are introduced and USEd by other modules, sometimes
!*  with ONLY and/or with renaming: the "base" module introduces types used in
!*  the "utility" module, which introduces types to be used in two more
!*  specialist modules ("geometry" and "presentation"), both of which are used
!*  in the main program, together with the utility and basic modules.  There is
!*  overlap in the names of types used.
!*  This is a variant of dtpUseChain02 which uses arrays instead of lists.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module base

  type string(l)
     integer, len :: l
     character(l) :: datum
  end type string

end module base


module utility

  use :: base, only: zwirn => string

  type strings(l,n)
     integer, len :: l, n
     type(zwirn(l)) :: datum(n)
  end type strings

  type integers(k,n)
     integer, kind :: k
     integer, len :: n
     integer(k) :: datum(n)
  end type integers

end module utility


module geometry

  use :: base, only: string
  use :: utility, only: strings, integers

  type points(k,l,n)
     integer, kind :: k
     integer, len :: l, n
     type(strings(l,n)) :: details
     type(integers(k,n)) :: pos(3)
  end type points

end module geometry


module presentation

  use :: base, only: string
  use :: utility, only: strings

  type points(l,n)
     integer, len :: l, n
     type(strings(l,n)) :: facts
  end type points

end module presentation


program dtpUseChain02a

  use base
  use utility
  use :: presentation, only: points => points
  use :: geometry, only: positions => points

  type(integers(8,1))    :: i8root
  type(string(4))        :: s4
  type(strings(7,2))     :: sl
  type(positions(2,5,2)) :: pos
  type(points(7,3))      :: pts

  i8root = integers(8,1)(-999)
  ! We don't want to test I/O procedures for these, yet:
  print *, i8root % datum

  s4  = string(4)('kangaroo')
  sl  = strings(7,2)([string(7)('jumping'),string(7)('kangaroo')])

  print *, sl % datum

  pos = positions(2,5,2)(strings(5,2)([string(5)('jumping'),string(5)('kangaroo')]), [integers(2,2)([1,4]), integers(2,2)([2,5]), integers(2,2)([3,6])])

  print *, pos

  pts = points(7,3)(strings(7,3)([string(7)('wombat'), string(7)('kangaroo'), string(7)('jumping')]))

  print *, pts

end program dtpUseChain02a
