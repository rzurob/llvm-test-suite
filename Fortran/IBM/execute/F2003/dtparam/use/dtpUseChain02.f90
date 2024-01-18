!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseChain02
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
!*  This differs from dtpUseChain01 in that string_list was moved to the utility
!*  module and the utility module now uses ONLY string from base, but renaming
!*  it to "zwirn".
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

  type string_list
     type(zwirn(:)), allocatable :: datum
     type(string_list), pointer :: next => null()
  end type string_list

  type integer_list(k)
     integer, kind :: k
     integer(k) :: datum
     type(integer_list(k)), pointer :: next => null()
  end type integer_list

end module utility


module geometry

  use :: base, only: string
  use :: utility, only: string_list

  type point_list(k)
     integer, kind :: k
     type(string_list) :: details
     integer(k) :: pos(3)
     type(point_list(k)), pointer :: next => null()
  end type point_list

end module geometry


module presentation

  use :: base, only: string
  use :: utility, only: string_list

  type point_list
     type(string_list) :: points
     type(point_list), pointer :: next => null()
  end type point_list

end module presentation


program dtpUseChain02

  use base
  use utility
  use :: presentation, only: points => point_list
  use :: geometry, only: positions => point_list

  type(integer_list(8)) :: i8root
  type(string(8))       :: s4
  type(string_list)     :: sl
  type(positions(2))    :: pos
  type(points)          :: pts

  i8root = integer_list(8)(1)
  ! We don't want to test I/O procedures for these, yet:
  print *, i8root % datum, associated(i8root%next) ! 1 F

  s4  = string(8)('kangaroo')
  sl  = string_list(string(7)('jumping'))
  allocate(sl%next)
  sl%next%datum = s4

  print *, sl % datum % datum, sl % next % datum % datum, associated(sl % next % next) ! jumpingkangaroo F

  pos % pos = [1,2,3]
  pos % details = sl
  allocate(pos % next)
  pos % next % pos = [4,5,6]
  pos % next % details = string_list(string(6)('wombat'))

  print *, pos % details % datum % datum, pos % details % next % datum % datum, associated(pos % details % next % next), pos % pos, associated(pos %next) ! jumpingkangaroo F 1 2 3 T
  pos = pos % next
  print *, pos % details % datum % datum, associated(pos % details % next), pos % pos, associated(pos % next) ! wombat F 4 5 6 F

  pts % points = sl
  print *, pts % points % datum % datum, pts % points % next % datum % datum, associated(pts % points % next % next), associated(pts % points%next) ! jumpingkangaroo F T

end program dtpUseChain02
