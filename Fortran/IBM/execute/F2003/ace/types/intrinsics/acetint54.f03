!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : enumerators
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : enumerators
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Create several AC's which have enumerated constants as content.
!*  Test them in assignments, print statements, and subroutine invocations, as
!*  well as in initialisation expressions.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint54

  implicit none

  enum, bind(c)
    enumerator :: ORANGE, APPLE, BANANA
    enumerator :: RED, GOLD=RED+3, BLACK=72
  end enum

  enum, bind(c)
    enumerator :: BIGULATOR=99999, DEBIGULATOR, INTEROCITER
  end enum

  integer(4) :: iarr(3), i

  integer(4) :: iarrA(9) = [ORANGE, APPLE, BANANA, RED, GOLD, BLACK, BIGULATOR, DEBIGULATOR, INTEROCITER]
  integer(4) :: iarrB(9) = [integer(4):: ORANGE, APPLE, BANANA, RED, GOLD, BLACK, BIGULATOR, DEBIGULATOR, INTEROCITER]

  integer(4), parameter :: iarrAp(9) = [ORANGE, APPLE, BANANA, RED, GOLD, BLACK, BIGULATOR, DEBIGULATOR, INTEROCITER]
  integer(4), parameter :: iarrBp(9) = [integer(4):: ORANGE, APPLE, BANANA, RED, GOLD, BLACK, BIGULATOR, DEBIGULATOR, INTEROCITER]

  integer(4) :: iarrAi(9) = [(ORANGE, APPLE, BANANA, RED, GOLD, BLACK, BIGULATOR, DEBIGULATOR, INTEROCITER, i=1,1)]
  integer(4) :: iarrBi(9) = [integer(4):: (ORANGE, APPLE, BANANA, RED, GOLD, BLACK, BIGULATOR, DEBIGULATOR, INTEROCITER, i=1,1)]

  integer(4), parameter :: iarrApi(9) = [(ORANGE, APPLE, BANANA, RED, GOLD, BLACK, BIGULATOR, DEBIGULATOR, INTEROCITER, i=1,1)]
  integer(4), parameter :: iarrBpi(9) = [integer(4):: (ORANGE, APPLE, BANANA, RED, GOLD, BLACK, BIGULATOR, DEBIGULATOR, INTEROCITER, i=1,1)]

  print *, ORANGE, APPLE, BANANA, RED, GOLD, BLACK, BIGULATOR, DEBIGULATOR, INTEROCITER
  print *, iarrA
  print *, iarrB
  print *, iarrAp
  print *, iarrBp
  print *, iarrAi
  print *, iarrBi
  print *, iarrApi
  print *, iarrBpi

  iarr = [ORANGE, APPLE, BANANA]
  call test (iarr)
  print *, iarr, "/", [ORANGE, APPLE, BANANA]

  iarr = [RED, GOLD, BLACK]
  call test([RED, GOLD, BLACK])
  print *, iarr, "/", [RED, GOLD, BLACK]
  iarr = [BIGULATOR, DEBIGULATOR, INTEROCITER]
  call test([BIGULATOR, DEBIGULATOR, INTEROCITER])
  print *, iarr, "/", [BIGULATOR, DEBIGULATOR, INTEROCITER]

  iarr = [integer:: ORANGE, APPLE, BANANA]
  call test([integer:: ORANGE, APPLE, BANANA])
  print *, iarr, "/", [integer:: ORANGE, APPLE, BANANA]
  iarr = [integer:: RED, GOLD, BLACK]
  call test([integer:: RED, GOLD, BLACK])
  print *, iarr, "/", [integer:: RED, GOLD, BLACK]
  iarr = [integer:: BIGULATOR, DEBIGULATOR, INTEROCITER]
  call test([integer:: BIGULATOR, DEBIGULATOR, INTEROCITER])
  print *, iarr, "/", [integer:: BIGULATOR, DEBIGULATOR, INTEROCITER]

  iarr = [integer(8):: ORANGE, APPLE, BANANA]
  call test8([integer(8):: ORANGE, APPLE, BANANA])
  print *, iarr, "/", [integer(8):: ORANGE, APPLE, BANANA]
  iarr = [integer(8):: RED, GOLD, BLACK]
  call test8([integer(8):: RED, GOLD, BLACK])
  print *, iarr, "/", [integer(8):: RED, GOLD, BLACK]
  iarr = [integer(8):: BIGULATOR, DEBIGULATOR, INTEROCITER]
  call test8([integer(8):: BIGULATOR, DEBIGULATOR, INTEROCITER])
  print *, iarr, "/", [integer(8):: BIGULATOR, DEBIGULATOR, INTEROCITER]

  iarr = [integer(8):: (BIGULATOR, ORANGE, BLACK, i=1,1)]
  call test8([integer(8):: (BIGULATOR, ORANGE, BLACK, i=1,1)])
  print *, iarr, "/", [integer(8):: (BIGULATOR, ORANGE, BLACK, i=1,1)]

contains

  subroutine test(arr)
    integer(4) :: arr(:)
    print *, arr
  end subroutine test

  subroutine test8(arr)
    integer(8) :: arr(:)
    print *, arr
  end subroutine test8

end program acetint54
