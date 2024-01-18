!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint54d
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
!*  Create several AC's which abuse enumerated constants, either by assigning
!*  an AC to it, or using it as an ac-do-variable.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint54d

  implicit none

  enum, bind(c)
    enumerator :: ORANGE, APPLE, BANANA
  end enum

  integer(4) :: iarr(3), i

  integer(4) :: iarrAi(3) = [(ORANGE, APPLE=1,3)]
  integer(4) :: iarrBi(3) = [integer(4):: (ORANGE, APPLE=1,3)]

  integer(4), parameter :: iarrApi(3) = [(ORANGE, APPLE=1,3)]
  integer(4), parameter :: iarrBpi(3) = [integer(4):: (ORANGE, APPLE=1,3)]

  print *, ORANGE, APPLE, BANANA

  print *, [(APPLE, ORANGE=1,2)]
  call test([(APPLE, ORANGE=1,2)])
  print *, [integer:: (APPLE, ORANGE=1,2)]
  call test([integer:: (APPLE, ORANGE=1,2)])

  ORANGE = [APPLE]
  ORANGE = [integer:: APPLE]

contains

  subroutine test(arr)
    integer(4) :: arr(:)
    print *, arr
  end subroutine test

end program acetint54d
