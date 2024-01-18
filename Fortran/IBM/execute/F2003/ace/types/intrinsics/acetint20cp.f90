!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint20cp
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-09-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier (CHARACTER) in output statement, init expr in kind and len
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : intrinsic type, character
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Derived from acetint20c, this programme just prints the array constructor
!*  directly, testing another path in the compiler.
!*
!*  Change History:
!*  2006-10-10 dforster - Commented out use of pure function in kind parameter in
!*                        what was 'x24' (restricted functions are not allowed in
!*                        initialisation expressions); then renumbered all lines
!*                        following that and altered verification file.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod
contains

  pure integer function sameold(i)
    integer, intent(in) :: i
    sameold = i
  end function sameold

end module mod

program acetint20cp

  use mod
  implicit none

  integer, parameter :: ONE  =  1
  integer, parameter :: TWO  =  2
  integer, parameter :: KIND =  1
  integer, parameter :: LEN  =  1

  integer(1), parameter :: TWO1 = 2
  integer(2), parameter :: TWO2 = 2
  integer(4), parameter :: TWO4 = 2
  integer(8), parameter :: TWO8 = 2

  character(TWO+ONE) :: charr(3)

  print *, 'x 1:', [ character(TWO):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x 2:', (/character(TWO8**ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x 3:', [ character(TWO4-ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x 4:', (/character(TWO2/ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x 5:', [ character((ONE+ONE)/ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x 6:', (/character(ONE*TWO1):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x 7:', [ character(ONE+INT(ONE+SIN(1.5))):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x 8:', (/character(sameold(ONE+INT(ONE+SIN(1.5)))):: 'a1x', 'b2y', 'c3z' /)

  print *, 'x 9:', [ character(LEN=TWO1):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x10:', (/character(LEN=TWO2**ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x11:', [ character(LEN=TWO2-ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x12:', (/character(LEN=TWO4/ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x13:', [ character(LEN=(ONE+ONE)/ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x14:', (/character(LEN=ONE*TWO8):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x15:', [ character(LEN=ONE+INT(ONE+SIN(1.5))):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x16:', (/character(LEN=sameold(ONE+INT(ONE+SIN(1.5)))):: 'a1x', 'b2y', 'c3z' /)

  print *, 'x17:', [ character(KIND=ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x18:', (/character(KIND=ONE**TWO):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x19:', [ character(KIND=TWO-ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x20:', (/character(KIND=TWO/TWO):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x21:', [ character(KIND=(ONE+ONE)/TWO):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x22:', (/character(KIND=ONE*ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x23:', [ character(KIND=ONE+INT(SIN(1.5))):: 'a1x', 'b2y', 'c3z' ]
! print *, 'x24:', (/character(KIND=sameold(ONE+INT(SIN(1.5)))):: 'a1x', 'b2y', 'c3z' /)

  print *, 'x24:', [ character(KIND):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x25:', (/character(LEN):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x26:', [ character(LEN=KIND):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x27:', (/character(LEN=LEN):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x28:', [ character(KIND=KIND):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x29:', (/character(KIND=LEN):: 'a1x', 'b2y', 'c3z' /)

  print *, 'x30:', [ character*(TWO):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x31:', (/character*(TWO**ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x32:', [ character*(TWO-ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x33:', (/character*(TWO/ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x34:', [ character*((ONE+ONE)/ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x35:', (/character*(ONE*TWO):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x36:', [ character*(ONE+INT(ONE+SIN(1.5))):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x37:', (/character*(sameold(ONE+INT(ONE+SIN(1.5)))):: 'a1x', 'b2y', 'c3z' /)

  print *, 'x38:', [ character(LEN=KIND,KIND=LEN):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x39:', (/character(KIND=LEN,LEN=KIND):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x40:', [ character(TWO,ONE):: 'a1x', 'b2y', 'c3z' ] ! length, kind
  print *, 'x41:', (/character(TWO,KIND=ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x42:', [ character(KIND=ONE,LEN=TWO):: 'a1x', 'b2y', 'c3z' ]

end program acetint20cp
