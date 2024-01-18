!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint20c
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-09-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier in assignment (CHARACTER), init expr in kind and len
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
!*  Verify that the array constructor using a character type specifier works
!*  correctly within an assignment statement.  Use different forms of type
!*  specifier, using initialization expressions for the kind.
!*  The test is successful if the programme  compiles correctly.
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

program acetint20c

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

  charr = [ character(TWO):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x 1:', charr
  charr = (/character(TWO8**ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x 2:', charr
  charr = [ character(TWO4-ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x 3:', charr
  charr = (/character(TWO2/ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x 4:', charr
  charr = [ character((ONE+ONE)/ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x 5:', charr
  charr = (/character(ONE*TWO1):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x 6:', charr
  charr = [ character(ONE+INT(ONE+SIN(1.5))):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x 7:', charr
  charr = (/character(sameold(ONE+INT(ONE+SIN(1.5)))):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x 8:', charr

  charr = [ character(LEN=TWO1):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x 9:', charr
  charr = (/character(LEN=TWO2**ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x10:', charr
  charr = [ character(LEN=TWO2-ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x11:', charr
  charr = (/character(LEN=TWO4/ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x12:', charr
  charr = [ character(LEN=(ONE+ONE)/ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x13:', charr
  charr = (/character(LEN=ONE*TWO8):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x14:', charr
  charr = [ character(LEN=ONE+INT(ONE+SIN(1.5))):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x15:', charr
  charr = (/character(LEN=sameold(ONE+INT(ONE+SIN(1.5)))):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x16:', charr

  charr = [ character(KIND=ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x17:', charr
  charr = (/character(KIND=ONE**TWO):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x18:', charr
  charr = [ character(KIND=TWO-ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x19:', charr
  charr = (/character(KIND=TWO/TWO):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x20:', charr
  charr = [ character(KIND=(ONE+ONE)/TWO):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x21:', charr
  charr = (/character(KIND=ONE*ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x22:', charr
  charr = [ character(KIND=ONE+INT(SIN(1.5))):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x23:', charr
! charr = (/character(KIND=sameold(ONE+INT(SIN(1.5)))):: 'a1x', 'b2y', 'c3z' /)
! print *, 'x24:', charr

  charr = [ character(KIND):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x24:', charr
  charr = (/character(LEN):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x25:', charr
  charr = [ character(LEN=KIND):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x26:', charr
  charr = (/character(LEN=LEN):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x27:', charr
  charr = [ character(KIND=KIND):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x28:', charr
  charr = (/character(KIND=LEN):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x29:', charr

  charr = [ character*(TWO):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x30:', charr
  charr = (/character*(TWO**ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x31:', charr
  charr = [ character*(TWO-ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x32:', charr
  charr = (/character*(TWO/ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x33:', charr
  charr = [ character*((ONE+ONE)/ONE):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x34:', charr
  charr = (/character*(ONE*TWO):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x35:', charr
  charr = [ character*(ONE+INT(ONE+SIN(1.5))):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x36:', charr
  charr = (/character*(sameold(ONE+INT(ONE+SIN(1.5)))):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x37:', charr

  charr = [ character(LEN=KIND,KIND=LEN):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x38:', charr
  charr = (/character(KIND=LEN,LEN=KIND):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x39:', charr
  charr = [ character(TWO,ONE):: 'a1x', 'b2y', 'c3z' ] ! length, kind
  print *, 'x40:', charr
  charr = (/character(TWO,KIND=ONE):: 'a1x', 'b2y', 'c3z' /)
  print *, 'x41:', charr
  charr = [ character(KIND=ONE,LEN=TWO):: 'a1x', 'b2y', 'c3z' ]
  print *, 'x42:', charr

end program acetint20c
