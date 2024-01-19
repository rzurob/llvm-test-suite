!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the concatanations on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
type A
   character(:), pointer :: name
end type

type(A) p
character(:), pointer  :: char(:, :)
character(8), target   ::  char1
character(8), target   ::  achar(2,2)

char1 = "abcdefgh"
p%name => char1(1:4)

char => achar
achar = p%name

char1 = "efghabcd"
achar(1,2) = p%name(2:4)//char1(2:6)
achar(2,1) = p%name//char1(5:8)

if ((char(1,2) .ne. "fghfghab") .or. (char(2,1) .ne. "efghabcd")) error stop 1

end

