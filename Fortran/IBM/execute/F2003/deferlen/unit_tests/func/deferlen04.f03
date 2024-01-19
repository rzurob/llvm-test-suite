!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the allocatable and pointer attributes
!*                               on characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
character(:), allocatable  :: char
character(:), pointer :: pChar
character(12), target :: tChar
allocate (character(5)::char)
if (len(char) .ne. 5) error stop 1

tChar = "You got it!"
pChar=>tChar
if (pChar /= 'You got it!') error stop 2

deallocate (char)
end

