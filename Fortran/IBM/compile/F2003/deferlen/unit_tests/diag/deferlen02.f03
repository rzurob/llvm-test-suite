!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The ALLOCATE statement for character
!*                               with deferred length must have type
!*                               spec or SOURCE =
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
character(:), allocatable :: char1
character(:), pointer     :: char2

allocate(char1)
allocate(char2)

end
