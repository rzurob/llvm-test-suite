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

implicit character(:) (a-c)
target :: char1
save   :: char2
end
