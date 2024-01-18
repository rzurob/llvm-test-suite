!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : There must be ALLOCATABLE or POINTER
!*                               attribute for deferred length character.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
type A
   character(:) :: char
end type

character(:) :: char1
character(:), dimension(10, 10)  :: char2
character(:)  char3 /'I am illegal'/
character(:), parameter :: char4 = "A"

end
