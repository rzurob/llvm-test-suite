!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Length on type specification must be an
!*                               integer constant expression.
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
character(:), allocatable  :: char
allocate (character(:)::char)
end
