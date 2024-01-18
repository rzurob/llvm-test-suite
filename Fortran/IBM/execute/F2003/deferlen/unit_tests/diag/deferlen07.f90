!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The declared type of the object to be
!*                               allocated must be compatible with the
!*                               type specified in the ALLOCTATE statement.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
character(:), allocatable  :: char
allocate (character(*)::char)
end
