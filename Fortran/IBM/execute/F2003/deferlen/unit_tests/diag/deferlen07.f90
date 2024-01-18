!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Deferred Character Length
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  DRIVER STANZA              : xlf90/95
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
