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
