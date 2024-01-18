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
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the IMPLICIT statement related
!*                               with characters with deferred length
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit character(:) (A-C)

allocatable :: char
pointer     :: charp
character(4), target :: tchar


allocate(character(4)::char)
char = "1234"
tchar = "abcd"
charp=>tchar

if (len(char) .ne. 4 .or. char .ne. "1234") then
   error stop 1
end if

if (len(charp) .ne. 4 .or. charp .ne. "abcd") then
   error stop 2
end if

deallocate (char)
end
