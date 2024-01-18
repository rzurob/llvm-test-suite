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
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the dummy argument has allocatable 
!*                               attributes and is character with deferred 
!*                               length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
interface 
   subroutine sub(char)
      character(:), allocatable :: char
   end subroutine
end interface

character(:), allocatable :: char
call sub(char)
deallocate(char)
end

subroutine sub(char)
   character(:), allocatable :: char
   integer length
   character(44) tchar
   tchar = "1234567890"
   allocate (character(4)::char)
   char = tchar(5:8)
   if (char /= '5678') error stop 1
end
