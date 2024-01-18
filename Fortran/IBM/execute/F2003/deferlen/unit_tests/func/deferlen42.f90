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
!*  DESCRIPTION                : Testing the function result which is
!*                               a character with deferred length and 
!*                               pointer attribute.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

 implicit none

 interface
    function fcn(char)
       character(*) char
       character(:) ,pointer :: fcn
    end function
 end interface

 character(5) char
 character (:), pointer :: r_char
 allocate (character(10)::r_char)
 char = '54321'
 r_char = fcn(char)
 if (r_char /= '5432112345') error stop 1
 end

 function fcn(char)
     character(*) char
     character(:) ,pointer :: fcn
     allocate(character(len(char) + 5)::fcn)
     fcn = char//'12345'
 end function

