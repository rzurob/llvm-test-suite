!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Abstractr Interface
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
!*  DESCRIPTION                : The procedures defined in an abstract
!*                               interface block are referenced.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
abstract interface 
   subroutine sub(arg)
      integer arg
   end subroutine
end interface 
integer a
a=10
call sub(a)				!Error: sub can't be referenced here
end program
subroutine sub(a)
   integer a
   a = a*a
end subroutine
