!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
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
procedure (sub), pointer :: p1
a=10
p1=>sub				        !Error: sub can't be referenced here
end program
