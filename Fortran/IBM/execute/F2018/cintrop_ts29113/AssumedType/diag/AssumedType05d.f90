!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : TYPE(*) cannot appear in a print statement
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedType05d
implicit none
integer :: i

i = 4
call sub(i)

contains

   subroutine sub(a)
      type(*) :: a

      print*, a
      write(6, *) a
   end subroutine sub

end program AssumedType05d
