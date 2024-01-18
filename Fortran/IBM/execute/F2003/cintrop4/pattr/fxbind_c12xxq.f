! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Test dummy procedure.
!*                              - Pointer to Function.
!*                              - Main program in Fortran,
!*                                Fortran call C function.
!*                              - interop procedure implemented in C
!                                 code.
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM p1
 IMPLICIT NONE

  interface
     subroutine sub(f) bind(c)
       interface
          integer function f(x) bind(c)
            integer x
          end function f
       end interface
     end subroutine sub
     integer function f(x) bind(c)
       integer x
     end function f
  end interface

  call sub(f)

end program p1




