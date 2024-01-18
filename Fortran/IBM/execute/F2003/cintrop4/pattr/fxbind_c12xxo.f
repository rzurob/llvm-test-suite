! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c12xxo  cxbind_c12xxo
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Test dummy procedure.
!*                              - Pointer to Function.
!*                              - Main program in Fortran,
!*                                Fortran call C function.
!*                              - One interop procedure implemented in C
!                                 and another one in Fortran.
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

integer function f(x)
integer x

 f= x**2

end function f




