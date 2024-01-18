! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c12xxr  cxbind_c12xxr
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
!* PRIMARY FUNCTIONS TESTED     : Interoperable Functions.
!*                              - Test dummy procedure.
!*                              - test function pointers using
!*                                C_FUNPTR, C_FUNLOC.
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
  use iso_c_binding
  IMPLICIT NONE

  interface
     subroutine sub(f) bind(c)
       use iso_c_binding
       type(C_FUNPTR) :: f
     end subroutine sub
     integer function f(x) bind(c)
       integer x
     end function f
  end interface

  type(C_FUNPTR) :: pf
  pf = C_FUNLOC(f)
  call sub(pf)

end program p1

integer function f(x)
  integer x
  f= x**2
end function f




