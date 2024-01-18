!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType01f
!*
!*  ORIGINAL PROGRAMMER        : Dorra Bouchiha
!* PROGRAMMER                  : Izhak Jakov
!* DATE                : September 9, 2015
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from C, 
!*                               where the procedure is defined in Fortran
!*
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
program main
  use ISO_C_BINDING

  interface
    subroutine cfun(x) bind(c)
      use ISO_C_BINDING, ONLY : C_FUNPTR
      type(c_funptr),value:: x
    end subroutine cfun
  end interface

  type(c_funptr) :: cproc
  cproc = c_funloc(sub)
  call cfun(cproc)

  contains
      subroutine sub(a) bind(C)
        use iso_c_binding
        implicit none
        TYPE(*) :: a
       
      end subroutine sub
end program main
