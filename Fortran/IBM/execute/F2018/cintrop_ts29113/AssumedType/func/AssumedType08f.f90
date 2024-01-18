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
!*  DESCRIPTION                : Calling a BIND(C) procedure from C,
!*                               where the procedure is defined in Fortran
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
      subroutine sub(a) bind(C)
        use iso_c_binding
        implicit none
        TYPE(*) :: a

      end subroutine sub
