!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocPtr02f
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Feb, 2013
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Allocatable/Pointer
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*
!*  DESCRIPTION                : Use C-interoperable allocatable and 
!*                               pointer arrays to multiply the 
!*                               following matrixes on the Fortran 
!*                               side and display the resulting matrix
!*                               on the C side:
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      subroutine matrix_mult_fortran(res, a, b) bind(c)
        use, intrinsic :: iso_c_binding
        real(c_double), allocatable, intent(out) :: res(:,:)
        real(c_double), pointer, intent(in) :: a(:,:)
        real(c_double), pointer, intent(in) :: b(:,:)
        integer(8) :: ub1, ub2

        ub1 = ubound(a,1)-lbound(a,1)+1
        ub2 = ubound(b,2)-lbound(b,2)+1
        allocate(res(ub1, ub2))
        res = matmul(a, b)
      end
