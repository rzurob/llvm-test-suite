!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocPtr01f
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
!*  DESCRIPTION                : Calling a BIND(C) procedure defined on
!*                               the Fortran side, from C. On the C side
!*                               a CFI descriptor is established and
!*                               passed to Fortran to become allocated
!*                               and initialized. The results are then 
!*                               printed on the C side.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      subroutine foo(arg) bind(c)
        use, intrinsic :: iso_c_binding
        real(c_double), allocatable :: arg(:,:)
        allocate(arg(10,2))
        arg = reshape([(i, i=1,20,1)], [10, 2])
      end
