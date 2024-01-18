!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocPtr17f
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : March, 2013
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
!*  DESCRIPTION                : Check the associated and contiguous
!*                               flags after the call to some CFI
!*                               functions.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      function test_contig (p) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), pointer :: p(:)
        integer(c_short) :: test_contig
        if (is_contiguous(p)) then
           test_contig = 1
        else
           test_contig = 0
        end if
      end

      function test_assoc (p) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), pointer :: p(:)
        integer(c_short) :: test_assoc
        if (associated(p)) then
           test_assoc = 1
        else
           test_assoc = 0
        end if
      end
