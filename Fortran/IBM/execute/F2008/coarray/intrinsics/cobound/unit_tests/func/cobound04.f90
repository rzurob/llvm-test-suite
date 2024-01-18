!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound04
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Aug 20, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Functionality of cobound intrinsics
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Testing cobound inquiry intrinsics on
!*                               coarray dummy arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, save :: c[*]
            
      call sub(c, 2)
      call foo(c)

      sync all

      contains
      subroutine sub(coarr, n)
        integer :: n
        integer :: coarr[n:*]
        if (lcobound(coarr,1) .ne. 2) error stop 1
      end subroutine

      subroutine foo(coarr)
        integer :: coarr[-1:3, 4:*]
        if (lcobound(coarr,1) .ne. -1) error stop 2
        if (lcobound(coarr,2) .ne. 4) error stop 3
        if (ucobound(coarr,1) .ne. 3) error stop 4
      end subroutine

      end
