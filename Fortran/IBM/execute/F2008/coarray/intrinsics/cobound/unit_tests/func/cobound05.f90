!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound05
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
!*                               local coarrays of a subprogram with
!*                               constant and non-constant cobounds
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      call sub(22)
      call foo()

      sync all

      contains
      subroutine sub(n)
        integer :: n
        integer, save :: coarr[n:*]
        if (lcobound(coarr,1) .ne. 22) error stop 1
      end subroutine

      subroutine foo()
        integer, save :: coarr[-1:3, 4:*]
        integer :: i1
        i1 = 1
        if (lcobound(coarr,1) .ne. -1) error stop 2
        if (lcobound(coarr,2) .ne. 4) error stop 3
        if (ucobound(coarr,1) .ne. 3) error stop 4

        if (lcobound(coarr,i1) .ne. -1) error stop 5
        if (lcobound(coarr,2*i1) .ne. 4) error stop 6
        if (ucobound(coarr,i1) .ne. 3) error stop 7
      end subroutine

      end
