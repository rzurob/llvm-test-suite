!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound09
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
!*  DESCRIPTION                : Testing cobound intrinsics in specification
!*                               expressions.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, save :: coarr(10)[(10*int(cos(0.0))+5)/2:10, 6:*]

      integer, parameter :: c(2) = lcobound(coarr)

      if (any(c .ne. [7,6])) error stop 1
      call foo([1,2,3,4,5,6,7,8,9,10], 1)

      sync all

      contains

      subroutine foo(arg,n)
        integer :: n
        integer :: arg(lcobound(coarr,n*2):)
        character(len=lcobound(coarr,n)) :: c1
        character(len=ucobound(coarr,1)) :: c2

        if (len(c1) .ne. 7) error stop 2
        if (len(c2) .ne. 10) error stop 3
        if (lbound(arg,1) .ne. 6) error stop 4
      end subroutine

      end

