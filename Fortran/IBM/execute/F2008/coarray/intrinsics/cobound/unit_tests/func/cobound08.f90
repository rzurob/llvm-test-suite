!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound08
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
!*  DESCRIPTION                : Testing cobound intrinsics in constant
!*                               expressions.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, save :: coarr1(5,2,3)[-1:10,4:8,*]
      integer, save :: coarr2[-8:-4,*]

      integer, parameter :: c1    = ucobound(coarr1, 1)
      integer, parameter :: c2(3) = lcobound(coarr1)
      integer, parameter :: c3    = ucobound(coarr2,1)

      integer(kind=ucobound(coarr1,2)) :: k1
      integer(kind=lcobound(coarr1,2)) :: k2
      integer(kind=-lcobound(coarr2,1)) :: k3

      if (c1 .ne. 10) error stop 1

      if (any(c2 .ne. [-1, 4, 1])) error stop 2

      if (c3 .ne. -4) error stop 3

      if (kind(k1) .ne. 8) error stop 4

      if (kind(k2) .ne. 4) error stop 5

      if (kind(k3) .ne. 8) error stop 6

      sync all

      end
