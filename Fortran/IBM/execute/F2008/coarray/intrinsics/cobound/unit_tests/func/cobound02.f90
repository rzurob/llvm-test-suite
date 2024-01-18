!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound02
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
!*  DESCRIPTION                : Testing cobound inquiry intrinsics on:
!*                                 - scalar coarray
!*                                 - array coarray
!*                                 - coarray with corank 1
!*                                 - coarray with corank > 1
!*  
!*  ASSUMPTION:                : Number of images is equal to 15.
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none
      integer, save :: scoarr_corank1[*]
      integer, save :: acoarr_corank1(10)[0:*]
      integer, save :: scoarr_corank_gt1[-2:2, 0:1, 1:*]
      integer, save :: acoarr_corank_gt1(10,20,30)[-4:0, 2:3, 3:*]

      if (num_images() .ne. 15) then
         print *, "This test case must be run with 15 images"
         error stop 1
      end if

      if (this_image() == 1) then
         ! LCOBOUND
         print *, lcobound(scoarr_corank1)
         print *, lcobound(acoarr_corank1)

         print *, lcobound(scoarr_corank_gt1)
         print *, lcobound(acoarr_corank_gt1)

         ! UCOBOUND
         print *, ucobound(scoarr_corank1)
         print *, ucobound(acoarr_corank1)

         print *, ucobound(scoarr_corank_gt1)
         print *, ucobound(acoarr_corank_gt1)

      end if

      sync all

      end
