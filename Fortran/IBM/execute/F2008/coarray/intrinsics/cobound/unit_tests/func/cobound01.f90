!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound01
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
!*  DESCRIPTION                : This functional test, makes sure that
!*                               L/UCOBOUND return expected values when
!*                               coarray has explicit coshape with 
!*                               compile-time constant cobounds. 
!*                               This test case also tests the following:
!*                                 - runtime values for DIM
!*                                 - DIM present/not present
!*                                 - I/O statement
!*                                 - array assignment (testing scalarization)
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none
      integer, save :: coarr[1:9,-2:5,10:20,0:*]
      integer :: res(4)
      integer :: i,j,k,l

      if (this_image() == 1) then
         i = 1; j = 2; k = 3; l = 4;

         ! LCOBOUND
         res = lcobound(coarr)
         print *, res

         print *, lcobound(coarr)
         print *, lcobound(coarr,1), lcobound(coarr,2), lcobound(coarr,3), lcobound(coarr,4)
         print *, lcobound(coarr,i), lcobound(coarr,j), lcobound(coarr,k), lcobound(coarr,l)

         res(1) = lcobound(coarr,1)
         res(2) = lcobound(coarr,2)
         res(3) = lcobound(coarr,3)
         res(4) = lcobound(coarr,4)
         print *, res

         res(1) = lcobound(coarr,i)
         res(2) = lcobound(coarr,j)
         res(3) = lcobound(coarr,k)
         res(4) = lcobound(coarr,l)
         print *, res

         ! UCOBOUND
         res = ucobound(coarr)
         res(4) = 0 ! ucobound of the last codimension depends on the # of images.
                    ! and is being tested elsewhere.

         print *, res

         print *, ucobound(coarr,1), ucobound(coarr,2), ucobound(coarr,3)
         print *, ucobound(coarr,i), ucobound(coarr,j), ucobound(coarr,k)

         res(1) = ucobound(coarr,1)
         res(2) = ucobound(coarr,2)
         res(3) = ucobound(coarr,3)
         res(4) = 0
         print *, res

         res(1) = ucobound(coarr,i)
         res(2) = ucobound(coarr,j)
         res(3) = ucobound(coarr,k)
         res(4) = 0
         print *, res

      end if
      sync all
end
