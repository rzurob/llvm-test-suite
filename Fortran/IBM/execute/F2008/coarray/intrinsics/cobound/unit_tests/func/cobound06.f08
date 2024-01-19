!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 20, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Functionality of cobound intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing runtime values of ucobound focusing
!*                               on upper cobound of the last codimension.
!*
!*  ASSUMPTION:                : Number of images is equal to 30.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, save :: c1(5)[1:2,-4:-2,10:11,0:*]
      integer, save :: c2[2:3, 0:7, *]
      integer, save :: c3(2,4)[60,*]
      integer :: i1
      integer :: res(4)

      if (num_images() .ne. 30) then
         print *, "This test case must be run with 30 images"
         error stop 1
      end if

      if (this_image() == 1) then
         i1 = 1
         print *, "c1 ucobounds:"
         res = ucobound(c1)
         print *, res

         print *, ucobound(c1)
         print *, ucobound(c1,i1), ucobound(c1,2*i1), ucobound(c1,3*i1), ucobound(c1,4*i1)

         res = 0
         res(1) = ucobound(c1,i1)
         res(2) = ucobound(c1,i1+1)
         res(3) = ucobound(c1,i1+2)
         res(4) = ucobound(c1,dim=i1*4) ! should be 2
         print *, res

         res = 0
         res(1) = ucobound(c1,dim=lcobound(c1,i1))
         res(2) = ucobound(c1,dim=ucobound(c1,i1))
         res(3) = ucobound(c1,dim=ucobound(c1,i1)+1)
         res(4) = ucobound(c1,dim=ucobound(c1,4)+2) ! should be 2
         print *, res

         print *, "coarray argument ucobounds:"
         call foo(1) ! should print 30
         call foo(5) ! should print 34

         print *, "c2 ucobounds:"
         print *, ucobound(c2) ! should print 3, 7, 2

         print *, "c3 ucobounds:"
         print *, ucobound(c3) ! should print 60, 1

      end if

      sync all

      contains
      subroutine foo(n)
        integer :: n
        integer, save :: coarr[n:*]
        print *, ucobound(coarr)
      end subroutine
      end
