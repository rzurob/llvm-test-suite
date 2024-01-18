!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 20, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Diagnostic: if used in constant expression
!*                               the result must be known at compile-time
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      real, save :: coarr[*]

      integer, parameter :: c = lcobound(coarr,1) ! should be ok
      integer, parameter :: cc = lcobound(coarr,2)
      integer, parameter :: ccc = ucobound(coarr,1)

      contains
      subroutine foo(coarr, n)
        integer :: n
        real :: coarr[n:*]
        real, save :: coarr2(10)[1:10,2:*]

        integer, parameter :: c0(2) = lcobound(coarr2) ! should be ok
        integer, parameter :: c1(2) = ucobound(coarr2)
        integer, parameter :: c2 = lcobound(coarr2,n)

        integer, parameter :: c3 = lcobound(coarr,n)
        integer, parameter :: c4 = lcobound(coarr,1)
        integer, parameter :: c5 = ucobound(coarr,1)
      end subroutine

      end
