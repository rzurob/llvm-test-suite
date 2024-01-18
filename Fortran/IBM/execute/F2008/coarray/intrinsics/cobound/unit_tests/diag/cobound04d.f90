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
!*  DESCRIPTION                : Diagnostic: KIND must be a scalar integer
!*                               known at compile-time.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      implicit none
      integer, save :: coarr[*]
      integer(4) :: i = 4
      real :: r, ra(1)

      print *, lcobound(coarr, kind=i)
      print *, lcobound(coarr, kind=[1,2])
      print *, lcobound(coarr, kind=r, dim=1)
      print *, lcobound(coarr, kind=3.14)
      print *, lcobound(coarr, kind=3.14, dim=1)
      print *, lcobound(coarr, 1, 3.14)


      print *, ucobound(coarr, kind=i)
      print *, ucobound(coarr, kind=ra)
      print *, ucobound(coarr, kind=r, dim=1)
      print *, ucobound(coarr, kind=3.14)
      print *, ucobound(coarr, kind=3.14, dim=1)
      print *, ucobound(coarr, 1, 3.14)

      end
