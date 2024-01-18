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
!*  DESCRIPTION                : Diagnostic:  1 <= DIM <= corank
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      integer, save :: coarr[*]
      integer, save :: arrcoarr(10,20,30,40)[10,*]

      print *, lcobound(coarr, 2)
      print *, lcobound(coarr, 0)
      print *, lcobound(coarr, -1)

      print *, lcobound(coarr, dim=3)
      print *, lcobound(coarr, dim=0, kind=8)
      print *, lcobound(coarr, dim=-1, kind=4)


      print *, ucobound(coarr, 2)
      print *, ucobound(coarr, 0)
      print *, ucobound(coarr, -1)

      print *, ucobound(coarr, dim=3)
      print *, ucobound(coarr, dim=0, kind=8)
      print *, ucobound(coarr, dim=-1, kind=4)


      print *, lcobound(arrcoarr, kind=4, dim=3)
      print *, lcobound(arrcoarr, 0)
      print *, lcobound(arrcoarr, -1)
      print *, ucobound(arrcoarr, 3)
      print *, ucobound(arrcoarr, dim=0)
      print *, ucobound(arrcoarr, -1)


      end
