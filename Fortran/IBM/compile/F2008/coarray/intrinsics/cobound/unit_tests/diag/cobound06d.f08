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
!*  DESCRIPTION                : Diagnostic: Argument Keywords
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none
      integer, save :: coarr[*]

      print *, lcobound(COARR=coarr)
      print *, lcobound(COARRay=coarr) ! this should be fine.
      print *, lcobound(coarr, dimm=1, kinds=4)
      print *, lcobound(coarr, dim=1, kinds=4)

      print *, ucobound(COARR=coarr)
      print *, ucobound(COARRay=coarr) ! this should be fine.
      print *, ucobound(coarr, dimm=1, kinds=4)
      print *, ucobound(coarr, dim=1, kinds=4)

      end
