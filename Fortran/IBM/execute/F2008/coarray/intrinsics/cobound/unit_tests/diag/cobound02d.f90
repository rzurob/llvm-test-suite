!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound02d
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Aug 20, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Diagnostic: DIM must be a scalar integer
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      integer, save :: coarr[*]
      real :: rl1
      integer :: arr(10)
      integer(8) :: i8

      print *, lcobound(coarr, 1.5)
      print *, lcobound(coarr, dim=1.5)
      print *, lcobound(coarr, dim=rl1, kind=4)

      print *, lcobound(coarr, arr)
      print *, lcobound(coarr, dim=arr, kind=4)
      print *, lcobound(coarr, dim=[1,2], kind=4)

      print *, lcobound(coarr, dim=i8) ! this should be fine


      print *, ucobound(coarr, 1.5)
      print *, ucobound(coarr, dim=1.5)
      print *, ucobound(coarr, dim=rl1, kind=4)

      print *, ucobound(coarr, arr)
      print *, ucobound(coarr, dim=arr, kind=4)
      print *, ucobound(coarr, dim=[1,2], kind=4)

      print *, ucobound(coarr, dim=i8) ! this should be fine

      end
