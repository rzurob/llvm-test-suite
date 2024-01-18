!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 20, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Diagnostic: Value of KIND must be a
!*                               valid value for an integer KIND type
!*                               parameter.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      integer, save :: coarr[*]
      integer(4) :: i
      integer, parameter :: kk = 8
      real :: r

      print *, lcobound(coarr, 1, 13)
      print *, lcobound(coarr, 1, -4)
      print *, lcobound(COARRAY=coarr, dim=1, kind=0)
      print *, lcobound(COARRAY=coarr, dim=1, kind=2) ! this should be fine
      print *, lcobound(coarr, kind=9, dim=1)
      print *, lcobound(coarr, kind=kk, dim=1) ! this should be fine


      print *, ucobound(coarr, 1, 13)
      print *, ucobound(coarr, 1, -4)
      print *, ucobound(COARRAY=coarr, dim=1, kind=0)
      print *, ucobound(COARRAY=coarr, dim=1, kind=2) ! this should be fine
      print *, ucobound(coarr, kind=9, dim=1)
      print *, ucobound(coarr, kind=kk, dim=1) ! this should be fine

      end
