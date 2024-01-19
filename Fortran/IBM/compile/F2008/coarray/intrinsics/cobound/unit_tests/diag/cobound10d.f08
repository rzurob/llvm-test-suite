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
!*  DESCRIPTION                : Diagnostic: specification and constant expr
!*                               including deferred coshape coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, save :: coarr[0:10,2:*]
      integer, allocatable :: alco[:]

      integer, parameter :: c1 = ucobound(coarr, 2)
      integer, parameter :: c2(2) = ucobound(coarr)
      integer, parameter :: c3(2) = lcobound(coarr) ! this should be fine
      integer, parameter :: c4 = lcobound(alco)
      integer(kind=lcobound(coarr,1)) :: i1 ! value is not valid for KIND
      integer(kind=lcobound(coarr,2)) :: i2 ! this should be fine


      contains
      subroutine sub(arg, n)
        integer :: n
        integer :: arg[n:*]
        integer(kind=lcobound(arg, 1)) :: ii
        integer(kind=ucobound(arg, 1)) :: jj
      end subroutine


      end
