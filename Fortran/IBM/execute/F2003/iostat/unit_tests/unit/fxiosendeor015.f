!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxiosendeor015.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 19, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This tests the functionality of the intrinsic when used
!*                               in an implied-do loop.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: i
      integer, parameter :: arg(7) = (/-4,-3,-2,-1,0,1,2/)
      logical :: aa(7)

      aa = (/ ( IS_IOSTAT_END( arg(i) ), i=1,7 ) /)
      write(*,*) aa

      aa = (/ ( IS_IOSTAT_EOR( arg(i) ), i=1,7 ) /)
      write(*,*) aa

      end
