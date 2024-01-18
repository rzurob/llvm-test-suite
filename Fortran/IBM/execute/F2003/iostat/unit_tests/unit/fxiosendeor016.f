!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxiosendeor016.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : fxiosendeor016
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Aug. 19, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This tests the functionality of the intrinsic when the
!*                               argument is an implied-do loop
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: i
      integer, parameter :: arg(7) = (/-4,-3,-2,-1,0,1,2/)
      logical :: aa(7)
       
      aa = IS_IOSTAT_END( (/ ( arg(i), i=1, 7 ) /) )
      write(*,*) aa

      aa = IS_IOSTAT_EOR( (/ ( arg(i), i=1, 7 ) /) )
      write(*,*) aa


      end
