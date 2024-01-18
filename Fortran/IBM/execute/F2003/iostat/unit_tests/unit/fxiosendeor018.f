!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxiosendeor018.f
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
!*  TEST CASE TITLE            : fxiosendeor018
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
!*  DESCRIPTION                : This tests the functionality of is_iostat_end when used
!*                               the way it is intended to be used. In other words, a 
!*                               file is read until the end of file is reached. The
!*                               runtime option IOSTAT_END is set to extended
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: ios = 0, x

      call setrteopts("iostat_end=extended")

      open( 1, file='input.dat', action='read' ) 

      do while( .not. is_iostat_end(ios) )
         
         read( 1,*,iostat=ios ) x
         write(6,*) "ios = ", ios
         write(6,*) "x = ", x
         
      enddo
      
      end
