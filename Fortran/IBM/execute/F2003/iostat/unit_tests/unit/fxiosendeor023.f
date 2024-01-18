!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxiosendeor023.f
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
!*  TEST CASE TITLE            : fxiosendeor023
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Aug. 23, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This tests the functionality of is_iostat_eor when used
!*                               the way it is intended to be used. In other words, a 
!*                               file is read until the end of record is reached. The
!*                               runtime option IOSTAT_END is set to 2003std
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: ios = 0, x

      call setrteopts("iostat_end=2003std")

      open( 1, file='input.dat', action='read' ) 

      do while( .not. is_iostat_eor(ios) )
         
         read( 1, '(i3)', iostat=ios, advance='no', pad='no' ) x
         write(6,*) "ios = ", ios
         write(6,*) "x = ", x
         
      enddo
      
      end
