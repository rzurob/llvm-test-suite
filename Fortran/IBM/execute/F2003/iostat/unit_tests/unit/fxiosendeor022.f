!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxiosendeor022.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 23, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This tests the functionality of is_iostat_eor when used
!*                               the way it is intended to be used. In other words, a
!*                               file is read until the end of record is reached. The
!*                               runtime option IOSTAT_END is set to extended
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: ios = 0, x

      call setrteopts("iostat_end=extended")

      open( 1, file='input.dat', action='read' )

      ! this loop reads the first record, but in the second iteration
      ! it can not read a 3-digit integer because of non-advancing io
      do while( .not. is_iostat_eor(ios) )

         read( 1, '(i3)', iostat=ios, advance='no', pad='no' ) x
         write(6,*) "ios = ", ios
         write(6,*) "x = ", x

      enddo

      end
