!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 19, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This tests the functionality of is_iostat_end when used
!*                               the way it is intended to be used. In other words, a
!*                               file is read until the end of file is reached. The
!*                               runtime option IOSTAT_END is set to 2003std
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: ios = 0, x

      call setrteopts("iostat_end=2003std")

      open( 1, file='input.dat', action='read' )

      do while( .not. is_iostat_end(ios) )

         read( 1,*,iostat=ios ) x
         write(6,*) "ios = ", ios
         write(6,*) "x = ", x

      enddo

      end