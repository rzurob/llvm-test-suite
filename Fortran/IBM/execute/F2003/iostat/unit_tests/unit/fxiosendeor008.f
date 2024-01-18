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
!*  DESCRIPTION                : This tests the functionality of the intrinsic when args
!*                               to the intrinsic are literal values.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer, parameter :: a = -4, b = -1, c = -2, d = 3

      ! TESTING IS_IOSTAT_END
      if( .not. IS_IOSTAT_END( b ) ) then
        error stop 1
      endif
      if( .not. IS_IOSTAT_END( c ) ) then
        error stop 2
      endif
      if( IS_IOSTAT_END( a ) ) then
        error stop 3
      endif
      if( IS_IOSTAT_END( d ) ) then
        error stop 4
      endif
      if( .not. IS_IOSTAT_END( -1 ) ) then
        error stop 5
      endif
      if( .not. IS_IOSTAT_END( -2 ) ) then
        error stop 6
      endif
      if( IS_IOSTAT_END( -4 ) ) then
        error stop 7
      endif
      if( IS_IOSTAT_END( 3 ) ) then
        error stop 8
      endif
      if( .not. IS_IOSTAT_END( 3-4*1 ) ) then
        error stop 9
      endif


      ! TESTING IS_IOSTAT_EOR
      if( .not. IS_IOSTAT_EOR( a ) ) then
        error stop 10
      endif
      if( IS_IOSTAT_EOR( b ) ) then
        error stop 11
      endif
      if( IS_IOSTAT_EOR( c ) ) then
        error stop 12
      endif
      if( IS_IOSTAT_EOR( d ) ) then
        error stop 13
      endif
      if( .not. IS_IOSTAT_EOR( -4 ) ) then
        error stop 14
      endif
      if( IS_IOSTAT_EOR( -1 ) ) then
        error stop 15
      endif
      if( IS_IOSTAT_EOR( -2 ) ) then
        error stop 16
      endif
      if( IS_IOSTAT_EOR( 3 ) ) then
        error stop 17
      endif
      if( .not. IS_IOSTAT_EOR( 3-7*1 ) ) then
        error stop 18
      endif

      end
