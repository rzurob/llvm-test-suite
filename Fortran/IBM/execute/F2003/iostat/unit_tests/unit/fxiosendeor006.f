!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxiosendeor006.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 18, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This tests the functionality of the intrinsic when args
!*                               to the intrinsic are mathematical expressions.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: a = 4, b = 1

      ! TESTING IS_IOSTAT_END
      if( .not. IS_IOSTAT_END( 2-a ) ) then
        error stop 1
      endif
      if( .not. IS_IOSTAT_END( 3-a ) ) then
        error stop 2
      endif
      if( IS_IOSTAT_END( 0-a ) ) then
        error stop 3
      endif
      if( IS_IOSTAT_END( -a ) ) then
        error stop 4
      endif
      if( IS_IOSTAT_END( 2*a ) ) then
        error stop 5
      endif
      if( IS_IOSTAT_END( a/2 ) ) then
        error stop 6
      endif
      if( IS_IOSTAT_END( 1-a ) ) then
        error stop 7
      endif
      if( IS_IOSTAT_END( a*a/b ) ) then
        error stop 8
      endif
      if( IS_IOSTAT_END( a/a*b ) ) then
        error stop 9
      endif
      if( .not. IS_IOSTAT_END( -a/a ) ) then
        error stop 10
      endif
      if( .not. IS_IOSTAT_END( b*3-a ) ) then
        error stop 11
      endif
      if( .not. IS_IOSTAT_END( b*3-(a+b) ) ) then
        error stop 12
      endif
      if( .not. IS_IOSTAT_END( b-b-b-b ) ) then
        error stop 13
      endif
      if( .not. IS_IOSTAT_END( b-b-b ) ) then
        error stop 14
      endif
      if( .not. IS_IOSTAT_END( -b ) ) then
        error stop 15
      endif
      if( IS_IOSTAT_END(-b*a ) ) then
        error stop 16
      endif

      ! TESTING IS_IOSTAT_EOR
      if( .not. IS_IOSTAT_EOR( -a ) ) then
        error stop 17
      endif
      if( .not. IS_IOSTAT_EOR( 0-a ) ) then
        error stop 18
      endif
      if( IS_IOSTAT_EOR( 1-a ) ) then
        error stop 19
      endif
      if( IS_IOSTAT_EOR( 2-a ) ) then
        error stop 20
      endif
      if( IS_IOSTAT_EOR( 2*a ) ) then
        error stop 21
      endif
      if( IS_IOSTAT_EOR( a/2 ) ) then
        error stop 22
      endif
      if( IS_IOSTAT_EOR( 3-a ) ) then
        error stop 23
      endif
      if( IS_IOSTAT_EOR( a*a/b ) ) then
        error stop 24
      endif
      if( IS_IOSTAT_EOR( a/a*b ) ) then
        error stop 25
      endif
      if( .not. IS_IOSTAT_EOR( -(a+a)/2*b ) ) then
        error stop 26
      endif
      if( .not. IS_IOSTAT_EOR( b+b+b+b-2*a ) ) then
        error stop 27
      endif
      if( .not. IS_IOSTAT_EOR( 4*b-a*2 ) ) then
        error stop 28
      endif
      if( .not. IS_IOSTAT_EOR( b-b-b-b-b-b ) ) then
        error stop 29
      endif
      if( .not. IS_IOSTAT_EOR( -(a*(b+1) - 2*(a/(b+b)) )) ) then
        error stop 30
      endif
      if( .not. IS_IOSTAT_EOR( a-a+b-5*b ) ) then
        error stop 31
      endif
      if( .not. IS_IOSTAT_EOR( a/(b**b) * (-1) ) ) then
        error stop 32
      endif

      end
