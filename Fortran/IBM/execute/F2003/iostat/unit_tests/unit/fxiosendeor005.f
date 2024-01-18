!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 18, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This tests the functionality of the intrinsic when args
!*                               of type scalar integer are passed.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: a = -1, b = -2, c = -3, d = -4,                        &
     & e = 0, f = 1, g = -5, h = 1000000, j = -1000000
      integer :: i  ! this is used to test a wider range of integers in a loop
      logical :: res

! FIRST TEST SOME INITIALIZED INTEGERS:
      ! TESTING IS_IOSTAT_END
      if( .not. IS_IOSTAT_END( a ) ) then
        error stop 1
      endif
      if( .not. IS_IOSTAT_END( b ) ) then
        error stop 2
      endif
      if( IS_IOSTAT_END( c ) ) then
        error stop 3
      endif
      if( IS_IOSTAT_END( d ) ) then
        error stop 4
      endif
      if( IS_IOSTAT_END( e ) ) then
        error stop 5
      endif
      if( IS_IOSTAT_END( f ) ) then
        error stop 6
      endif
      if( IS_IOSTAT_END( g ) ) then
        error stop 7
      endif
      if( IS_IOSTAT_END( h ) ) then
        error stop 8
      endif
      if( IS_IOSTAT_END( j ) ) then
        error stop 9
      endif
      res = IS_IOSTAT_END( a )
      if( .not. res ) then
         error stop 10
      endif
      res = IS_IOSTAT_END( b )
      if( .not. res ) then
         error stop 11
      endif
      res = IS_IOSTAT_END( c )
      if( res ) then
         error stop 12
      endif

      ! TESTING IS_IOSTAT_EOR
      if( IS_IOSTAT_EOR( a ) ) then
        error stop 13
      endif
      if( IS_IOSTAT_EOR( b ) ) then
        error stop 14
      endif
      if( IS_IOSTAT_EOR( c ) ) then
        error stop 15
      endif
      if( .not. IS_IOSTAT_EOR( d ) ) then
        error stop 16
      endif
      if( IS_IOSTAT_EOR( e ) ) then
        error stop 17
      endif
      if( IS_IOSTAT_EOR( f ) ) then
        error stop 18
      endif
      if( IS_IOSTAT_EOR( g ) ) then
        error stop 19
      endif
      if( IS_IOSTAT_EOR( h ) ) then
        error stop 20
      endif
      if( IS_IOSTAT_EOR( j ) ) then
        error stop 21
      endif
      res = IS_IOSTAT_EOR( d )
      if( .not. res ) then
         error stop 22
      endif
      res = IS_IOSTAT_EOR( a )
      if( res ) then
         error stop 23
      endif
      res = IS_IOSTAT_EOR( g )
      if( res ) then
         error stop 24
      endif

! NOW TEST A WIDER RANGE OF INTEGERS:
      do i = -50, 50, 1
         if( ((i .eq. -2) .or. (i .eq. -1)) .and.                       &
     &        .not. IS_IOSTAT_END(i) ) then
            error stop 25
         else if((i .ne. -2) .and. (i .ne. -1) .and. IS_IOSTAT_END(i))  &
     &   then
            error stop 26
         endif
         if( (i .eq. -4) .and. .not. IS_IOSTAT_EOR(i) ) then
            error stop 27
         else if ( i .ne. -4 .and. IS_IOSTAT_EOR(i) ) then
            error stop 28
         endif
      enddo

      end
