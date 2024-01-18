!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 8, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Test input of IEEE NaN (quiet and signaling) for reals(4)/(8)/(16)
!*  with the G edit descriptor.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      real(4)  :: rl1
      real(8)  :: rl2, rl3equiv
      real(16) :: rl3
      integer  :: ios
      integer(4)  :: i = 0 ! return code whose least significant 3-digit
                           ! value correspond to the line of input causing the error.

      integer, parameter :: unit = 11

      equivalence(rl3equiv, rl3) ! ieee intrinsics are not fully supported for
                                 ! extended reals, so we input to an extended
                                 ! real, and then use the real(8) equivalent
                                 ! for verification.

      ! open the file containing signaling NaN input values
      open(unit, file='real_nan_s.dat', action='read')

      ! Read in the signaling NaNs to a real(4) variable
      do
         i = i+1

         rl1 = 0.0 ! reset rl1

         read(unit,'(G15.1)',iostat=ios) rl1

         if ( is_iostat_end(ios) ) exit

         if ( .not. ieee_is_nan( rl1 ) ) call zzrc(1000_4+i)

         if ( ieee_class( rl1 ) .ne. ieee_signaling_nan ) call zzrc(2000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      rewind(unit) ! reposition the file to the beginning.
      i = 0

      ! Read in the signaling NaNs to a real(8) variable
      do
         i = i+1

         rl2 = 0.0 ! reset rl2

         read(unit,'(G15.1)',iostat=ios) rl2

         if ( is_iostat_end(ios) ) exit

         if ( .not. ieee_is_nan( rl2 ) ) call zzrc(3000_4+i)

         if ( ieee_class( rl2 ) .ne. ieee_signaling_nan ) call zzrc(4000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      rewind(unit) ! reposition the file to the beginning.
      i = 0

      ! Read in the signaling NaNs to a real(16) variable
      do
         i = i+1

         rl3 = 0.0 ! reset rl3

         read(unit,'(G15.1)',iostat=ios) rl3

         if ( is_iostat_end(ios) ) exit

         if ( .not. ieee_is_nan( rl3equiv ) ) call zzrc(5000_4+i)

         if ( ieee_class( rl3equiv ) .ne. ieee_signaling_nan ) call zzrc(6000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if


      close(unit) ! Close the file containing signaling NaNs.
      i = 0

      ! open the file containing quiet NaN input values
      open(unit, file='real_nan_q.dat', action='read')

      ! Read in the quiet NaNs into a real(4) variable
      do
         i = i+1

         rl1 = 0.0 ! reset rl1

         read(unit,'(G15.1)',iostat=ios) rl1

         if ( is_iostat_end(ios) ) exit

         if ( .not. ieee_is_nan( rl1 ) ) call zzrc(7000_4+i)

         if ( ieee_class( rl1 ) .ne. ieee_quiet_nan ) call zzrc(8000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      rewind(unit) ! reposition the file to the beginning
      i = 0

      ! Read in the quiet NaNs into a real(8) variable
      do
         i = i+1

         rl2 = 0.0 ! reset rl2

         read(unit,'(G15.1)',iostat=ios) rl2

         if ( is_iostat_end(ios) ) exit

         if ( .not. ieee_is_nan( rl2 ) ) call zzrc(9000_4+i)

         if ( ieee_class( rl2 ) .ne. ieee_quiet_nan ) call zzrc(10000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      rewind(unit) ! reposition the file to the beginning
      i = 0

      ! Read in the quiet NaNs into a real(16) variable
      do
         i = i+1

         rl3 = 0.0 ! reset rl3

         read(unit,'(G15.1)',iostat=ios) rl3

         if ( is_iostat_end(ios) ) exit

         if ( .not. ieee_is_nan( rl3equiv ) ) call zzrc(11000_4+i)

         if ( ieee_class( rl3equiv ) .ne. ieee_quiet_nan ) call zzrc(12000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if


      close(unit)


      end
