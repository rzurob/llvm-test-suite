!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : inputRealInf004.f
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
!*  Test input of IEEE Infinity (positive & negative) for reals(4)/(8)/(16)
!*  with the EN edit descriptor.
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


      ! open the file containing positive IEEE Infinity values
      open(unit, file='real_inf_pos.dat', action='read')

      ! Read in to a real(4) variable
      do
         i = i+1

         rl1 = 0.0 ! reset rl1

         read(unit,'(EN15.1)',iostat=ios) rl1

         if ( is_iostat_end(ios) ) exit

         if ( ieee_is_finite( rl1 ) ) call zzrc(1000_4+i)

         if ( ieee_is_negative( rl1 ) ) call zzrc(2000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      rewind(unit) ! reposition the file to the beginning.
      i = 0

      ! Read in to a real(8) variable
      do
         i = i+1

         rl2 = 0.0 ! reset rl2

         read(unit,'(EN15.1)',iostat=ios) rl2

         if ( is_iostat_end(ios) ) exit

         if ( ieee_is_finite( rl2 ) ) call zzrc(3000_4+i)

         if ( ieee_is_negative( rl2 ) ) call zzrc(4000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      rewind(unit) ! reposition the file to the beginning.
      i = 0

      ! Read in to a real(16) variable
      do
         i = i+1

         rl3 = 0.0 ! reset rl3

         read(unit,'(EN15.1)',iostat=ios) rl3

         if ( is_iostat_end(ios) ) exit

         if ( ieee_is_finite( rl3equiv ) ) call zzrc(5000_4+i)

         if ( ieee_is_negative( rl3equiv ) ) call zzrc(6000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      close(unit)
      i = 0

      ! open the file containing negative IEEE Infinity values
      open(unit, file='real_inf_neg.dat', action='read')

      ! Read in to a real(4) variable
      do
         i = i+1

         rl1 = 0.0 ! reset rl1

         read(unit,'(EN15.1)',iostat=ios) rl1

         if ( is_iostat_end(ios) ) exit

         if ( ieee_is_finite( rl1 ) ) call zzrc(7000_4+i)

         if ( .not. ieee_is_negative( rl1 ) ) call zzrc(8000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      rewind(unit) ! reposition the file to the beginning.
      i = 0

      ! Read in to a real(8) variable
      do
         i = i+1

         rl2 = 0.0 ! reset rl2

         read(unit,'(EN15.1)',iostat=ios) rl2

         if ( is_iostat_end(ios) ) exit

         if ( ieee_is_finite( rl2 ) ) call zzrc(9000_4+i)

         if ( .not. ieee_is_negative( rl2 ) ) call zzrc(10000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      rewind(unit) ! reposition the file to the beginning.
      i = 0

      ! Read in to a real(16) variable
      do
         i = i+1

         rl3 = 0.0 ! reset rl3

         read(unit,'(EN15.1)',iostat=ios) rl3

         if ( is_iostat_end(ios) ) exit

         if ( ieee_is_finite( rl3equiv ) ) call zzrc(11000_4+i)

         if ( .not. ieee_is_negative( rl3equiv ) ) call zzrc(12000_4+i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      close(unit)

      end
