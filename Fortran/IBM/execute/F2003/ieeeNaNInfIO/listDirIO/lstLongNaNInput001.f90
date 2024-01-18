!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : lstLongNaNInput001.f
!*
!*  DATE                       : June 26, 2006
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
!*  Try very large string of alphanumeric characters inside the
!*  parentheses for NaN input.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11

      complex(4)   :: cx
      real(4)      :: rl1, rl2, rl3, rl4
      integer(4)   :: ii1, ii2, ii3, ii4
      character(3) :: char

      logical precision_r4

      equivalence(rl1, ii1)
      equivalence(rl2, ii2)
      equivalence(rl3, ii3)
      equivalence(rl4, ii4)


      open(in,  file='lstLongNaNInput001.dat', action='read')

      ! initialize variables:
      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0
      ii1 = 0; ii2 = 0; ii3 = 0; ii4 = 0;
      char = 'xxx'

      ! read the first line and analyze the input:
      read(in, *) rl1, rl4, cx, char

      if ( (.not. ieee_is_nan(rl1)) .or.                            &
     &     (ieee_class(rl1) .ne. ieee_quiet_nan) .or.               &
     &     (ii1 .le. 0) ) error stop 1_4

      if ( .not. precision_r4(rl4, 3.14) ) error stop 2_4

      rl2 = real(cx)
      rl3 = imag(cx)

      if ( (.not. ieee_is_nan(rl2)) .or.                            &
     &     (ieee_class(rl2) .ne. ieee_quiet_nan) .or.               &
     &     (ii2 .ge. 0) ) error stop 3_4

      if ( (.not. ieee_is_nan(rl3)) .or.                            &
     &     (ieee_class(rl3) .ne. ieee_signaling_nan) .or.           &
     &     (ii3 .le. 0) ) error stop 4_4

      if ( char .ne. 'ibm' ) error stop 5_4

      ! reset variables:
      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0
      ii1 = 0; ii2 = 0; ii3 = 0; ii4 = 0;
      char = 'xxx'


      ! read the second line and analyze the input:
      read(in, *) rl1, rl4, cx, char

      if ( (.not. ieee_is_nan(rl1)) .or.                            &
     &     (ieee_class(rl1) .ne. ieee_quiet_nan) .or.               &
     &     (ii1 .le. 0) ) error stop 6_4

      if ( .not. precision_r4(rl4, -3.14) ) error stop 7_4

      rl2 = real(cx)
      rl3 = imag(cx)

      if ( ieee_is_finite(rl2) .or. ieee_is_negative(rl2) ) then
         error stop 8_4
      end if

      if ( (.not. ieee_is_nan(rl3)) .or.                            &
     &     (ieee_class(rl3) .ne. ieee_quiet_nan) .or.               &
     &     (ii3 .le. 0) ) error stop 9_4

      if ( char .ne. 'xlf' ) error stop 10_4



      ! reset variables:
      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0
      ii1 = 0; ii2 = 0; ii3 = 0; ii4 = 0;
      char = 'xxx'


      ! read the third line and analyze the input:
      read(in, *) rl1, rl4, cx, char

      if ( (.not. ieee_is_nan(rl1)) .or.                            &
     &     (ieee_class(rl1) .ne. ieee_quiet_nan) .or.               &
     &     (ii1 .le. 0) ) error stop 11_4

      if ( .not. precision_r4(rl4, 3.14) ) error stop 12_4

      rl2 = real(cx)
      rl3 = imag(cx)

      if ( (.not. ieee_is_nan(rl2)) .or.                            &
     &     ( ieee_class(rl2) .ne. ieee_signaling_nan) .or.          &
     &     ( ii2 .le. 0 ) ) error stop 13_4


      if ( (.not. ieee_is_nan(rl3)) .or.                            &
     &     (ieee_class(rl3) .ne. ieee_quiet_nan) .or.               &
     &     (ii3 .ge. 0) ) error stop 14_4

      if ( char .ne. 'ibm' ) error stop 15_4

      close(in)

      end
