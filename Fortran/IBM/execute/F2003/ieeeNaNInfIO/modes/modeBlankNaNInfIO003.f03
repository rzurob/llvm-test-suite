!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 30, 2006
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
!*  According to the standard:
!*  section 10.6.1 - Numeric editing
!*
!*    "On input, leading blanks are not significant. When the input
!*    field is not an IEEE exceptional specification (10.6.1.2.1),
!*    the interpretation of blanks, other than leading blanks, is
!*    determined by the blank interpretation mode (10.7.6)."
!*
!* Analysis:
!* Blank mode of NULL or ZERO should not affect NaN and Inf I/O. For
!* example 'N aN' should not be considered NaN regardless of blank mode.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: in = 11

      logical, external :: precision_r4

      real(4) :: rl

      open(in, file="modeBlankNaNInfIO003.dat", action='read')

      !*************************************************
      !* BLANK='ZERO'
      !************************************************

      rl = -1.0
      read(in, '(F5.2)', blank='zero') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 1_4

      rl = -1.0
      read(in, '(F4.1)', blank='zero') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 2_4

      rl = -1.0
      read(in, '(F11.1)', blank='zero') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 3_4

      rl = -1.0
      read(in, '(F8.1)', blank='zero') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 4_4

      rl = -1.0
      read(in, '(F9.1)', blank='zero') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 5_4

      rl = -1.0
      read(in, '(F8.1)', blank='zero') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 6_4

      rl = -1.0
      read(in, '(F11.1)', blank='zero') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 7_4

      rl = -1.0
      read(in, '(F5.1)', blank='zero') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 8_4

      rl = -1.0
      read(in, '(F9.1)', blank='zero') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 9_4

      rl = -1.0
      read(in, '(F9.1)', blank='zero') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 10_4

      !*************************************************
      !* BLANK='NULL'
      !************************************************

      rl = -1.0
      read(in, '(F5.2)', blank='null') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 11_4

      rl = -1.0
      read(in, '(F4.1)', blank='null') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 12_4

      rl = -1.0
      read(in, '(F11.1)', blank='null') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 13_4

      rl = -1.0
      read(in, '(F8.1)', blank='null') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 14_4

      rl = -1.0
      read(in, '(F9.1)', blank='null') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 15_4

      rl = -1.0
      read(in, '(F8.1)', blank='null') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 16_4

      rl = -1.0
      read(in, '(F11.1)', blank='null') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 17_4

      rl = -1.0
      read(in, '(F5.1)', blank='null') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 18_4

      rl = -1.0
      read(in, '(F9.1)', blank='null') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 19_4

      rl = -1.0
      read(in, '(F9.1)', blank='null') rl
      if ( .not. precision_r4(rl, 0.0_4) ) error stop 20_4


      close(in)

      end
