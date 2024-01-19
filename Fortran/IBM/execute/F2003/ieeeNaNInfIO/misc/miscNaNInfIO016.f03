!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 26, 2006
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
!*  Cross-feature testing the IEEE NaN and Inf I/O with tabulation using
!*  T, X, TR, and TL control edit descriptors.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      real(4) :: rl1, rl2
      integer :: ii1, ii2

      integer :: myint

      integer, parameter :: in = 11, out = 12

      equivalence(rl1, ii1)
      equivalence(rl2, ii2)

      open(unit=in, file='miscNaNInfIO016.dat', action='read')
      open(unit=out, file='miscNaNInfIO016.out', action='write')

      rl1 = -1.0; rl2 = -1.0
      read(in, '(t7, f3.1, tr3, f4.2)') rl1, rl2

      ! rl1 should be +NaN(Q)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( ii1 .le. 0 ) ) error stop 1_4

      ! rl2 should be -Inf
      if ( ieee_is_finite(rl2) .or. .not. ieee_is_negative(rl2) ) then
         error stop 2_4
      end if

      rl1 = -1.0; rl2 = -1.0; myint = -1
      read(in, '(t13, i3, tl11, F8.1, 3x, es5.2)') myint, rl1, rl2

      if ( myint .ne. 566 ) error stop 3_4

      ! rl1 should be +Inf
      if ( ieee_is_finite(rl1) .or. ieee_is_negative(rl1) ) then
         error stop 4_4
      end if

      ! rl2 should be +NaN(Q)
      if ( ( .not. ieee_is_nan(rl2) ) .or.                             &
     &     ( ieee_class(rl2) .ne. ieee_quiet_nan ) .or.                &
     &     ( ii2 .le. 0 ) ) error stop 5_4


      rl1 = -1.0; rl2 = -1.0
      read(in, '(tr4, G7.1, t20, f7.2)') rl1, rl2

      ! rl1 should be -NaN(S)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_signaling_nan ) .or.            &
     &     ( ii1 .ge. 0 ) ) error stop 6_4

      ! rl2 should be -NaN(Q)
      if ( ( .not. ieee_is_nan(rl2) ) .or.                             &
     &     ( ieee_class(rl2) .ne. ieee_quiet_nan ) .or.                &
     &     ( ii2 .ge. 0 ) ) error stop 7_4


      ! Now test the output:

      write(out, '(a20, t3, F6.2, 7x, tr1, G3.2)') repeat('x',20),     &
     &     ieee_value(1.0, ieee_quiet_nan), real(z'7F800000')

      write(out, '(a12, tl10, ES4.2, tr1, F6.2)') repeat('1',12),     &
     &     real(z'FF800000'), ieee_value(1.0, ieee_signaling_nan)

      write(out, '(a11, t5, D4.2)') repeat('y',11),                   &
     &     ieee_value(1.0, ieee_quiet_nan)

      end
