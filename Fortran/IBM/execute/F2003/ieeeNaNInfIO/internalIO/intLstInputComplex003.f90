!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : intLstInputComplex003.f
!*
!*  DATE                       : June 27, 2006
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
!*  Test input of IEEE NaN and Inf with internal files using list-directed I/O.
!*  In this testcase IEEE exceptional specifications are placed inside objects
!*  of type COMPLEX and kind 16.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      character(200) :: iFile =                                        &
     &     '(nan(q),-inf) (+infinity, -nan(s)) (nan(_abc13), -nan())'  &
     &  // ' (-nan,+inf) (-infinity,+inf)'

      complex(16) :: cx1, cx2, cx3, cx4, cx5

      real(16) :: rl1i, rl2i, rl3i, rl4i, rl5i ! imaginary parts
      real(16) :: rl1r, rl2r, rl3r, rl4r, rl5r ! real parts

      real(8) :: rl1ie, rl2ie, rl3ie, rl4ie, rl5ie ! 8-byte equivalence of imaginary parts
      real(8) :: rl1re, rl2re, rl3re, rl4re, rl5re ! 8-byte equivalence of real parts

      integer(8) :: ii1i, ii2i, ii3i, ii4i, ii5i ! integer equivalence of imaginary parts
      integer(8) :: ii1r, ii2r, ii3r, ii4r, ii5r ! integer equivalence of real parts

      equivalence(rl1i, ii1i, rl1ie); equivalence(rl2i, ii2i, rl2ie)
      equivalence(rl3i, ii3i, rl3ie); equivalence(rl4i, ii4i, rl4ie)
      equivalence(rl5i, ii5i, rl5ie)

      equivalence(rl1r, ii1r, rl1re); equivalence(rl2r, ii2r, rl2re)
      equivalence(rl3r, ii3r, rl3re); equivalence(rl4r, ii4r, rl4re)
      equivalence(rl5r, ii5r, rl5re)

      ! initialize variables:
      cx1 = (0.0, 0.0); cx2 = (0.0, 0.0); cx3 = (0.0, 0.0)
      cx4 = (0.0, 0.0); cx5 = (0.0, 0.0)

      ! read from the internal file
      read(iFile,*) cx1, cx2, cx3, cx4, cx5

      ! validate values read in

      ! validate cx1: (+NaN(Q), -Inf)
      rl1r = qreal(cx1)
      rl1i = qimag(cx1)

      if ( ( .not. ieee_is_nan( rl1re ) ) .or.                         &
     &     ( ieee_class( rl1re ) .ne. ieee_quiet_nan ) .or.            &
     &     ( ii1r .le. 0 ) ) error stop 1_4

      if ( ieee_is_finite(rl1ie) .or. (.not.ieee_is_negative(rl1ie)) ) &
     &     error stop 2_4

      ! validate cx2: (+Inf, -NaN(S))
      rl2r = qreal(cx2)
      rl2i = qimag(cx2)

      if ( ieee_is_finite( rl2re ) .or. ieee_is_negative( rl2re ) )    &
     &     error stop 3_4

      if ( ( .not. ieee_is_nan( rl2ie ) ) .or.                         &
     &     ( ieee_class( rl2ie ) .ne. ieee_signaling_nan ) .or.        &
     &     ( ii2i .ge. 0 ) ) error stop 4_4

      ! validate cx3: (+NaN(Q), -NaN(Q))
      rl3r = qreal(cx3)
      rl3i = qimag(cx3)

      if ( ( .not. ieee_is_nan( rl3re ) ) .or.                         &
     &     ( ieee_class( rl3re ) .ne. ieee_quiet_nan ) .or.            &
     &     ( ii3r .le. 0 ) ) error stop 5_4

      if ( ( .not. ieee_is_nan( rl3ie ) ) .or.                         &
     &     ( ieee_class( rl3ie ) .ne. ieee_quiet_nan ) .or.            &
     &     ( ii3i .ge. 0 ) ) error stop 6_4

      ! validate cx4: (-NaN(Q), +Inf)
      rl4r = qreal(cx4)
      rl4i = qimag(cx4)

      if ( ( .not. ieee_is_nan( rl4re ) ) .or.                         &
     &     ( ieee_class( rl4re ) .ne. ieee_quiet_nan ) .or.            &
     &     ( ii4r .ge. 0 ) ) error stop 7_4

      if ( ieee_is_finite( rl4ie ) .or. ieee_is_negative( rl4ie ) )    &
     &     error stop 8_4

      ! validate cx5: (-Inf, +Inf)
      rl5r = qreal(cx5)
      rl5i = qimag(cx5)

      if ( ieee_is_finite(rl5re) .or. (.not.ieee_is_negative(rl5re)) ) &
     &     error stop 9_4

      if ( ieee_is_finite( rl5ie ) .or. ieee_is_negative( rl5ie ) )    &
     &     error stop 10_4


      end
