!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : intLstInputComplex001.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 27, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  DRIVER STANZA              :
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
!*  of type COMPLEX and kind 4.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      character(200) :: iFile =                                        &
     &     '(nan(q),-inf) (+infinity, -nan(s)) (nan(_abc13), -nan())'  &
     &  // ' (-nan,+inf) (-infinity,+inf)'
      
      complex(4) :: cx1, cx2, cx3, cx4, cx5

      real(4) :: rl1i, rl2i, rl3i, rl4i, rl5i ! imaginary parts
      real(4) :: rl1r, rl2r, rl3r, rl4r, rl5r ! real parts
      
      integer(4) :: ii1i, ii2i, ii3i, ii4i, ii5i ! integer equivalence of imaginary parts
      integer(4) :: ii1r, ii2r, ii3r, ii4r, ii5r ! integer equivalence of real parts
      
      equivalence(rl1i, ii1i); equivalence(rl2i, ii2i)
      equivalence(rl3i, ii3i); equivalence(rl4i, ii4i)
      equivalence(rl5i, ii5i)

      equivalence(rl1r, ii1r); equivalence(rl2r, ii2r)
      equivalence(rl3r, ii3r); equivalence(rl4r, ii4r)
      equivalence(rl5r, ii5r)
      
      ! initialize variables:
      cx1 = (0.0, 0.0); cx2 = (0.0, 0.0); cx3 = (0.0, 0.0)
      cx4 = (0.0, 0.0); cx5 = (0.0, 0.0)

      ! read from the internal file
      read(iFile,*) cx1, cx2, cx3, cx4, cx5

      ! validate values read in
      
      ! validate cx1: (+NaN(Q), -Inf)
      rl1r = real(cx1)
      rl1i = imag(cx1)
      
      if ( ( .not. ieee_is_nan( rl1r ) ) .or.                          &
     &     ( ieee_class( rl1r ) .ne. ieee_quiet_nan ) .or.             &
     &     ( ii1r .le. 0 ) ) error stop 1_4

      if ( ieee_is_finite( rl1i ) .or. (.not.ieee_is_negative(rl1i)) ) &
     &     error stop 2_4
      
      ! validate cx2: (+Inf, -NaN(S))
      rl2r = real(cx2)
      rl2i = imag(cx2)

      if ( ieee_is_finite( rl2r ) .or. ieee_is_negative( rl2r ) )      &
     &     error stop 3_4
      
      if ( ( .not. ieee_is_nan( rl2i ) ) .or.                          &
     &     ( ieee_class( rl2i ) .ne. ieee_signaling_nan ) .or.         &
     &     ( ii2i .ge. 0 ) ) error stop 4_4

      ! validate cx3: (+NaN(Q), -NaN(Q))
      rl3r = real(cx3)
      rl3i = imag(cx3)
      
      if ( ( .not. ieee_is_nan( rl3r ) ) .or.                          &
     &     ( ieee_class( rl3r ) .ne. ieee_quiet_nan ) .or.             &
     &     ( ii3r .le. 0 ) ) error stop 5_4

      if ( ( .not. ieee_is_nan( rl3i ) ) .or.                          &
     &     ( ieee_class( rl3i ) .ne. ieee_quiet_nan ) .or.             &
     &     ( ii3i .ge. 0 ) ) error stop 6_4

      ! validate cx4: (-NaN(Q), +Inf)
      rl4r = real(cx4)
      rl4i = imag(cx4)

      if ( ( .not. ieee_is_nan( rl4r ) ) .or.                          &
     &     ( ieee_class( rl4r ) .ne. ieee_quiet_nan ) .or.             &
     &     ( ii4r .ge. 0 ) ) error stop 7_4

      if ( ieee_is_finite( rl4i ) .or. ieee_is_negative( rl4i ) )      &
     &     error stop 8_4
      
      ! validate cx5: (-Inf, +Inf)
      rl5r = real(cx5)
      rl5i = imag(cx5)
      
      if ( ieee_is_finite( rl5r ) .or. (.not.ieee_is_negative(rl5r)) ) &
     &     error stop 9_4

      if ( ieee_is_finite( rl5i ) .or. ieee_is_negative( rl5i ) )      &
     &     error stop 10_4

      
      end
