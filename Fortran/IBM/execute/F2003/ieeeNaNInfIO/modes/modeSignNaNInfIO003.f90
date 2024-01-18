!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : modeSignNaNInfIO003.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 28, 2006
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
!*  Make sure SIGN mode does not affect the input of NaN and Inf.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11
      
      real(4)    :: rl1, rl2, real_part, imag_part
      integer(4) :: ii1, ii2, real_eq, imag_eq
      complex(4) :: cx1, cx2
      
      equivalence(rl1, ii1)
      equivalence(rl2, ii2)
      equivalence(real_part, real_eq)
      equivalence(imag_part, imag_eq)

      open(in, file='modeSignNaNInfIO003.dat', action='read')

      rl1 = 0.0
      rl2 = 0.0
      cx1 = (0.0, 0.0)
      cx2 = (0.0, 0.0)
      read(in, '(sp, 6f9.1)') rl1, rl2, cx1, cx2
      
      ! rl1 should be +NaN(Q)
      if ( ( .not. ieee_is_nan( rl1 ) ) .or.                           &
     &     ( ieee_class( rl1 ) .ne. ieee_quiet_nan ) .or.              &
     &     ( ii1 .le. 0 ) ) error stop 1_4
      
      ! rl2 should be -Inf      
      if ( ieee_is_finite( rl2 ) .or. .not.ieee_is_negative( rl2 ) )   &
     &     error stop 2_4
      
      ! cx1 should be (-NaN(Q),-NaN(S))
      real_part = real(cx1)
      imag_part = imag(cx1)

      if ( ( .not. ieee_is_nan( real_part ) ) .or.                     &
     &     ( ieee_class( real_part ) .ne. ieee_quiet_nan ) .or.        &
     &     ( real_eq .ge. 0 ) ) error stop 3_4

      if ( ( .not. ieee_is_nan( imag_part ) ) .or.                     &
     &     ( ieee_class( imag_part ) .ne. ieee_signaling_nan ) .or.    &
     &     ( imag_eq .ge. 0 ) ) error stop 4_4

      ! cx2 should be (+Inf,+Inf)
      real_part = real(cx2)
      imag_part = imag(cx2)

      if ( ieee_is_finite(real_part) .or. ieee_is_negative(real_part) )&
     &     error stop 5_4

      if ( ieee_is_finite(imag_part) .or. ieee_is_negative(imag_part) )&
     &     error stop 6_4

      

      close(in)


      end
