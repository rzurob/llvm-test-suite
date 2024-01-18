!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : miscNaNInfIO017.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : July 26, 2006
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
!*  When reading IEEE NaN and Inf values and an arbitrary string of characters
!*  is encountered, zeros must be read in place of it. Please see defect 323885.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      
      use, intrinsic     :: ieee_arithmetic
      implicit none

      complex(4)         :: cx1
      real(4)            :: rl1, rl2
      
      integer, parameter :: in = 11

      logical, external  :: precision_r4
      
      open(unit=in, file='miscNaNInfIO017.dat', action='read')
      
      rl1 = -1.0
      rl2 = -1.0
      
      read(in, *) rl1, rl2
      
      if ( (.not. ieee_is_nan(rl1)) .or.                               &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) ) stop 1
      
      if ( .not. precision_r4(rl2, 0.0_4) ) stop 2
      
      rl1 = -1.0
      rl2 = -1.0
      cx1 = (-1.0, -1.0)
      
      read(in, *) cx1, rl1

      if ( .not. precision_r4(real(cx1), -3.14) ) stop 3
      
      if ( ieee_is_finite(imag(cx1)) ) stop 4
      
      if ( .not. precision_r4(rl1, 0.0_4) ) stop 5

      end
