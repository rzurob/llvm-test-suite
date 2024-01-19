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
!*  Testing IEEE NaN and Inf output using PRINT statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      real(4) :: nanq_pos_4, nanq_neg_4, nans_pos_4,                   &
     &           nans_neg_4, inf_pos_4, inf_neg_4

      real(8) :: nanq_pos_8, nanq_neg_8, nans_pos_8,                   &
     &           nans_neg_8, inf_pos_8, inf_neg_8

      real(16) :: nanq_pos_6, nanq_neg_6, nans_pos_6,                  &
     &           nans_neg_6, inf_pos_6, inf_neg_6

      complex(4)  :: cx4
      complex(8)  :: cx8
      complex(16) :: cx6

      nanq_pos_4 = b'01111111111111111111111111111111'
      nanq_pos_8 = z'7FFFFFFFFFFFFFFF'
      nanq_pos_6 = z'7FFFFFFFFFFFFFFF7FFFFFFFFFFFFFFF'

      nanq_neg_4 = b'11111111111111111111111111111111'
      nanq_neg_8 = z'FFFFFFFFFFFFFFFF'
      nanq_neg_6 = z'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'

      nans_pos_4 = b'01111111101111111111111111111111'
      nans_pos_8 = z'7FF7FFFFFFFFFFFF'
      nans_pos_6 = z'7FF7FFFFFFFFFFFF7FF7FFFFFFFFFFFF'

      nans_neg_4 = b'11111111101111111111111111111111'
      nans_neg_8 = z'FFF7FFFFFFFFFFFF'
      nans_neg_6 = z'FFF7FFFFFFFFFFFFFFF7FFFFFFFFFFFF'

      inf_pos_4 =  b'01111111100000000000000000000000'
      inf_pos_8 =  z'7FF0000000000000'
      inf_pos_6 =  z'7FF00000000000007FF0000000000000'

      inf_neg_4 =  b'11111111100000000000000000000000'
      inf_neg_8 =  z'FFF0000000000000'
      inf_neg_6 =  z'FFF0000000000000FFF0000000000000'

      cx4 = (inf_neg_4, nans_pos_4)
      cx8 = (nanq_neg_8, inf_pos_8)
      cx6 = (nans_neg_6, nanq_pos_6)

      ! list directed output:

      print *, nanq_pos_4, nanq_neg_4, nans_pos_4, nans_neg_4,         &
     &         inf_pos_4, inf_neg_4

      print *, nanq_pos_8, nanq_neg_8, nans_pos_8, nans_neg_8,         &
     &         inf_pos_8, inf_neg_8

      print *, nanq_pos_6, nanq_neg_6, nans_pos_6, nans_neg_6,         &
     &         inf_pos_6, inf_neg_6

      print *, cx4, cx8, cx6

      ! format-directed outut:

      print 100, nanq_pos_4, nanq_neg_4, nans_pos_4, nans_neg_4,       &
     &         inf_pos_4, inf_neg_4

      print 100, nanq_pos_8, nanq_neg_8, nans_pos_8, nans_neg_8,       &
     &         inf_pos_8, inf_neg_8

      print 100, nanq_pos_6, nanq_neg_6, nans_pos_6, nans_neg_6,       &
     &         inf_pos_6, inf_neg_6

      print '(6F10.2)', cx4, cx8, cx6

 100  format(D10.2, ES10.2, EN10.2, G10.2, F10.2, Q10.2)

      ! trying some variations of edit descriptors and modes

      print '(en3.2, es5.2, en4.2)', inf_neg_6, nans_pos_4, inf_neg_8

      print '(dc, ru, sp, g7.6, d10.2)', nanq_pos_8, inf_pos_6

      print '(dp, dc, sp, e6.6, 2ES3.2, ss, ES3.2)', nans_pos_4, cx8,  &
     &                                               inf_pos_6

      end
