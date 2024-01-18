!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : outputRealInf001.f
!*
!*  DATE                       : June 9, 2006
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
!*  Test output of IEEE Infinity (positive vs. negative)
!*  for reals(4)/(8)/(16) with the F edit descriptor.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      real(4)  :: rl1
      real(8)  :: rl2, rl3equiv
      real(16) :: rl3

      integer, parameter :: unit = 11

      equivalence(rl3, rl3equiv)

      open(unit, file='outputRealInf001.out', action='write')

      ! Write out real(4) IEEE Infinity ( positive and negative )

      rl1 = b'01111111100000000000000000000000' ! positive Inf
      write(unit, '(F8.2)') rl1
      rl1 = b'11111111100000000000000000000000' ! negative Inf
      write(unit, '(F8.2)') rl1

      ! Write out real(8) IEEE Infinity ( positive and negative )

      rl2 = z'7FF0000000000000' ! positive Inf
      write(unit, '(F8.2)') rl2
      rl2 = z'FFF0000000000000' ! negative Inf
      write(unit, '(F8.2)') rl2

      ! Write out real(16) IEEE Infinity ( positive and negative )

      rl3equiv = z'7FF0000000000000' ! positive Inf
      write(unit, '(F8.2)') rl3
      rl3equiv = z'FFF0000000000000' ! negative Inf
      write(unit, '(F8.2)') rl3

      close(unit)

      end
