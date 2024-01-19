!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 12, 2006
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
!*  Boundary testing for different widths of the output field.
!*  This testcase covers output of IEEE NaN (quiet vs. signaling &
!*  positive vs. negative) for reals(4)/(8)/(16) with the Q edit descriptor.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      real(4)  :: rl1(8)
      real(8)  :: rl2(8), rl3equiv
      real(16) :: rl3
      integer  :: i = 0

      integer, parameter :: unit = 11

      character(64), parameter :: myfmt =                              &
     & '(Q1.1,/,Q2.1,/,Q3.2,/,Q4.2,/,Q5.2,/,Q6.2,/,Q7.2,/,Q15.2)'

      equivalence(rl3, rl3equiv)

      open(unit, file='boundaryRealNaN006.out', action='write')

      ! Write out real(4) quiet NaN ( positive and negative )

      rl1 = b'01111111111111111111111111111111' ! positive NaN(Q)
      write(unit, fmt=myfmt) rl1

      rl1 = b'11111111111111111111111111111111' ! negative NaN(Q)
      write(unit, fmt=myfmt) rl1

      ! Write out real(8) quiet NaN ( positive and negative )

      rl2 = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      write(unit, fmt=myfmt) rl2

      rl2 = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      write(unit, fmt=myfmt) rl2

      ! Write out real(16) quiet NaN ( positive and negative )

      rl3equiv = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      write(unit, fmt=myfmt) rl3, rl3, rl3, rl3, rl3, rl3, rl3, rl3

      rl3equiv = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      write(unit, fmt=myfmt) rl3, rl3, rl3, rl3, rl3, rl3, rl3, rl3


     !*********************************************************


      ! Write out real(4) signaling NaN ( positive and negative )

      rl1 = b'01111111101111111111111111111111' ! positive NaN(S)
      write(unit, fmt=myfmt) rl1

      rl1 = b'11111111101111111111111111111111' ! negative NaN(S)
      write(unit, fmt=myfmt) rl1

      ! Write out real(8) signaling NaN ( positive and negative )

      rl2 = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      write(unit, fmt=myfmt) rl2

      rl2 = z'FFF7FFFFFFFFFFFF' ! negative NaN(Q)
      write(unit, fmt=myfmt) rl2

      ! Write out real(16) signaling NaN ( positive and negative )

      rl3equiv = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      write(unit, fmt=myfmt) rl3, rl3, rl3, rl3, rl3, rl3, rl3, rl3

      rl3equiv = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      write(unit, fmt=myfmt) rl3, rl3, rl3, rl3, rl3, rl3, rl3, rl3

      close(unit)

      end
