!*********************************************************************
!*  ===================================================================
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
!*  for complex(4)/(8)/(16) with the ES edit descriptor.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      complex(4)  :: cx1
      complex(8)  :: cx2
      complex(16) :: cx3
      real(4)  :: rl1r, rl1i
      real(8)  :: rl2r, rl2i, rl3requiv, rl3iequiv
      real(16) :: rl3r, rl3i

      integer, parameter :: unit = 11

      equivalence(rl3r, rl3requiv)
      equivalence(rl3i, rl3iequiv)

      open(unit, file='outputComplexInf005.out', action='write')

      ! Write out complex(4) with real and imaginary parts consisting of
      ! combinations of positive and negative infinity values

      rl1r = b'01111111100000000000000000000000' ! positive Inf
      rl1i = b'01111111100000000000000000000000'
      cx1 = (rl1r, rl1i)
      write(unit, '(2ES8.2)') cx1

      rl1r = b'11111111100000000000000000000000' ! negative Inf
      rl1i = b'11111111100000000000000000000000'
      cx1 = (rl1r, rl1i)
      write(unit, '(2ES8.2)') cx1

      rl1r = b'11111111100000000000000000000000' ! negative Inf
      rl1i = b'01111111100000000000000000000000' ! positive Inf
      cx1 = (rl1r, rl1i)
      write(unit, '(2ES8.2)') cx1

      rl1r = b'01111111100000000000000000000000' ! positive Inf
      rl1i = b'11111111100000000000000000000000' ! negative Inf
      cx1 = (rl1r, rl1i)
      write(unit, '(2ES8.2)') cx1

      ! Write out complex(8) with real and imaginary parts consisting of
      ! combinations of positive and negative infinity values

      rl2r = z'7FF0000000000000' ! positive Inf
      rl2i = z'7FF0000000000000'
      cx2 = (rl2r, rl2i)
      write(unit, '(2ES8.2)') cx2

      rl2r = z'FFF0000000000000' ! negative Inf
      rl2i = z'FFF0000000000000'
      cx2 = (rl2r, rl2i)
      write(unit, '(2ES8.2)') cx2

      rl2r =  z'FFF0000000000000' ! negative Inf
      rl2i =  z'7FF0000000000000' ! positive Inf
      cx2 = (rl2r, rl2i)
      write(unit, '(2ES8.2)') cx2

      rl2r =  z'7FF0000000000000' ! positive Inf
      rl2i =  z'FFF0000000000000' ! negative Inf
      cx2 = (rl2r, rl2i)
      write(unit, '(2ES8.2)') cx2

      ! Write out complex(16) with real and imaginary parts consisting of
      ! combinations of positive and negative infinity values

      rl3requiv = z'7FF0000000000000' ! positive Inf
      rl3iequiv = z'7FF0000000000000'
      cx3 = (rl3r, rl3i)
      write(unit, '(2ES8.2)') cx3

      rl3r = z'FFF0000000000000' ! negative Inf
      rl3i = z'FFF0000000000000'
      cx3 = (rl3r, rl3i)
      write(unit, '(2ES8.2)') cx3

      rl3r =  z'FFF0000000000000' ! negative Inf
      rl3i =  z'7FF0000000000000' ! positive Inf
      cx3 = (rl3r, rl3i)
      write(unit, '(2ES8.2)') cx3

      rl3r =  z'7FF0000000000000' ! positive Inf
      rl3i =  z'FFF0000000000000' ! negative Inf
      cx3 = (rl3r, rl3i)
      write(unit, '(2ES8.2)') cx3


      close(unit)

      end

