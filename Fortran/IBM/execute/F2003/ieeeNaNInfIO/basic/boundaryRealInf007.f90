!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : boundaryRealInf007.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 12, 2006
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
!*  Boundary testing for different widths of the output field.
!*  This testcase covers output of IEEE Infinity (positive vs.
!*  negative) for reals(4)/(8)/(16) with the G edit descriptor.
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
     & '(G1.1,/,G2.1,/,G3.2,/,G4.2,/,G5.2,/,G8.2,/,G9.2,/,G15.2)'

      equivalence(rl3, rl3equiv)

      open(unit, file='boundaryRealInf007.out', action='write')
      
      ! Write out real(4) IEEE Infinity ( positive and negative )

      rl1 = b'01111111100000000000000000000000' ! positive Inf
      write(unit, fmt=myfmt) rl1

      rl1 = b'11111111100000000000000000000000' ! negative Inf
      write(unit, fmt=myfmt) rl1

      ! Write out real(8) IEEE Infinity ( positive and negative )

      rl2 = z'7FF0000000000000' ! positive Infinity
      write(unit, fmt=myfmt) rl2

      rl2 = z'FFF0000000000000' ! negative Infinity
      write(unit, fmt=myfmt) rl2

      ! Write out real(16) IEEE Infinity ( positive and negative )

      rl3equiv = z'7FF0000000000000' ! positive Infinity
      write(unit, fmt=myfmt) rl3, rl3, rl3, rl3, rl3, rl3, rl3, rl3

      rl3equiv = z'FFF0000000000000' ! negative Infinity
      write(unit, fmt=myfmt) rl3, rl3, rl3, rl3, rl3, rl3, rl3, rl3

      close(unit)

      end
