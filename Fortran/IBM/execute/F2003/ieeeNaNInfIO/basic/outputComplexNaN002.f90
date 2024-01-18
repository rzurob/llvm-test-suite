!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : outputComplexNaN002.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 9, 2006
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
!*  Test output of IEEE NaN (quiet vs. signaling, positive vs. negative)
!*  for complex(4)/(8)/(16) with the E edit descriptor.
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

      open(unit, file='outputComplexNaN002.out', action='write')
      
      ! Write out complex(4) with real and imaginary parts consisting of
      ! combinations of quiet vs. signaling NaN ( positive & negative )

      rl1r = b'01111111111111111111111111111111' ! positive NaN(Q)
      rl1i = b'01111111111111111111111111111111'
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'11111111111111111111111111111111' ! negative NaN(Q)
      rl1i = b'11111111111111111111111111111111'
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'11111111111111111111111111111111' ! negative NaN(Q)
      rl1i = b'01111111111111111111111111111111' ! positive NaN(Q)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'01111111111111111111111111111111' ! positive NaN(Q)
      rl1i = b'11111111111111111111111111111111' ! negative NaN(Q)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'01111111101111111111111111111111' ! positive NaN(S)
      rl1i = b'01111111101111111111111111111111' ! positive NaN(S)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'11111111101111111111111111111111' ! negative NaN(S)
      rl1i = b'11111111101111111111111111111111'
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'11111111101111111111111111111111' ! negative NaN(S)
      rl1i = b'01111111101111111111111111111111' ! positive NaN(S)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'01111111101111111111111111111111' ! positive NaN(S)
      rl1i = b'11111111101111111111111111111111' ! negative NaN(S)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1


      rl1r = b'01111111111111111111111111111111' ! positive NaN(Q)
      rl1i = b'01111111101111111111111111111111' ! positive NaN(S)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'11111111111111111111111111111111' ! negative NaN(Q)
      rl1i = b'11111111101111111111111111111111' ! negative NaN(S)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'11111111111111111111111111111111' ! negative NaN(Q)
      rl1i = b'01111111101111111111111111111111' ! positive NaN(S)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'01111111111111111111111111111111' ! positive NaN(Q)
      rl1i = b'11111111101111111111111111111111' ! negative NaN(S)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'01111111101111111111111111111111' ! positive NaN(S)
      rl1i = b'01111111111111111111111111111111' ! positive NaN(Q)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'11111111101111111111111111111111' ! negative NaN(S)
      rl1i = b'11111111111111111111111111111111' ! negative NaN(Q)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'11111111101111111111111111111111' ! negative NaN(S)
      rl1i = b'01111111111111111111111111111111' ! positive NaN(Q)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1

      rl1r = b'01111111101111111111111111111111' ! positive NaN(S)
      rl1i = b'11111111111111111111111111111111' ! negative NaN(Q)
      cx1 = (rl1r, rl1i)
      write(unit, '(2E8.2)') cx1


      ! Write out complex(8) with real and imaginary parts consisting of
      ! combinations of quiet vs. signaling NaN ( positive & negative )

      rl2r = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2i = z'7FFFFFFFFFFFFFFF'
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl2i = z'FFFFFFFFFFFFFFFF'
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r =  z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl2i =  z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r =  z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2i =  z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl2i = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl2i = z'FFF7FFFFFFFFFFFF'
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl2i = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl2i = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2


      rl2r = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2i = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl2i = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl2i = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2i = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl2i = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl2i = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl2i = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2

      rl2r = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl2i = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx2 = (rl2r, rl2i)
      write(unit, '(2E8.2)') cx2


      ! Write out complex(16) with real and imaginary parts consisting of
      ! combinations of quiet vs. signaling NaN ( positive & negative )

      rl3requiv = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl3iequiv = z'7FFFFFFFFFFFFFFF'
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl3iequiv = z'FFFFFFFFFFFFFFFF'
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv =  z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl3iequiv =  z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv =  z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl3iequiv =  z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl3iequiv = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl3iequiv = z'FFF7FFFFFFFFFFFF'
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl3iequiv = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl3iequiv = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3


      rl3requiv = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl3iequiv = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl3iequiv = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl3iequiv = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl3iequiv = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl3iequiv = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl3iequiv = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl3iequiv = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3

      rl3requiv = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl3iequiv = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx3 = (rl3r, rl3i)
      write(unit, '(2E8.2)') cx3


      close(unit)
      
      end

