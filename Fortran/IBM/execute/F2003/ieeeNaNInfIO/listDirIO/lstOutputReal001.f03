!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 23, 2006
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
!*  Test output of IEEE NaN and Inf for list-directed I/O.
!*  In this testcase IEEE exceptional specifications
!*  are placed inside objects of type real and kind 4.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: out = 11

      real(4)      :: rl1, rl2, rl3, rl4
      integer      :: i1 = 11, i2 = 22
      character(3) :: c1='IBM', c2='xlf'

      open(out, file='lstOutputReal001.out', action='write')

      rl1 = b'01111111101111111111111111111111' ! positive NaN(S)
      rl2 = b'01111111100000000000000000000000' ! positive Inf
      rl3 = b'11111111101111111111111111111111' ! negative NaN(S)
      rl4 = b'11111111100000000000000000000000' ! negative Inf
      write(out,*) c1, rl1, i1, rl2, i2, rl3, c2, rl4

      rl1 = b'01111111111111111111111111111111' ! positive NaN(Q)
      rl2 = b'01111111100000000000000000000000' ! positive Inf
      rl3 = b'11111111111111111111111111111111' ! negative NaN(Q)
      rl4 = b'11111111100000000000000000000000' ! negative Inf
      i1 = 13; i2 = 14
      c1 = 'ibm'; c2='XLF'
      write(out,*) c1, rl1, i1, rl2, i2, rl3, c2, rl4

      rl1 = b'01111111111111111111111111111111' ! positive NaN(Q)
      rl2 = b'01111111100000000000000000000000' ! positive Inf
      rl3 = b'11111111101111111111111111111111' ! negative NaN(S)
      rl4 = b'11111111100000000000000000000000' ! negative Inf
      i1 = 15; i2 = 16
      c1 = 'IBM'; c2='xlf'
      write(out,*) c1, rl1, i1, rl2, i2, rl3, c2, rl4

      rl1 = b'11111111100000000000000000000000' ! negative Inf
      rl2 = b'01111111100000000000000000000000' ! positive Inf
      rl3 = b'01111111101111111111111111111111' ! positive NaN(S)
      rl4 = b'11111111111111111111111111111111' ! negative NaN(Q)
      i1 = 17; i2 = 18
      c1 = 'ibm'; c2='XLF'
      write(out,*) c1, rl1, i1, rl2, i2, rl3, c2, rl4

      rl1 = b'01111111100000000000000000000000' ! positive Inf
      rl2 = b'11111111100000000000000000000000' ! negative Inf
      rl3 = b'01111111101111111111111111111111' ! positive NaN(S)
      rl4 = b'11111111111111111111111111111111' ! negative NaN(Q)
      write(out,*) rl1, rl2, rl3, rl4

      rl1 = 3.0
      rl2 = b'11111111100000000000000000000000' ! negative Inf
      rl3 = 2.0
      rl4 = b'11111111111111111111111111111111' ! negative NaN(Q)
      i1 = 19; i2 = 20
      c1 = 'IBM'; c2='xlf'
      write(out,*) c1, rl1, i1, rl2, i2, rl3, c2, rl4


      close(out)

      end