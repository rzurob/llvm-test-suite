!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : lstOutputComplex001.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 23, 2006
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
!*  Test output of IEEE NaN and Inf for list-directed I/O.
!*  In this testcase IEEE exceptional specifications
!*  are placed inside objects of type complex and kind 4.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: out = 11

      real(4)      :: rl1, rl2, rl3, rl4
      complex(4)   :: cx1, cx2
      integer      :: i1 = 11
      character(3) :: c1='IBM'
      
      open(out, file='lstOutputComplex001.out', action='write')
      
      rl1 = b'01111111101111111111111111111111' ! positive NaN(S)
      rl2 = b'01111111100000000000000000000000' ! positive Inf
      rl3 = b'11111111101111111111111111111111' ! negative NaN(S)
      rl4 = b'11111111100000000000000000000000' ! negative Inf
      cx1 = (rl1, rl2)
      cx2 = (rl3, rl4)
      write(out,*) c1, cx1, i1, cx2

      rl1 = b'01111111111111111111111111111111' ! positive NaN(Q)
      rl2 = b'01111111100000000000000000000000' ! positive Inf
      rl3 = b'11111111111111111111111111111111' ! negative NaN(Q)
      rl4 = b'11111111100000000000000000000000' ! negative Inf
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 12
      c1 = 'ibm'
      write(out,*) c1, cx1, i1, cx2
      
      rl1 = b'01111111111111111111111111111111' ! positive NaN(Q)
      rl2 = b'01111111100000000000000000000000' ! positive Inf
      rl3 = b'11111111101111111111111111111111' ! negative NaN(S)
      rl4 = b'11111111100000000000000000000000' ! negative Inf
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 13
      c1 = 'IBM'
      write(out,*) c1, cx1, i1, cx2

      rl1 = b'11111111100000000000000000000000' ! negative Inf
      rl2 = b'01111111100000000000000000000000' ! positive Inf
      rl3 = b'01111111101111111111111111111111' ! positive NaN(S)
      rl4 = b'11111111111111111111111111111111' ! negative NaN(Q)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 14
      c1 = 'ibm'
      write(out,*) c1, cx1, i1, cx2

      rl1 = b'01111111100000000000000000000000' ! positive Inf
      rl2 = b'11111111100000000000000000000000' ! negative Inf
      rl3 = b'01111111101111111111111111111111' ! positive NaN(S)
      rl4 = b'11111111111111111111111111111111' ! negative NaN(Q)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      write(out,*) cx1, cx2

      rl1 = b'01111111100000000000000000000000' ! positive Inf
      rl2 = b'01111111100000000000000000000000' ! positive Inf
      rl3 = b'01111111101111111111111111111111' ! positive NaN(S)
      rl4 = b'11111111101111111111111111111111' ! negative NaN(S)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      write(out,*) cx1, cx2

      rl1 = b'11111111100000000000000000000000' ! negative Inf
      rl2 = b'11111111100000000000000000000000' ! negative Inf
      rl3 = b'01111111111111111111111111111111' ! positive NaN(Q)
      rl4 = b'11111111111111111111111111111111' ! negative NaN(Q)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      write(out,*) cx1, cx2

      rl1 = b'11111111100000000000000000000000' ! negative Inf
      rl2 = 3.0
      rl3 = b'11111111111111111111111111111111' ! negative NaN(Q)
      rl4 = 2.0
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 15
      c1 = 'IBM'
      write(out,*) c1, cx1, i1, cx2

      rl1 = -2.0
      rl2 = b'01111111100000000000000000000000' ! positive Inf
      rl3 = 3.0
      rl4 = b'11111111101111111111111111111111' ! negative NaN(S)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 16
      c1 = 'ibm'
      write(out,*) c1, cx1, i1, cx2


      close(out)

      end
