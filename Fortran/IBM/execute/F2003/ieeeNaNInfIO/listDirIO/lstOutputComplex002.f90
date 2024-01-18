!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : lstOutputComplex002.f
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
!*  are placed inside objects of type complex and kind 8.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: out = 11

      real(8)      :: rl1, rl2, rl3, rl4
      complex(8)   :: cx1, cx2
      integer      :: i1 = 11
      character(3) :: c1='IBM'

      open(out, file='lstOutputComplex002.out', action='write')

      rl1 = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl2 = z'7FF0000000000000' ! positive Inf
      rl3 = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl4 = z'FFF0000000000000' ! negative Inf
      cx1 = (rl1, rl2)
      cx2 = (rl3, rl4)
      write(out,*) c1, cx1, i1, cx2

      rl1 = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2 = z'7FF0000000000000' ! positive Inf
      rl3 = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl4 = z'FFF0000000000000' ! negative Inf
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 12
      c1 = 'ibm'
      write(out,*) c1, cx1, i1, cx2

      rl1 = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2 = z'7FF0000000000000' ! positive Inf
      rl3 = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl4 = z'FFF0000000000000' ! negative Inf
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 13
      c1 = 'IBM'
      write(out,*) c1, cx1, i1, cx2

      rl1 = z'FFF0000000000000' ! negative Inf
      rl2 = z'7FF0000000000000' ! positive Inf
      rl3 = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl4 = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 14
      c1 = 'ibm'
      write(out,*) c1, cx1, i1, cx2

      rl1 = z'7FF0000000000000' ! positive Inf
      rl2 = z'FFF0000000000000' ! negative Inf
      rl3 = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl4 = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      write(out,*) cx1, cx2

      rl1 = z'7FF0000000000000' ! positive Inf
      rl2 = z'7FF0000000000000' ! positive Inf
      rl3 = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl4 = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      write(out,*) cx1, cx2

      rl1 = z'FFF0000000000000' ! negative Inf
      rl2 = z'FFF0000000000000' ! negative Inf
      rl3 = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl4 = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      write(out,*) cx1, cx2

      rl1 = z'FFF0000000000000' ! negative Inf
      rl2 = 3.0
      rl3 = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl4 = 2.0
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 15
      c1 = 'IBM'
      write(out,*) c1, cx1, i1, cx2

      rl1 = -2.0
      rl2 = z'7FF0000000000000' ! positive Inf
      rl3 = 3.0
      rl4 = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 16
      c1 = 'ibm'
      write(out,*) c1, cx1, i1, cx2


      close(out)

      end
