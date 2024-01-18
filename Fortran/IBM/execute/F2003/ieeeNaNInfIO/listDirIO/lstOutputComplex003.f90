!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : lstOutputComplex003.f
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
!*  are placed inside objects of type complex and kind 16.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: out = 11

      real(16)      :: rl1, rl2, rl3, rl4
      complex(8)    :: rl1e, rl2e, rl3e, rl4e
      complex(16)   :: cx1, cx2
      integer      :: i1 = 11
      character(3) :: c1='IBM'

      equivalence(rl1, rl1e)
      equivalence(rl2, rl2e)
      equivalence(rl3, rl3e)
      equivalence(rl4, rl4e)

      open(out, file='lstOutputComplex003.out', action='write')

      rl1e = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl2e = z'7FF0000000000000' ! positive Inf
      rl3e = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl4e = z'FFF0000000000000' ! negative Inf
      cx1 = (rl1, rl2)
      cx2 = (rl3, rl4)
      write(out,*) c1, cx1, i1, cx2

      rl1e = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2e = z'7FF0000000000000' ! positive Inf
      rl3e = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl4e = z'FFF0000000000000' ! negative Inf
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 12
      c1 = 'ibm'
      write(out,*) c1, cx1, i1, cx2

      rl1e = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2e = z'7FF0000000000000' ! positive Inf
      rl3e = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl4e = z'FFF0000000000000' ! negative Inf
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 13
      c1 = 'IBM'
      write(out,*) c1, cx1, i1, cx2

      rl1e = z'FFF0000000000000' ! negative Inf
      rl2e = z'7FF0000000000000' ! positive Inf
      rl3e = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl4e = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 14
      c1 = 'ibm'
      write(out,*) c1, cx1, i1, cx2

      rl1e = z'7FF0000000000000' ! positive Inf
      rl2e = z'FFF0000000000000' ! negative Inf
      rl3e = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl4e = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      write(out,*) cx1, cx2

      rl1e = z'7FF0000000000000' ! positive Inf
      rl2e = z'7FF0000000000000' ! positive Inf
      rl3e = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl4e = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      write(out,*) cx1, cx2

      rl1e = z'FFF0000000000000' ! negative Inf
      rl2e = z'FFF0000000000000' ! negative Inf
      rl3e = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl4e = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      write(out,*) cx1, cx2

      rl1e = z'FFF0000000000000' ! negative Inf
      rl2e = 3.0
      rl3e = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl4e = 2.0
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 15
      c1 = 'IBM'
      write(out,*) c1, cx1, i1, cx2

      rl1e = -2.0
      rl2e = z'7FF0000000000000' ! positive Inf
      rl3e = 3.0
      rl4e = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      cx1 = ( rl1, rl2)
      cx2 = ( rl3, rl4)
      i1 = 16
      c1 = 'ibm'
      write(out,*) c1, cx1, i1, cx2


      close(out)

      end
