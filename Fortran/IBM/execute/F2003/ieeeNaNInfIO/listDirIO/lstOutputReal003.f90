!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : lstOutputReal003.f
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
!*  are placed inside objects of type real and kind 16.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: out = 11

      real(16)      :: rl1, rl2, rl3, rl4
      real(8)       :: rl1e, rl2e, rl3e, rl4e
      integer      :: i1 = 11, i2 = 22
      character(3) :: c1='IBM', c2='xlf'
      
      equivalence(rl1, rl1e)
      equivalence(rl2, rl2e)
      equivalence(rl3, rl3e)
      equivalence(rl4, rl4e)
      
      open(out, file='lstOutputReal003.out', action='write')
      
      rl1 = 0
      rl2 = 0
      rl3 = 0
      rl4 = 0

      rl1e = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl2e = z'7FF0000000000000' ! positive Inf
      rl3e = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl4e = z'FFF0000000000000' ! negative Inf
      write(out,*) c1, rl1, i1, rl2, i2, rl3, c2, rl4

      rl1e = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2e = z'7FF0000000000000' ! positive Inf
      rl3e = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl4e = z'FFF0000000000000' ! negative Inf
      i1 = 13; i2 = 14
      c1 = 'ibm'; c2='XLF'
      write(out,*) c1, rl1, i1, rl2, i2, rl3, c2, rl4
      
      rl1e = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2e = z'7FF0000000000000' ! positive Inf
      rl3e = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl4e = z'FFF0000000000000' ! negative Inf
      i1 = 15; i2 = 16
      c1 = 'IBM'; c2='xlf'
      write(out,*) c1, rl1, i1, rl2, i2, rl3, c2, rl4

      rl1e = z'FFF0000000000000' ! negative Inf
      rl2e = z'7FF0000000000000' ! positive Inf
      rl3e = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl4e = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      i1 = 17; i2 = 18
      c1 = 'ibm'; c2='XLF'
      write(out,*) c1, rl1, i1, rl2, i2, rl3, c2, rl4

      rl1e = z'7FF0000000000000' ! positive Inf
      rl2e = z'FFF0000000000000' ! negative Inf
      rl3e = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl4e = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      write(out,*) rl1, rl2, rl3, rl4

      rl1e = 3.0
      rl2e = z'FFF0000000000000' ! negative Inf
      rl3e = 2.0
      rl4e = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      i1 = 19; i2 = 20
      c1 = 'IBM'; c2='xlf'
      write(out,*) c1, rl1, i1, rl2, i2, rl3, c2, rl4


      close(out)

      end
