!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nmlOutputComplex003.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 19, 2006
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
!*  Test output of IEEE NaN and Inf for namelist I/O.
!*  In this testcase IEEE exceptional specifications
!*  are placed inside objects of type complex and kind 16.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: out = 11

      complex(16)   :: cx1, cx2
      real(16)      :: rl1, rl2, rl3, rl4
      real(8)       :: rl1eq, rl2eq, rl3eq, rl4eq
      integer      :: i1 = 11
      character(3) :: c1='IBM'
      
      equivalence(rl1, rl1eq)
      equivalence(rl2, rl2eq)
      equivalence(rl3, rl3eq)
      equivalence(rl4, rl4eq)
      
      namelist /mynml/ cx1, i1, cx2, c1
      
      open(out, file='nmlOutputComplex003.out', action='write')
      
      rl1 = 0
      rl2 = 0
      rl3 = 0
      rl4 = 0

      rl1eq = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl2eq = z'7FF0000000000000' ! positive Inf
      rl3eq = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl4eq = z'FFF0000000000000' ! negative Inf
      cx1 = ( rl1, rl2 )
      cx2 = ( rl3, rl4 )
      write(out,nml=mynml)

      rl1eq = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2eq = z'7FF0000000000000' ! positive Inf
      rl3eq = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      rl4eq = z'FFF0000000000000' ! negative Inf
      cx1 = ( rl1, rl2 )
      cx2 = ( rl3, rl4 )
      write(out,nml=mynml)
      
      rl1eq = z'7FFFFFFFFFFFFFFF' ! positive NaN(Q)
      rl2eq = z'7FF0000000000000' ! positive Inf
      rl3eq = z'FFF7FFFFFFFFFFFF' ! negative NaN(S)
      rl4eq = z'FFF0000000000000' ! negative Inf
      cx1 = ( rl1, rl2 )
      cx2 = ( rl3, rl4 )
      write(out,nml=mynml)

      rl1eq = z'FFF0000000000000' ! negative Inf
      rl2eq = z'7FF0000000000000' ! positive Inf
      rl3eq = z'7FF7FFFFFFFFFFFF' ! positive NaN(S)
      rl4eq = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx1 = ( rl1, rl2 )
      cx2 = ( rl3, rl4 )
      write(out,nml=mynml)

      rl1eq = 3.0
      rl2eq = z'FFF0000000000000' ! negative Inf
      rl3eq = 2.0
      rl4eq = z'FFFFFFFFFFFFFFFF' ! negative NaN(Q)
      cx1 = ( rl1, rl2 )
      cx2 = ( rl3, rl4 )
      write(out,nml=mynml)


      close(out)

      end
