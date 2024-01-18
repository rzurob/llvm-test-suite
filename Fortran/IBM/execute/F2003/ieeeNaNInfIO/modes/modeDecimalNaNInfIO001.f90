!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : modeDecimalNaNInfIO001.f
!*
!*  DATE                       : June 28, 2006
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
!*  Make sure the DECIMAL edit mode does not affect the Input/Output
!*  of IEEE exceptional specifications. This testcase covers list-directed
!*  I/O with decimal mode of COMMA.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: in = 11, out = 12

      real(4) :: rl1, rl2
      real(8) :: rl3, rl4
      real(16) :: rl5
      complex(4) :: cx1
      complex(8) :: cx2

      integer :: ios = 0

      open(in, file='modeDecimalNaNInfIO001.dat', action='read')
      open(out, file='modeDecimalNaNInfIO001.out', action='write')

      do

         ! reset variables
         rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0; rl5 = 0.0
         cx1 = (0.0, 0.0); cx2 = (0.0, 0.0)

         read(in, *, decimal='comma', iostat=ios)                      &
     &        rl1, rl2, rl3, rl4, cx1, cx2, rl5

         if ( is_iostat_end(ios) ) exit

         write(out,*, decimal='comma') rl5, cx2, cx1, rl4, rl3, rl2, rl1
         write(out,*, decimal='point') rl5, cx2, cx1, rl4, rl3, rl2, rl1

      end do

      close(in)
      close(out)

      end
