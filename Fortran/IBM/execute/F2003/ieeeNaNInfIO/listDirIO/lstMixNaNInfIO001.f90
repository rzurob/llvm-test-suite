!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : lstMixNaNInfIO001.f
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
!*  Mix objects of type COMPLEX and REAL with different kinds and also
!*  mix input and output for IEEE exceptional specification during
!*  list-directed I/O.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: in = 11, out = 12

      complex(4)  :: cx1
      complex(8)  :: cx2
      complex(16) :: cx3
      real(4)     :: rl1
      real(8)     :: rl2
      real(16)    :: rl3

      integer :: ios = 0

      open(in,  file='lstMixNaNInfIO001.dat', action='read')
      open(out, file='lstMixNaNInfIO001.out', action='write')

      do
         read(in, *, iostat=ios) cx1, rl1, cx2, rl2, cx3, rl3

         if ( is_iostat_end(ios) ) exit

         write(out, *) rl3, cx3, rl2, cx2, rl1, cx1

      end do

      close(in)
      close(out)


      end
