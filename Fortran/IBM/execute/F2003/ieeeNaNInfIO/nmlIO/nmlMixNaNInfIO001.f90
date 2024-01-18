!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : nmlMixNaNInfIO001.f
!*
!*  DATE                       : June 22, 2006
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
!*  Mix objects of type complex and real with different kinds and also
!*  mix input and output for IEEE exceptional specification during
!*  namelist I/O.
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

      namelist /nan/ rl1, cx1, rl2, cx2, rl3, cx3

      open(in,  file='nmlMixNaNInfIO001.dat', action='read')
      open(out, file='nmlMixNaNInfIO001.out', action='write')

      do
         read(in, nml=nan, iostat=ios)

         if ( is_iostat_end(ios) ) exit

         write(out, nml=nan)

      end do

      close(in)
      close(out)

      end
