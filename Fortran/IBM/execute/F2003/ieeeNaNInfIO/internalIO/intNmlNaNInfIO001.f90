!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 27, 2006
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
!*  Test input/output of IEEE NaN and Inf with internal files using namelist I/O.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      real(4)    :: rl1
      real(8)    :: rl2
      complex(4) :: cx1
      complex(8) :: cx2

      namelist /mynml/ rl1, cx1, rl2, cx2

      character(64)  :: iFile(6)
      character(64)  :: oFile(6)

      iFile(1) = '&MYNML'
      iFile(2) = 'RL1=NaN, CX1=(+inf,-nan(s)), RL2=-InfinitY, CX2=(NaN(),NaN(Q))'
      iFile(3) = '/'
      iFile(4) = '&MYNML'
      iFile(5) = 'RL1=-inF, CX1=(infinity,-nan(Q)), RL2=-Nan, CX2=(NaN,NaN())'
      iFile(6) = '/'

      read(iFile(1:3), nml=mynml)

      write(oFile(1:3), nml=mynml)

      read(iFile(4:), nml=mynml)

      write(oFile(4:), nml=mynml)

      print *, oFile(1)
      print *, oFile(2)
      print *, oFile(3)
      print *, oFile(4)
      print *, oFile(5)
      print *, oFile(6)


      end
