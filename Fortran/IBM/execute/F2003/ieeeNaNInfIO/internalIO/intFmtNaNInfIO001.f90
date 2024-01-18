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
!*  Test input/output of IEEE NaN and Inf with internal files using format-directed I/O.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      character(100) :: iFile =                                        &
    & 'nan -infinity +nan(s) nan(q) -nan(_abc123)+infinity +inf -NaN(S)'

      character(64) :: oFile

      character(64) :: verif_str =                                    &
    & '  NaN***   NaN(S)     -Inf NaN    NaN(Q)     Inf     Inf  NaN(S)'

      real(4)    :: rl1, rl2
      real(8)    :: rl3, rl4
      complex(4) :: cx1
      complex(8) :: cx2

      read(iFile,'(f3.0, f11.1, f7.7, f8.4, f13.1, f10.1, f4.3, f8.1)')&
     &     rl1, cx1, rl2, cx2, rl3, rl4

      write(oFile, '(f5.4, f3.1, f9.2, f9.2, f4.0, f10.1, 3f8.1)')     &
     &     rl1, cx1, real(cx1), rl2, cx2, rl3, rl4

      if ( oFile .ne. verif_str ) error stop 1_4

      end
