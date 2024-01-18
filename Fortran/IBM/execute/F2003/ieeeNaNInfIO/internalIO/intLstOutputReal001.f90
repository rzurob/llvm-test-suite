!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : intLstOutputReal001.f
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
!*  Test output of IEEE NaN and Inf with internal files using list-directed I/O.
!*  In this testcase IEEE exceptional specifications are placed inside objects
!*  of type REAL and kind 4.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      character(37) :: iFile
      character(37), parameter :: verif_str =                          &
     &     ' NaN(S) NaN(S) NaN(Q) NaN(Q) Inf -Inf'

      real(4) :: rl1, rl2, rl3, rl4, rl5, rl6

      rl1 = b'01111111101111111111111111111111' ! +NaN(S)
      rl2 = b'11111111101111111111111111111111' ! -NaN(S)
      rl3 = b'01111111111111111111111111111111' ! +NaN(Q)
      rl4 = b'11111111111111111111111111111111' ! -NaN(Q)
      rl5 = b'01111111100000000000000000000000' ! +Inf
      rl6 = b'11111111100000000000000000000000' ! -Inf

      write(iFile, *) rl1, rl2, rl3, rl4, rl5, rl6

      if ( iFile .ne. verif_str ) error stop 1_4

      end
