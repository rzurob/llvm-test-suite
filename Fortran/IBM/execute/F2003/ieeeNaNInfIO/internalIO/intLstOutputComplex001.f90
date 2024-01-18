!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : intLstOutputComplex001.f
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
!*  of type COMPLEX and kind 4.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      character(54) :: iFile
      character(54), parameter :: verif_str =                          &
     &     ' (NaN(S),Inf) (-Inf,NaN(Q)) (Inf,-Inf) (NaN(S),NaN(Q))'

      complex(4) :: cx1, cx2, cx3, cx4
      real(4)    :: real_part, imag_part

      real_part = b'01111111101111111111111111111111' ! +NaN(S)
      imag_part = b'01111111100000000000000000000000' ! +Inf
      cx1 = (real_part, imag_part)

      real_part = b'11111111100000000000000000000000' ! -Inf
      imag_part = b'01111111111111111111111111111111' ! +NaN(Q)
      cx2 = (real_part, imag_part)

      real_part = b'01111111100000000000000000000000' ! +Inf
      imag_part = b'11111111100000000000000000000000' ! -Inf
      cx3 = (real_part, imag_part)

      real_part = b'11111111101111111111111111111111' ! -NaN(S)
      imag_part = b'11111111111111111111111111111111' ! -NaN(Q)
      cx4 = (real_part, imag_part)

      write(iFile, *) cx1, cx2, cx3, cx4

      if ( iFile .ne. verif_str ) error stop 1_4

      end
