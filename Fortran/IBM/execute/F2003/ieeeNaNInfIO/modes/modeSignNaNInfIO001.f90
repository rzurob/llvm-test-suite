!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : modeSignNaNInfIO001.f
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
!*  Test the output of IEEE NaN and Inf when SIGN mode is 'PLUS'.
!*  Try different field widths for format-directed I/O.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: out = 11

      real(4)    :: rl1
      complex(4) :: cx

      character(67)  :: myfmt =                                        &
 & '(f0.0, f1.1, f4.1, f2.1, f6.6, f5.1, f3.3, f7.1, f8.2, f9.3, f15.2)'

      open(out, file='modeSignNaNInfIO001.out', action='write')

      rl1 = b'01111111100000000000000000000000' ! positive Inf
      cx = (rl1, rl1)
      write(out, fmt=myfmt, sign='plus')                               &
     &     rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1
      write(out, fmt=myfmt, sign='plus')                               &
     &     cx, cx, cx, cx, cx, rl1

      rl1 = b'11111111100000000000000000000000' ! negative Inf
      cx = (rl1, rl1)
      write(out, fmt=myfmt, sign='plus')                               &
     &     rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1
      write(out, fmt=myfmt, sign='plus')                               &
     &     cx, cx, cx, cx, cx, rl1

      rl1 = b'01111111111111111111111111111111' ! positive NaN(Q)
      cx = (rl1, rl1)
      write(out, fmt=myfmt, sign='plus')                               &
     &     rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1
      write(out, fmt=myfmt, sign='plus')                               &
     &     cx, cx, cx, cx, cx, rl1

      rl1 = b'01111111101111111111111111111111' ! positive NaN(S)
      cx = (rl1, rl1)
      write(out, fmt=myfmt, sign='plus')                               &
     &     rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1
      write(out, fmt=myfmt, sign='plus')                               &
     &     cx, cx, cx, cx, cx, rl1

      rl1 = b'11111111111111111111111111111111' ! negative NaN(Q)
      cx = (rl1, rl1)
      write(out, fmt=myfmt, sign='plus')                               &
     &     rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1
      write(out, fmt=myfmt, sign='plus')                               &
     &     cx, cx, cx, cx, cx, rl1

      rl1 = b'11111111101111111111111111111111' ! negative NaN(S)
      cx = (rl1, rl1)
      write(out, fmt=myfmt, sign='plus')                               &
     &     rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1, rl1
      write(out, fmt=myfmt, sign='plus')                               &
     &     cx, cx, cx, cx, cx, rl1



      close(out)

      end
