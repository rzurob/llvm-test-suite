!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : modeSignNaNInfIO004.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 28, 2006
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
!*  Test the output of IEEE NaN and Inf when SIGN mode is 'SUPPRESS'.
!*  Testing list-directed and format-directed I/O to make sure the
!*  "-" sign is not suppressed.
!* 
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: out = 11
      
      real(4)    :: rl1
      complex(4) :: cx
      
      open(out, file='modeSignNaNInfIO004.out', action='write')

      rl1 = b'01111111100000000000000000000000' ! positive Inf
      cx = (rl1, rl1)
      write(out, *, sign='suppress') rl1, cx, -rl1
      write(out, '(ss, 4f9.2)') rl1, cx, -rl1

      rl1 = b'11111111100000000000000000000000' ! negative Inf
      cx = (rl1, rl1)
      write(out, *, sign='suppress') rl1, cx, -rl1
      write(out, '(ss, 4f9.2)') rl1, cx, -rl1

      rl1 = b'01111111111111111111111111111111' ! positive NaN(Q)
      cx = (rl1, rl1)
      write(out, *, sign='suppress') rl1, cx, -rl1
      write(out, '(ss, 4f9.2)') rl1, cx, -rl1

      rl1 = b'01111111101111111111111111111111' ! positive NaN(S)
      cx = (rl1, rl1)
      write(out, *, sign='suppress') rl1, cx, -rl1
      write(out, '(ss, 4f9.2)') rl1, cx, -rl1

      rl1 = b'11111111111111111111111111111111' ! negative NaN(Q)
      cx = (rl1, rl1)
      write(out, *, sign='suppress') rl1, cx, -rl1
      write(out, '(ss, 4f9.2)') rl1, cx, -rl1

      rl1 = b'11111111101111111111111111111111' ! negative NaN(S)
      cx = (rl1, rl1)
      write(out, *, sign='suppress') rl1, cx, -rl1
      write(out, '(ss, 4f9.2)') rl1, cx, -rl1
      
      
      close(out)

      end
