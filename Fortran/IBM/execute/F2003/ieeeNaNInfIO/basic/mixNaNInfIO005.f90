!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mixNaNInfIO005.f
!*
!*  DATE                       : June 16, 2006
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
!*  Test the compiler behaviour when invalid input for NaN and Inf are
!*  specified.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      real(4)  :: rl1 = -1.0
      complex(4) :: cx1

      integer, parameter :: out = 11, in = 12

      call setrteopts("errloc=yes")

      open(in, file='mixNaNInfIO005.dat', action='read')
      open(out, file='mixNaNInfIO005.out', action='write')

      ! bad input line 1
      read(in, '(F8.1)') rl1
      write(out, '(F8.1)') rl1
      rl1 = -1.0

      ! good input line 2
      read(in, '(F8.1)') rl1
      write(out, '(F8.1)') rl1
      rl1 = -1.0

      ! bad input line 3
      read(in, '(F7.2)') rl1
      write(out, '(F8.1)') rl1
      rl1 = -1.0


      ! bad input line 4
      read(in, '(F3.2)') rl1
      write(out, '(F8.1)') rl1
      rl1 = -1.0

      ! good input line 5
      read(in, '(F3.2)') rl1
      write(out, '(F8.1)') rl1
      rl1 = -1.0

      ! bad input line 6
      read(in, '(F5.2)') rl1
      write(out, '(F8.1)') rl1
      rl1 = -1.0

      ! good input line 7
      read(in, '(F5.2)') rl1
      write(out, '(F8.1)') rl1
      rl1 = -1.0

      ! bad input line 8
      read(in, '(F9.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 9
      read(in, '(F9.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 10
      read(in, '(F5.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 11
      read(in, '(f45.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 12
      read(in, '(F5.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 13
      read(in, '(f5.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

! nan values:

      ! bad input line 14
      read(in, '(F4.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 15
      read(in, '(f4.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 16
      read(in, '(F5.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 17
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 18
      read(in, '(F6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 19
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 20
      read(in, '(F8.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 21
      read(in, '(f8.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 22
      read(in, '(F6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 23
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 24
      read(in, '(F6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 25
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 26
      read(in, '(F6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 27
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 28
      read(in, '(F4.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 29
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 30
      read(in, '(F7.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 31
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 32
      read(in, '(F5.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 33
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0


      ! bad input line 34
      read(in, '(F7.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 35
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 36
      read(in, '(F6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 37
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 38
      read(in, '(F6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 39
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 40
      read(in, '(F6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 41
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 42
      read(in, '(F7.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 43
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 44
      read(in, '(F7.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 45
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 46
      read(in, '(F7.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 47
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 48
      read(in, '(F5.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 49
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 50
      read(in, '(F8.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 51
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! bad input line 52
      read(in, '(F5.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0

      ! good input line 53
      read(in, '(f6.2)') rl1
      write(out, '(F9.1)') rl1
      rl1 = -1.0


      close(in)
      close(out)

      end
