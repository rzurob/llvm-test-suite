!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 05, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for the DECIMAL=
!*                               specifier and decimal edit mode control
!*                               descriptors. Feature 289039.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This diagnostic checks the situation
!*                               where a point (.) is encountered
!*                               while reading data in comma mode.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      real :: rl1
      logical :: precision_r4

! testing list-directed i/o

      ! open in comma mode
      open(unit=11, file='decimaleditd017.dat', decimal='comma')

      read(11,*) rl1
      if ( .not. precision_r4(rl1, 3.14) ) error stop 1

      read(11,*) rl1
      if ( .not. precision_r4(rl1, 2033.0) ) error stop 2

      read(11,*) rl1
      if ( .not. precision_r4(rl1, 3.77) ) error stop 3

      close(11)

      ! open in point mode
      open(unit=11, file='decimaleditd017.dat', decimal='point')

      read(11,*) rl1
      if ( .not. precision_r4(rl1, 3.0) ) error stop 4

      read(11,*) rl1
      if ( .not. precision_r4(rl1, 2.33) ) error stop 5

      read(11,*) rl1
      if ( .not. precision_r4(rl1, 3.0) ) error stop 6

      close(11)

! testing format-directed i/o:

      ! open in comma mode
      open(unit=11, file='decimaleditd017.dat', decimal='comma')

      read(11,'(dc, f4.2)') rl1
      if ( .not. precision_r4(rl1, 3.14) ) error stop 7

      read(11,'(dc, f4.2)') rl1
      if ( .not. precision_r4(rl1, 20.33) ) error stop 8

      read(11,'(dc, f4.2)') rl1
      if ( .not. precision_r4(rl1, 3.77) ) error stop 9

      close(11)

      ! open in point mode
      open(unit=11, file='decimaleditd017.dat', decimal='point')

      read(11,'(dp, f4.2)') rl1 ! should not produce error msg

      read(11,'(dp, f4.2)') rl1
      if ( .not. precision_r4(rl1, 2.33) ) error stop 10

      read(11,'(dp, f4.2)') rl1 ! should not produce error msg

      close(11)

      end
