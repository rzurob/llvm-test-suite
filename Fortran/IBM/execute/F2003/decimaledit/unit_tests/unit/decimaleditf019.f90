!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditf019.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 21, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the decimal
!*                               edit mode in Fortran 2003 std ( Feature
!*                               289039 ). This feature affects the decimal
!*                               symbol and value separator during I/O.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Make sure internal representation of the
!*                               numbers are intact when reading values
!*                               using various decimal modes.
!*                               This tests i/o for the external files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_C = 'decimaleditf019.c.dat',   &
     &                            FNAME_P = 'decimaleditf019.p.dat'

      integer, parameter :: IN_C = 11, IN_P = 22 ! unit numbers

      real :: rl1
      complex :: cx1

      logical :: precision_r4, precision_x8

      open(IN_C, file=FNAME_C)
      open(IN_P, file=FNAME_P)

! read from the point mode file:
      read(IN_P,'(dp,f5.3)') rl1
      if( .not. precision_r4(rl1, 1.345) ) error stop 1

      read(IN_P, *, decimal='point') rl1
      if( .not. precision_r4(rl1, 2.1212) ) error stop 2

      read(IN_P, '(dp,2f6.4)') cx1
      if( .not. precision_x8(cx1, (1.3455,2.1212)) ) error stop 3

      read(IN_P, *, decimal='point') cx1
      if( .not. precision_x8(cx1, (2.1212, 1.345) ) ) error stop 4

! read from the comma mode file:
      read(IN_C,'(dc,f5.3)') rl1
      if( .not. precision_r4(rl1, 1.345) ) error stop 5

      read(IN_C, *, decimal='comma') rl1
      if( .not. precision_r4(rl1, 2.1212) ) error stop 6

      read(IN_C, '(dc,2f6.4)') cx1
      if( .not. precision_x8(cx1, (1.3455,2.1212)) ) error stop 7

      read(IN_C, *, decimal='comma') cx1
      if( .not. precision_x8(cx1, (2.1212, 1.345) ) ) error stop 8


      close(IN_C)
      close(IN_P)


      open(IN_C, file=FNAME_C, decimal='comma')
      open(IN_P, file=FNAME_P, decimal='point')

! read from the point mode file:
      read(IN_P,'(dp,f5.3)') rl1
      if( .not. precision_r4(rl1, 1.345) ) error stop 9

      read(IN_P, *, decimal='point') rl1
      if( .not. precision_r4(rl1, 2.1212) ) error stop 10

      read(IN_P, '(dp,2f6.4)') cx1
      if( .not. precision_x8(cx1, (1.3455,2.1212)) ) error stop 11

      read(IN_P, *, decimal='point') cx1
      if( .not. precision_x8(cx1, (2.1212, 1.345) ) ) error stop 12

! read from the comma mode file:
      read(IN_C,'(dc,f5.3)') rl1
      if( .not. precision_r4(rl1, 1.345) ) error stop 13

      read(IN_C, *, decimal='comma') rl1
      if( .not. precision_r4(rl1, 2.1212) ) error stop 14

      read(IN_C, '(dc,2f6.4)') cx1
      if( .not. precision_x8(cx1, (1.3455,2.1212)) ) error stop 15

      read(IN_C, *, decimal='comma') cx1
      if( .not. precision_x8(cx1, (2.1212, 1.345) ) ) error stop 16


      close(IN_C)
      close(IN_P)


      open(IN_C, file=FNAME_C, decimal='point')
      open(IN_P, file=FNAME_P, decimal='comma')

! read from the point mode file:
      read(IN_P,'(dp,f5.3)') rl1
      if( .not. precision_r4(rl1, 1.345) ) error stop 17

      read(IN_P, *, decimal='point') rl1
      if( .not. precision_r4(rl1, 2.1212) ) error stop 18

      read(IN_P, '(dp,2f6.4)') cx1
      if( .not. precision_x8(cx1, (1.3455,2.1212)) ) error stop 19

      read(IN_P, *, decimal='point') cx1
      if( .not. precision_x8(cx1, (2.1212, 1.345) ) ) error stop 20

! read from the comma mode file:
      read(IN_C,'(dc,f5.3)') rl1
      if( .not. precision_r4(rl1, 1.345) ) error stop 21

      read(IN_C, *, decimal='comma') rl1
      if( .not. precision_r4(rl1, 2.1212) ) error stop 22

      read(IN_C, '(dc,2f6.4)') cx1
      if( .not. precision_x8(cx1, (1.3455,2.1212)) ) error stop 23

      read(IN_C, *, decimal='comma') cx1
      if( .not. precision_x8(cx1, (2.1212, 1.345) ) ) error stop 24


      close(IN_C)
      close(IN_P)


      end
