!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditf020.f
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
!*                               This tests i/o for internal files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(5) :: IN_P1 = '1.345'
      character(6) :: IN_P2 = '2.1212'
      character(12) :: IN_P3 = '1.34552.1212'
      character(14) :: IN_P4 = '(2.1212,1.345)'
      character(5) :: IN_C1 = '1,345'
      character(6) :: IN_C2 = '2,1212'
      character(12) :: IN_C3 = '1,34552,1212'
      character(14) :: IN_C4 = '(2,1212;1,345)'

      real :: rl1
      complex :: cx1

      logical :: precision_r4, precision_x8


! read from the point mode file:
      read(IN_P1,'(dp,f5.3)') rl1
      if( .not. precision_r4(rl1, 1.345) ) error stop 1

      read(IN_P2, *, decimal='point') rl1
      if( .not. precision_r4(rl1, 2.1212) ) error stop 2

      read(IN_P3, '(dp,2f6.4)') cx1
      if( .not. precision_x8(cx1, (1.3455,2.1212)) ) error stop 3

      read(IN_P4, *, decimal='point') cx1
      if( .not. precision_x8(cx1, (2.1212, 1.345) ) ) error stop 4

! read from the comma mode file:
      read(IN_C1,'(dc,f5.3)') rl1
      if( .not. precision_r4(rl1, 1.345) ) error stop 5

      read(IN_C2, *, decimal='comma') rl1
      if( .not. precision_r4(rl1, 2.1212) ) error stop 6

      read(IN_C3, '(dc,2f6.4)') cx1
      if( .not. precision_x8(cx1, (1.3455,2.1212)) ) error stop 7

      read(IN_C4, *, decimal='comma') cx1
      if( .not. precision_x8(cx1, (2.1212, 1.345) ) ) error stop 8

      end
