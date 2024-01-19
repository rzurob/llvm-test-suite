!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 05, 2006
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
!*  DESCRIPTION                : This test makes sure that input values
!*                               with formats such as .xyz and xyz. are
!*                               read properly.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_C = 'decimaleditf038.c.dat',   &
     &                            FNAME_P = 'decimaleditf038.p.dat'

      integer, parameter :: IN_C = 11, IN_P = 22 ! unit numbers

      real :: rl1

      character(50) :: buffer

      logical precision_r4

      ! open the unit with the mode in which data has been written to each file
      open(IN_C, file=FNAME_C, decimal='comma')
      open(IN_P, file=FNAME_P, decimal='point')

      ! read everything from input files and test the value read
      read(IN_C, *) rl1
      if( .not. precision_r4( rl1, 0.14 ) ) error stop 1

      read(IN_C, *) rl1
      if( .not. precision_r4( rl1, 14.0 ) ) error stop 2

      read(IN_C, *) rl1
      if( .not. precision_r4( rl1, 0.973 ) ) error stop 3

      read(IN_C, *) rl1
      if( .not. precision_r4( rl1, 12345.0 ) ) error stop 4

      read(IN_P, *) rl1
      if( .not. precision_r4( rl1, 0.14 ) ) error stop 5

      read(IN_P, *) rl1
      if( .not. precision_r4( rl1, 14.0 ) ) error stop 6

      read(IN_P, *) rl1
      if( .not. precision_r4( rl1, 0.973 ) ) error stop 7

      read(IN_P, *) rl1
      if( .not. precision_r4( rl1, 12345.0 ) ) error stop 8

      close(IN_C)
      close(IN_P)

! now test for internal files:
      buffer = '.11'
      rl1 = 0.0
      read(buffer,*) rl1
      if( .not. precision_r4( rl1, 0.11 ) ) error stop 9
      buffer = '11.'
      rl1 = 0.0
      read(buffer,*) rl1
      if( .not. precision_r4( rl1, 11.0 ) ) error stop 10

      buffer = ',11'
      rl1 = 0.0
      read(buffer,*,decimal='comma') rl1
      if( .not. precision_r4( rl1, 0.11 ) ) error stop 11

      buffer = '11,'
      rl1 = 0.0
      read(buffer,*,decimal='comma') rl1
      if( .not. precision_r4( rl1, 11.0 ) ) error stop 12

      end
