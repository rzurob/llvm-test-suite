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
!*  DESCRIPTION                : This testcase tests the behaviour when
!*                               reading a file containing data with mixed
!*                               decimal edit modes.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_IN = 'decimaleditf039.dat'

      integer, parameter :: IN = 11 ! unit numbers

      real :: rl1

      character(50) :: buffer, my_fmt

      logical precision_r4

      ! first try DECIMAL= specifier
      open(IN, file=FNAME_IN)

      ! read everything from input files and test the value read
      read(IN, *) rl1
      if( .not. precision_r4( rl1, 14.1 ) ) error stop 1

      read(IN, *, decimal='comma') rl1
      if( .not. precision_r4( rl1, 1.973 ) ) error stop 2

      read(IN, *) rl1
      if( .not. precision_r4( rl1, 1.14 ) ) error stop 3

      my_fmt='comma'
      read(IN, *, decimal=my_fmt) rl1
      if( .not. precision_r4( rl1, 12345.1 ) ) error stop 4

      close(IN)

      ! now try DC and DP descriptors:
      open(IN, file=FNAME_IN)

      ! read everything from input files and test the value read
      read(IN, 11) rl1
 11   format(dp, f4.1)
      if( .not. precision_r4( rl1, 14.1 ) ) error stop 5

      read(IN, 12) rl1
 12   format(dc, f5.3)
      if( .not. precision_r4( rl1, 1.973 ) ) error stop 6

      my_fmt='(dp, f4.2)'
      read(IN, my_fmt) rl1
      if( .not. precision_r4( rl1, 1.14 ) ) error stop 7


      read(IN, 13) rl1
 13   format(dc, f7.1)
      if( .not. precision_r4( rl1, 12345.1 ) ) error stop 8

      close(IN)

      end
