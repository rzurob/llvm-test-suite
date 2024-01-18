!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 04, 2006
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
!*  DESCRIPTION                : Changing the decimal mode for pre-connected
!*                               units 0, 5, and 6.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer, parameter :: OUT = 11
      real :: rl1 = 3.14, rl2

      character(10) :: fmt_comma, fmt_point

      open(unit=OUT, file='decimaleditf034.out')

      write(0,*) rl1
      write(0,*,decimal='comma') rl1

      open(0, decimal='comma')
      write(0,*) rl1
      write(0,*,decimal='point') rl1

      open(0, decimal='point')
      write(0,*) rl1
      write(0,*,decimal='comma') rl1

      read(5,*) rl2
      write(OUT,*) rl2
      read(5,*,decimal='comma') rl2
      write(OUT,*) rl2

      open(5, decimal='comma')
      read(5,*) rl2
      write(OUT,*) rl2
      read(5,*,decimal='point') rl2
      write(OUT,*) rl2

      open(5, decimal='point')
      read(5,*) rl2
      write(OUT,*) rl2
      read(5,*,decimal='comma') rl2
      write(OUT,*) rl2

      write(6,*) rl1
      write(6,*,decimal='comma') rl1

      open(6, decimal='comma')
      write(6,*) rl1
      write(6,*,decimal='point') rl1

      open(6, decimal='point')
      write(6,*) rl1
      write(6,*,decimal='comma') rl1

      end
