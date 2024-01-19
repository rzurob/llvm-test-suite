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
!*  DESCRIPTION                : openning an already opened stream
!*                               should not change the decimal mode
!*                               to default implicitly.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer, parameter :: OUT = 22 ! unit numbers

      real :: rl1 = 3.14

      character(20) c

      open(unit=OUT, file='decimaleditf041.out')
      inquire(OUT, decimal=c)
      if( c .ne. 'POINT' ) error stop 1
      write(OUT,'(f4.2)') rl1

      open(unit=OUT, decimal='comma')
      inquire(OUT, decimal=c)
      if( c .ne. 'COMMA' ) error stop 2
      write(OUT,'(f4.2)') rl1

      open(unit=OUT) ! should not revert the mode to default
      inquire(OUT, decimal=c)
      if( c .ne. 'COMMA' ) error stop 3
      write(OUT,'(f4.2)') rl1


      end
