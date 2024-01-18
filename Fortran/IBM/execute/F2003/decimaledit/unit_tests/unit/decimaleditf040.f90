!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditf040.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 05, 2006
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
!*  DESCRIPTION                : This testcase tests the behaviour when
!*                               reading a line containing multiple data
!*                               using various decimal edit modes. Also
!*                               test the functionality of PRINT stmt.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_IN = 'decimaleditf040.dat'
      integer, parameter :: IN = 22 ! unit numbers

      real :: rl1 = 3.14, rl2, rl3, rl4

      character(50) :: buffer, my_fmt

      print '(dc, f4.2)', rl1
      print '(dp, f4.2)', rl1

      my_fmt='(dc, f4.2)'
      print my_fmt, rl1
      my_fmt='(dp, f4.2)'
      print my_fmt, rl1

      print 100, rl1
 100  format( dc, f4.2 )
      print 101, rl1
 101  format( dp, f4.2 )

      open(unit=IN, file=FNAME_IN, decimal='comma')

      read(IN,*) rl1, rl2, rl3, rl4
      print '(4f5.2)', rl1, rl2, rl3, rl4
      print '(dc, 4f5.2)', rl1, rl2, rl3, rl4
      call resetReals(rl1, rl2, rl3, rl4)

      read(IN,*) rl1, rl2, rl3, rl4
      print '(4f5.2)', rl1, rl2, rl3, rl4
      print '(dc, 4f5.2)', rl1, rl2, rl3, rl4
      call resetReals(rl1, rl2, rl3, rl4)

      read(IN,*) rl1, rl2, rl3, rl4
      print '(4f5.2)', rl1, rl2, rl3, rl4
      print '(dc, 4f5.2)', rl1, rl2, rl3, rl4
      call resetReals(rl1, rl2, rl3, rl4)

      read(IN,*) rl1, rl2, rl3, rl4
      print '(4f5.2)', rl1, rl2, rl3, rl4
      print '(dc, 4f5.2)', rl1, rl2, rl3, rl4
      call resetReals(rl1, rl2, rl3, rl4)

      read(IN,'(4f5.2)') rl1, rl2, rl3, rl4
      print '(dp, 4f5.2)', rl1, rl2, rl3, rl4
      print '(dc, f5.2, dp, f5.2, dc, f5.2, dp, f5.2)', rl1,rl2,rl3,rl4
      call resetReals(rl1, rl2, rl3, rl4)

      close(IN)

      end

      subroutine resetReals(a,b,c,d)
        real :: a, b, c, d
        a = 0.0
        b = 0.0
        c = 0.0
        d = 0.0
      end subroutine
