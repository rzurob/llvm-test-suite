!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditf009.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 16, 2005
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
!*  DESCRIPTION                : This tests the functionality of the
!*                               decimal edit mode when using OPEN,
!*                               READ and WRITE stmts during list-directed I/O
!*                               for objects of type COMPLEX. This testcase validates
!*                               the behaviour when using compile-time encoding
!*                               of the DECIMAL= specifier for external files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_C = 'decimaleditf009.c.dat',   &
     &                            FNAME_P = 'decimaleditf009.p.dat',   &
     &                            FNAME_OUT = 'decimaleditf009.out'
      integer, parameter :: IN_C = 11, IN_P = 22, OUT = 33 ! unit numbers

      complex :: rl1 = (3.14,4.13), rl2

      integer, parameter :: NUM_TESTS = 7 ! number of records in each input files

      integer :: i

      ! open the unit with the mode in which data has been written to each file
      open(IN_C, file=FNAME_C, decimal='comma')
      open(IN_P, file=FNAME_P, decimal='point')
      open(OUT, file=FNAME_OUT, decimal='point')

      write(OUT, *, decimal='point') rl1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         read(IN_C, *, decimal='comma') rl2
         write(OUT, *, decimal='point') rl2
         write(OUT, *, decimal='comma') rl2

         read(IN_P, *, decimal='point') rl2
         write(OUT, *, decimal='comma') rl2
         write(OUT, *, decimal='point') rl2
      end do

      close(IN_C)
      close(IN_P)
      close(OUT)

!*** This part is the same as above, except that the decimal mode in open stmt is set
!*** to the opposite of the mode in which data has been written to each file.

      ! open the unit with the opposite mode associated with the file
      open(IN_C, file=FNAME_C, decimal='point')
      open(IN_P, file=FNAME_P, decimal='comma')
      open(OUT, file=FNAME_OUT, decimal='comma', position='append')

      write(OUT, *, decimal='point') rl1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         read(IN_C, *, decimal='comma') rl2
         write(OUT, *, decimal='point') rl2
         write(OUT, *, decimal='comma') rl2

         read(IN_P, *, decimal='point') rl2
         write(OUT, *, decimal='comma') rl2
         write(OUT, *, decimal='point') rl2
      end do

      close(IN_C)
      close(IN_P)
      close(OUT)

      end
