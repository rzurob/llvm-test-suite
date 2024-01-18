!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 16, 2005
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
!*                               the behaviour when using run-time encoding
!*                               of the DECIMAL= specifier for external files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_C = 'decimaleditf010.c.dat',   &
     &                            FNAME_P = 'decimaleditf010.p.dat',   &
     &                            FNAME_OUT = 'decimaleditf010.out'
      integer, parameter :: IN_C = 11, IN_P = 22, OUT = 33 ! unit numbers

      complex :: rl1 = (3.14,4.13), rl2

      integer, parameter :: NUM_TESTS = 7 ! number of records in each input files

      integer :: i

      character(10) :: decim_mode

      ! open the unit with the mode in which data has been written to each file
      decim_mode = 'comma'
      open(IN_C, file=FNAME_C, decimal=decim_mode)
      decim_mode = 'point'
      open(IN_P, file=FNAME_P, decimal=decim_mode)
      open(OUT, file=FNAME_OUT, decimal=decim_mode)

      write(OUT, *, decimal=decim_mode) rl1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         decim_mode = 'comma'
         read(IN_C, *, decimal=decim_mode) rl2
         decim_mode = 'point'
         write(OUT, *, decimal=decim_mode) rl2
         decim_mode = 'comma'
         write(OUT, *, decimal=decim_mode) rl2

         decim_mode = 'point'
         read(IN_P, *, decimal=decim_mode) rl2
         decim_mode = 'comma'
         write(OUT, *, decimal=decim_mode) rl2
         decim_mode = 'point'
         write(OUT, *, decimal=decim_mode) rl2
      end do

      close(IN_C)
      close(IN_P)
      close(OUT)

!*** This part is the same as above, except that the decimal mode in open stmt is set
!*** to the opposite of the mode in which data has been written to each file.

      ! open the unit with the opposite mode associated with the file
      decim_mode = 'point'
      open(IN_C, file=FNAME_C, decimal=decim_mode)
      decim_mode = 'comma'
      open(IN_P, file=FNAME_P, decimal=decim_mode)
      open(OUT, file=FNAME_OUT, decimal=decim_mode, position='append')

      decim_mode = 'point'
      write(OUT, *, decimal=decim_mode) rl1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         decim_mode = 'comma'
         read(IN_C, *, decimal=decim_mode) rl2
         decim_mode = 'point'
         write(OUT, *, decimal=decim_mode) rl2
         decim_mode = 'comma'
         write(OUT, *, decimal=decim_mode) rl2

         decim_mode = 'point'
         read(IN_P, *, decimal=decim_mode) rl2
         decim_mode = 'comma'
         write(OUT, *, decimal=decim_mode) rl2
         decim_mode = 'point'
         write(OUT, *, decimal=decim_mode) rl2
      end do

      close(IN_C)
      close(IN_P)
      close(OUT)

      end
