!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 03, 2006
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
!*                               decimal edit mode when doing I/O with
!*                               arrays of REALs. Both internal and external
!*                               files are tested. This testcase tests the
!*                               run-time encoding of DC and DP descriptors.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_C = 'decimaleditf028.c.dat',   &
     &                            FNAME_P = 'decimaleditf028.p.dat',   &
     &                            FNAME_OUT = 'decimaleditf028.out'
      integer, parameter :: IN_C = 11, IN_P = 22, OUT = 33 ! unit numbers

      real, dimension(3) :: rl1 = (/3.14, 4.13, 0.78/), rl2

      integer, parameter :: NUM_TESTS = 4 ! number of records in each input files

      character(50) :: buffer, my_fmt

      integer :: i

      ! open the unit with the mode in which data has been written to each file
      open(IN_C, file=FNAME_C, decimal='comma')
      open(IN_P, file=FNAME_P, decimal='point')
      open(OUT, file=FNAME_OUT, decimal='point')

      my_fmt='(dp, 3f5.2)'
      write(OUT, my_fmt) rl1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         my_fmt='(dc, 3f5.2)'
         read(IN_C, my_fmt) rl2
         my_fmt='(dp, 3f5.2)'
         write(OUT, my_fmt) rl2
         my_fmt='(dc, 3f5.2)'
         write(OUT, my_fmt) rl2

         my_fmt='(dp, 3f5.2)'
         read(IN_P, my_fmt) rl2
         my_fmt='(dc, 3f5.2)'
         write(OUT, my_fmt) rl2
         my_fmt='(dp, 3f5.2)'
         write(OUT, my_fmt) rl2
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

      my_fmt='(dp, 3f5.2)'
      write(OUT, my_fmt) rl1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         my_fmt='(dc, 3f5.2)'
         read(IN_C, my_fmt) rl2
         my_fmt= '(dp, 3f5.2)'
         write(OUT, my_fmt) rl2
         my_fmt='(dc, 3f5.2)'
         write(OUT, my_fmt) rl2

         my_fmt='(dp, 3f5.2)'
         read(IN_P, my_fmt) rl2
         my_fmt='(dc, 3f5.2)'
         write(OUT, my_fmt) rl2
         my_fmt='(dp, 3f5.2)'
         write(OUT, my_fmt) rl2
      end do

      close(IN_C)
      close(IN_P)
      close(OUT)

! now test for internal files:
      open(OUT, file=FNAME_OUT, position='append')
      buffer = '1.11,2.22,3.33'
      my_fmt='(dp, 3f5.2)'
      read(buffer, my_fmt) rl2
      my_fmt='(dc, 3f5.2)'
      write(OUT,my_fmt) rl2
      rl2 = (/4.44, 5.55, 6.66/)
      buffer=''
      my_fmt='(dp, 3f5.2)'
      write(buffer,my_fmt) rl2
      write(OUT, *) buffer

      buffer = '1,11;2,22;3,33'
      my_fmt='(dc, 3f5.2)'
      read(buffer, my_fmt) rl2
      my_fmt='(dp, 3f5.2)'
      write(OUT, my_fmt) rl2
      rl2 = (/4.44, 5.55, 6.66/)
      buffer=''
      my_fmt='(dc, 3f5.2)'
      write(buffer, my_fmt) rl2
      write(OUT, *) buffer

      end