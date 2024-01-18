!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
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
!*                               READ and WRITE stmts during format-directed I/O
!*                               for objects of type COMPLEX. This testcase validates
!*                               the behaviour when using run-time encoding
!*                               of DP and DC descriptors for external files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_C = 'decimaleditf012.c.dat',   &
     &                            FNAME_P = 'decimaleditf012.p.dat',   &
     &                            FNAME_OUT = 'decimaleditf012.out'
      integer, parameter :: IN_C = 11, IN_P = 22, OUT = 33 ! unit numbers

      complex :: rl1 = (3.14,4.13), rl2

      character(20) :: my_fmt

      ! open the unit with the mode in which data has been written to each file
      open(IN_C, file=FNAME_C, decimal='comma')
      open(IN_P, file=FNAME_P, decimal='point')
      open(OUT, file=FNAME_OUT, decimal='point')

      my_fmt='(dp, 2f4.2)'
      write(OUT, my_fmt) rl1

      ! read everything from input files in comma mode and output it to OUT
      my_fmt='(dc, 2f4.2)'
      read(IN_C, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dp,2f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dc, f4.1, f4.2)'
      read(IN_C, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dp, f4.1, f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dc, 2f5.3)'
      read(IN_C, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dp,2f5.3)'
      write(OUT, my_fmt) rl2

      my_fmt='(dc, 2f4.2)'
      read(IN_C, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dp,2f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dc, 2f3.1)'
      read(IN_C, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dp,2f3.1)'
      write(OUT, my_fmt) rl2

      ! read everything from input files in point mode and output it to OUT
      my_fmt='(dp, 2f4.2)'
      read(IN_P, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dc,2f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dp, f4.1, f4.2)'
      read(IN_P, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dc, f4.1, f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dp, 2f5.3)'
      read(IN_P, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dc,2f5.3)'
      write(OUT, my_fmt) rl2

      my_fmt='(dp, 2f4.2)'
      read(IN_P, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dc, 2f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dp, 2f3.1)'
      read(IN_P, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dc,2f3.1)'
      write(OUT, my_fmt) rl2


      close(IN_C)
      close(IN_P)
      close(OUT)

!*** This part is the same as above, except that the decimal mode in open stmt is set
!*** to the opposite of the mode in which data has been written to each file.

      ! open the unit with the opposite mode associated with the file
      open(IN_C, file=FNAME_C, decimal='point')
      open(IN_P, file=FNAME_P, decimal='comma')
      open(OUT, file=FNAME_OUT, decimal='comma', position='append')

      my_fmt='(dp, 2f4.2)'
      write(OUT, my_fmt) rl1

      ! read everything from input files in comma mode and output it to OUT
      my_fmt='(dc, 2f4.2)'
      read(IN_C, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dp,2f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dc, f4.1, f4.2)'
      read(IN_C, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dp, f4.1, f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dc, 2f5.3)'
      read(IN_C, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dp,2f5.3)'
      write(OUT, my_fmt) rl2

      my_fmt='(dc, 2f4.2)'
      read(IN_C, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dp,2f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dc, 2f3.1)'
      read(IN_C, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dp,2f3.1)'
      write(OUT, my_fmt) rl2

      ! read everything from input files in point mode and output it to OUT
      my_fmt='(dp, 2f4.2)'
      read(IN_P, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dc,2f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dp, f4.1, f4.2)'
      read(IN_P, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dc, f4.1, f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dp, 2f5.3)'
      read(IN_P, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dc,2f5.3)'
      write(OUT, my_fmt) rl2

      my_fmt='(dp, 2f4.2)'
      read(IN_P, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dc, 2f4.2)'
      write(OUT, my_fmt) rl2

      my_fmt='(dp, 2f3.1)'
      read(IN_P, my_fmt) rl2
      write(OUT, my_fmt) rl2
      my_fmt='(dc,2f3.1)'
      write(OUT, my_fmt) rl2

      close(IN_C)
      close(IN_P)
      close(OUT)

      end
