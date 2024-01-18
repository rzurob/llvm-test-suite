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
!*  DESCRIPTION                : This tests the default behaviour of
!*                               decimal edit mode as well as validating
!*                               the temporary precedence of descriptors
!*                               over default edit mode.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_C = 'decimaleditf037.c.dat',   &
     &                            FNAME_P = 'decimaleditf037.p.dat',   &
     &                            FNAME_OUT = 'decimaleditf037.out'
      integer, parameter :: IN_C = 11, IN_P = 22, OUT = 33 ! unit numbers

      real :: rl1 = 3.14, rl2

      integer, parameter :: NUM_TESTS = 4 ! number of records in each input files

      character(50) :: buffer, my_fmt

      integer :: i

      ! open the unit with default decimal mode
      open(IN_C, file=FNAME_C)
      open(IN_P, file=FNAME_P)
      open(OUT, file=FNAME_OUT)

      my_fmt='point'
      write(OUT, *, decimal=my_fmt) rl1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         read(IN_C, *, decimal='comma') rl2
         write(OUT, *) rl2
         my_fmt='comma'
         write(OUT, *, decimal=my_fmt) rl2

         read(IN_P, *) rl2
         write(OUT, *, decimal='comma') rl2
         write(OUT, *) rl2
      end do

      close(IN_C)
      close(IN_P)
      close(OUT)

!*** This part is the same as above, except that edit descriptors are used
!*** instead of the DECIMAL= specifier

      ! open the unit with the default decimal mode
      open(IN_C, file=FNAME_C)
      open(IN_P, file=FNAME_P)
      open(OUT, file=FNAME_OUT, position='append', decimal='comma')

      write(OUT, '(dp, f12.9)') rl1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         read(IN_C, '(dc, f12.9)') rl2
         my_fmt='point'
         write(OUT, '(f12.9)', decimal=my_fmt) rl2
         write(OUT, '(f12.9)') rl2

         read(IN_P, '(f12.9)') rl2
         write(OUT, '(f12.9)') rl2
         my_fmt='(dp, f12.9)'
         write(OUT, my_fmt) rl2
      end do

      close(IN_C)
      close(IN_P)

      write(OUT, '(dp, f12.9)') rl1      ! dp should win
      my_fmt='(dc, f12.9)'
      write(OUT, my_fmt, decimal='point') rl1 ! dc should win
      write(OUT, *, decimal='point') rl1 ! point should win

      close(OUT)

! now test for internal files:
      open(OUT, file=FNAME_OUT, position='append')
      buffer = '1.11'
      read(buffer,'(f12.9)') rl2
      write(OUT,*,decimal='comma') rl2
      rl2 = 4.44
      buffer=''
      write(buffer, '(f12.9)') rl2
      write(OUT, *) buffer

      buffer = '1,11'
      read(buffer,'(dc, f12.9)') rl2
      write(OUT,*,decimal='point') rl2
      rl2 = 4.44
      buffer=''
      write(buffer, '(f12.9)', decimal='comma') rl2
      write(OUT, *) buffer

      close(OUT)

      end
