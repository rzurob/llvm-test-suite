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
!*                               READ and WRITE stmts during list-directed I/O
!*                               for objects of type COMPLEX. This testcase validates
!*                               the behaviour when using run-time encoding
!*                               of the DECIMAL= specifer for internal files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_OUT = 'decimaleditf014.out'

      integer, parameter :: OUT = 33 ! unit numbers
      integer, parameter :: NUM_TESTS = 5

      complex          :: values(NUM_TESTS) =                          &
     & (/(7.01,1.07), (32.1,1.23), (1.324,4.231), (3.14,4.13),         &
     &   (9.891029359,9.539201989) /)
      character(26) :: p_values(NUM_TESTS) =                           &
     & (/ '(7.01, 1.07)              ', '(32.1,1.23)               ',  &
     &    '(1.324, 4.231)            ', '(3.14,4.13)               ',  &
     &    '(9.891029359, 9.539201989)' /)

      character(26) :: c_values(NUM_TESTS) =                           &
     & (/ '(7,01; 1,07)              ', '(32,1;1,23)               ',  &
     &    '(1,324; 4,231)            ', '(3,14;4,13)               ',  &
     &    '(9,891029359; 9,539201989)' /)


      character(50) :: buffer ! acts as the internal file

      complex :: rl1 = (3.14,4.13), rl2
      integer :: i

      character(10) :: my_fmt

      open(unit=OUT, file=FNAME_OUT)

      my_fmt='point'
      write(buffer, *, decimal=my_fmt) rl1
      write(OUT,*) buffer
      read(buffer, *, decimal=my_fmt) rl2
      write(OUT,*) rl2

      do i = 1, NUM_TESTS
         buffer = p_values(i)
         my_fmt='point'
         read(buffer,*,decimal=my_fmt) rl2
         write(OUT,*) rl2
         buffer = '' ! reset buffer
         my_fmt='comma'
         write(buffer,*,decimal=my_fmt) values(i)
         write(OUT,*) buffer
         read(buffer,*,decimal=my_fmt) rl2
         write(OUT,*) rl2
      end do

      my_fmt='comma'
      write(buffer, *, decimal=my_fmt) rl1
      write(OUT,*) buffer
      read(buffer, *, decimal=my_fmt) rl2
      write(OUT,*) rl2

      do i = 1, NUM_TESTS
         buffer = c_values(i)
         my_fmt='comma'
         read(buffer,*,decimal=my_fmt) rl2
         write(OUT,*) rl2
         buffer = '' ! reset buffer
         my_fmt='point'
         write(buffer,*,decimal=my_fmt) values(i)
         write(OUT,*) buffer
         read(buffer,*,decimal=my_fmt) rl2
         write(OUT,*) rl2
      end do

      end
