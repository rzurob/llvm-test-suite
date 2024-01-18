!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditf006.f
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
!*                               for objects of type REAL. This testcase validates
!*                               the behaviour when using run-time encoding
!*                               of the DECIMAL= specifier for internal files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_OUT = 'decimaleditf006.out'

      integer, parameter :: OUT = 33 ! unit numbers
      integer, parameter :: NUM_TESTS = 5

      real          :: values(NUM_TESTS) =                             &
     & (/7.01, 32.1, 1.324, 3.14, 9.891029359/)
      character(11) :: p_values(NUM_TESTS) =                           &
     & (/'7.01       ','32.1       ', '1.324      ', '3.14       ',    &
     &   '9.891029359'/)
      character(11) :: c_values(NUM_TESTS) =                           &
     & (/'7,01       ','32,1       ', '1,324      ', '3,14       ',    &
     &   '9,891029359'/)

      character(50) :: buffer ! acts as the internal file

      character(10) :: decim_mode

      real :: rl1 = 3.14, rl2
      integer :: i

      open(unit=OUT, file=FNAME_OUT)

      decim_mode='point'
      write(buffer, *, decimal=decim_mode) rl1
      write(OUT,*) buffer
      decim_mode='point'
      read(buffer, *, decimal=decim_mode) rl2
      write(OUT,*) rl2

      do i = 1, NUM_TESTS
         buffer = p_values(i)
         decim_mode='point'
         read(buffer,*,decimal=decim_mode) rl2
         write(OUT,*) rl2
         buffer = '' ! reset buffer
         decim_mode='comma'
         write(buffer,*,decimal=decim_mode) values(i)
         write(OUT,*) buffer
         decim_mode='comma'
         read(buffer,*,decimal=decim_mode) rl2
         write(OUT,*) rl2
      end do

      decim_mode='comma'
      write(buffer, *, decimal=decim_mode) rl1
      write(OUT,*) buffer
      decim_mode='comma'
      read(buffer, *, decimal=decim_mode) rl2
      write(OUT,*) rl2

      do i = 1, NUM_TESTS
         buffer = c_values(i)
         decim_mode='comma'
         read(buffer,*,decimal=decim_mode) rl2
         write(OUT,*) rl2
         buffer = '' ! reset buffer
         decim_mode='point'
         write(buffer,*,decimal=decim_mode) values(i)
         write(OUT,*) buffer
         decim_mode='point'
         read(buffer,*,decimal=decim_mode) rl2
         write(OUT,*) rl2
      end do

      end
