!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: decimaleditf008.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : decimaleditf008
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Dec. 16, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the decimal
!*                               edit mode in Fortran 2003 std ( Feature
!*                               289039 ). This feature affects the decimal
!*                               symbol and value separator during I/O.
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This tests the functionality of the  
!*                               decimal edit mode when using OPEN,
!*                               READ and WRITE stmts during format-directed I/O 
!*                               for objects of type REAL. This testcase validates
!*                               the behaviour when using run-time encoding
!*                               of the DC and DP descriptors for internal files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      character(50), parameter :: FNAME_OUT = 'decimaleditf008.out'

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

      character(20) :: my_fmt

      real :: rl1 = 3.14, rl2
      integer :: i

      open(unit=OUT, file=FNAME_OUT)
      
      my_fmt='(dp, f20.10)'
      write(buffer, my_fmt) rl1
      write(OUT,*) buffer
      my_fmt='(dp, f20.10)'
      read(buffer, my_fmt) rl2
      write(OUT,*) rl2

      do i = 1, NUM_TESTS
         buffer = p_values(i)
         my_fmt='(dp, f20.10)'
         read(buffer,my_fmt) rl2
         write(OUT,*) rl2
         buffer = '' ! reset buffer
         my_fmt='(dc, f20.10)'
         write(buffer,my_fmt) values(i)
         write(OUT,*) buffer
         my_fmt='(dc, f20.10)'
         read(buffer,my_fmt) rl2
         write(OUT,*) rl2
      end do

      my_fmt='(dc, f20.10)'
      write(buffer, my_fmt) rl1
      write(OUT,*) buffer
      my_fmt='(dc, f20.10)'
      read(buffer, my_fmt) rl2
      write(OUT,*) rl2

      do i = 1, NUM_TESTS
         buffer = c_values(i)
         my_fmt='(dc, f20.10)'
         read(buffer, my_fmt) rl2
         write(OUT,*) rl2
         buffer = '' ! reset buffer
         my_fmt='(dp, f20.10)'
         write(buffer, my_fmt) values(i)
         write(OUT,*) buffer
         my_fmt='(dp, f20.10)'
         read(buffer, my_fmt) rl2
         write(OUT,*) rl2
      end do

      end
