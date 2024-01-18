!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: decimaleditf015.f
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
!*  TEST CASE TITLE            : decimaleditf015
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Dec. 20, 2005
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
!*                               READ and WRITE stmts during list-directed I/O 
!*                               for objects of type COMPLEX. This testcase validates
!*                               the behaviour when using compile-time encoding
!*                               of the DC and DP descriptors for internal files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      character(50), parameter :: FNAME_OUT = 'decimaleditf015.out'

      integer, parameter :: OUT = 33 ! unit numbers
      integer, parameter :: NUM_TESTS = 5
      complex          :: values(NUM_TESTS) =                          &
     & (/(7.01,1.07), (32.1,1.23), (1.324,4.231), (3.14,4.13),         &
     &   (9.891029359,9.539201989) /)
      character(26) :: p_values(NUM_TESTS) =                           &
     & (/ '7.011.07                  ', '32.11.23                  ',  &
     &    '1.3244.231                ', '3.144.13                  ',  &
     &    '9.8910293599.539201989    ' /)

      character(26) :: c_values(NUM_TESTS) =                           &
     & (/ '7,011,07                  ', '32,11,23                  ',  &
     &    '1,3244,231                ', '3,144,13                  ',  &
     &    '9,8910293599,539201989    ' /)

      character(16), parameter :: dp_fmt(NUM_TESTS) =                  &
     & (/'(dp, 2f4.2)     ','(dp, f4.1, f4.2)','(dp, 2f5.3)     ',     &
     &   '(dp, 2f4.2)     ','(dp, 2f11.9)    ' /)

      character(16), parameter :: dc_fmt(NUM_TESTS) =                  &
     & (/'(dc, 2f4.2)     ','(dc, f4.1, f4.2)','(dc, 2f5.3)     ',     &
     &   '(dc, 2f4.2)     ','(dc, 2f11.9)    ' /)


      character(50) :: buffer ! acts as the internal file

      complex :: rl1 = (3.14,4.13), rl2
      integer :: i

      
      
      open(unit=OUT, file=FNAME_OUT)

      write(buffer, '(dp, 2f4.2)') rl1
      write(OUT,*) buffer
      read(buffer, '(dp, 2f4.2)') rl2
      write(OUT,*) rl2

      do i = 1, NUM_TESTS
         buffer = p_values(i)
         read(buffer,dp_fmt(i)) rl2
         write(OUT,*) rl2
         buffer = '' ! reset buffer
         write(buffer,dc_fmt(i)) values(i)
         write(OUT,*) buffer
         read(buffer,dc_fmt(i)) rl2
         write(OUT,*) rl2
      end do

      write(buffer, '(dc, 2f4.2)') rl1
      write(OUT,*) buffer
      read(buffer, '(dc, 2f4.2)') rl2
      write(OUT,*) rl2

      do i = 1, NUM_TESTS
         buffer = c_values(i)
         read(buffer,dc_fmt(i)) rl2
         write(OUT,*) rl2
         buffer = '' ! reset buffer
         write(buffer,dp_fmt(i)) values(i)
         write(OUT,*) buffer
         read(buffer,dp_fmt(i)) rl2
         write(OUT,*) rl2
      end do

      end
