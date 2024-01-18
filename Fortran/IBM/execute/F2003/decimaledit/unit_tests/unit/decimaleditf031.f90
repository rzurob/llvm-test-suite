!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditf031.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 04, 2006
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
!*                               decimal edit mode when doing I/O with
!*                               arrays of COMPLEX. Both internal and external
!*                               files are tested. This testcase tests the
!*                               compile-time encoding of DC and DP descriptors.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_C = 'decimaleditf031.c.dat',   &
     &                            FNAME_P = 'decimaleditf031.p.dat',   &
     &                            FNAME_OUT = 'decimaleditf031.out'
      integer, parameter :: IN_C = 11, IN_P = 22, OUT = 33 ! unit numbers

      complex, dimension(3) :: cx1, cx2

      integer, parameter :: NUM_TESTS = 2 ! number of records in each input files

      character(100) :: buffer

      integer :: i

      cx1 = (/(1.10, 1.12), (1.20, 1.22), (1.30, 1.32)/)

      ! open the unit with the mode in which data has been written to each file
      open(IN_C, file=FNAME_C, decimal='comma')
      open(IN_P, file=FNAME_P, decimal='point')
      open(OUT, file=FNAME_OUT, decimal='point')

      write(OUT, '(dp, 6f5.2)') cx1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         read(IN_C, '(dc, 6f5.2)') cx2
         write(OUT, '(dp, 6f5.2)') cx2
         write(OUT, '(dc, 6f5.2)') cx2

         read(IN_P, '(dp, 6f5.2)') cx2
         write(OUT, '(dc, 6f5.2)') cx2
         write(OUT, '(dp, 6f5.2)') cx2
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

      write(OUT, '(dp, 6f5.2)') cx1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         read(IN_C, '(dc, 6f5.2)') cx2
         write(OUT, '(dp, 6f5.2)') cx2
         write(OUT, '(dc, 6f5.2)') cx2

         read(IN_P, '(dp, 6f5.2)') cx2
         write(OUT, '(dc, 6f5.2)') cx2
         write(OUT, '(dp, 6f5.2)') cx2
      end do

      close(IN_C)
      close(IN_P)
      close(OUT)

! now test for internal files:
      open(OUT, file=FNAME_OUT, position='append')
      buffer = '9.00,8.99,8.99,9.00,7.33,3.77'
      read(buffer, '(dp, 6f5.2)') cx2
      write(OUT,'(dc, 6f5.2)') cx2
      cx2 = (/(4.44, 5.44), (5.55, 6.55), (6.66, 6.77)/)
      buffer=''
      write(buffer, '(dp, 6f5.2)') cx2
      write(OUT, *) buffer


      buffer = '9,00;8,99;8,99;9,00;7,33;3,77'
      read(buffer, '(dc, 6f5.2)') cx2
      write(OUT, '(dp, 6f5.2)') cx2
      cx2 = (/(4.44, 5.44), (5.55, 6.55), (6.66, 6.77)/)
      buffer=''
      write(buffer, '(dc, 6f5.2)') cx2
      write(OUT, *) buffer

      end
