!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditf021.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 21, 2005
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
!*                               decimal edit mode when using namelist
!*                               I/O. This tests the compile-time encoding
!*                               of DECIMAL= specifier for external files
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_C = 'decimaleditf021.c.dat',   &
     &                            FNAME_P = 'decimaleditf021.p.dat',   &
     &                            FNAME_OUT = 'decimaleditf021.out'
      integer, parameter :: IN_C = 11, IN_P = 22, OUT = 33 ! unit numbers

      real ::  rl1 = 4.12 ,rl2 = 3.14
      complex ::  cx1 = (1234.012, 1.01), cx2 = (13.13, 4.88)


      namelist /nml1/ rl1, cx1

      namelist /nml2/ rl2, cx2


      ! open the unit with the mode in which data has been written to each file
      open(IN_C, file=FNAME_C, decimal='comma')
      open(IN_P, file=FNAME_P, decimal='point')
      open(OUT, file=FNAME_OUT, decimal='point')

      write(OUT, nml1)
      write(OUT, nml1, decimal='point')
      write(OUT, nml1, decimal='comma')

      read(IN_C, nml2, decimal='comma')
      write(OUT, nml2, decimal='point')
      write(OUT, nml2, decimal='comma')

      read(IN_P, nml2, decimal='point')
      write(OUT, nml2, decimal='comma')
      write(OUT, nml2, decimal='point')

      close(IN_C)
      close(IN_P)
      close(OUT)

!*** This part is the same as above, except that the decimal mode in open stmt is set
!*** to the opposite of the mode in which data has been written to each file.

      ! open the unit with the opposite mode associated with the file
      open(IN_C, file=FNAME_C, decimal='point')
      open(IN_P, file=FNAME_P, decimal='comma')
      open(OUT, file=FNAME_OUT, decimal='comma', position='append')

      write(OUT, nml1)
      write(OUT, nml1, decimal='point')
      write(OUT, nml1, decimal='comma')

      read(IN_C, nml2, decimal='comma')
      write(OUT, nml2, decimal='point')
      write(OUT, nml2, decimal='comma')

      read(IN_P, nml2, decimal='point')
      write(OUT, nml2, decimal='comma')
      write(OUT, nml2, decimal='point')

      close(IN_C)
      close(IN_P)
      close(OUT)

      end
