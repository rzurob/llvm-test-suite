!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditf024.f
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
!*                               I/O. This tests the run-time encoding
!*                               of DECIMAL= specifier for internal files
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_OUT = 'decimaleditf024.out'
      integer, parameter :: OUT = 33 ! unit numbers

      real :: rl2 = 0.0
      complex ::  cx2 = (0.0,0.0)

      character(46) :: IN_P(3) =                                      &
     &  (/'&NML2                                         ',           &
     &    'RL2=3.140000105, CX2=(13.13000011,4.880000114)',           &
     &    '/                                             '/)
      character(46) :: IN_C(3) =                                      &
     &  (/'&NML2                                         ',           &
     &    'RL2=3,140000105; CX2=(13,13000011;4,880000114)',           &
     &    '/                                             '/)

      character(47) :: buffer(3)

      character(20) :: my_fmt

      namelist /nml2/ rl2, cx2

      open(unit=OUT, file=FNAME_OUT)

      my_fmt='comma'
      read(IN_C, nml2, decimal=my_fmt)
      my_fmt='point'
      write(buffer, nml2, decimal=my_fmt)
      write(OUT,*) buffer
      buffer=(/'','',''/)
      my_fmt='comma'
      write(buffer, nml2, decimal=my_fmt)
      write(OUT,*) buffer

      rl2=0.0
      cx2=(0.0,0.0)

      my_fmt='point'
      read(IN_P, nml2, decimal=my_fmt)
      my_fmt='comma'
      write(buffer, nml2, decimal=my_fmt)
      write(OUT, *) buffer
      buffer=(/'','',''/)
      my_fmt='point'
      write(buffer, nml2, decimal=my_fmt)
      write(OUT, *) buffer

      rl2=0.0
      cx2=(0.0,0.0)

      close(OUT)

      end
