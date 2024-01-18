!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: decimaleditf023.f
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
!*  TEST CASE TITLE            : decimaleditf023
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Dec. 21, 2005
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
!*                               decimal edit mode when using namelist 
!*                               I/O. This tests the compile-time encoding 
!*                               of DECIMAL= specifier for internal files
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      character(50), parameter :: FNAME_OUT = 'decimaleditf023.out'
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
      
      namelist /nml2/ rl2, cx2

      open(unit=OUT, file=FNAME_OUT)

      read(IN_C, nml2, decimal='comma')
      write(buffer, nml2, decimal='point')
      write(OUT,*) buffer
      buffer=(/'','',''/)
      write(buffer, nml2, decimal='comma')
      write(OUT,*) buffer

      rl2=0.0
      cx2=(0.0,0.0)

      read(IN_P, nml2, decimal='point')
      write(buffer, nml2, decimal='comma')
      write(OUT, *) buffer
      buffer=(/'','',''/)
      write(buffer, nml2, decimal='point')
      write(OUT, *) buffer

      rl2=0.0
      cx2=(0.0,0.0)

      close(OUT)

      end
