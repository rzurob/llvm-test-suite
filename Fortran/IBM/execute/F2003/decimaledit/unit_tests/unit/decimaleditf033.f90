!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: decimaleditf033.f
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
!*  TEST CASE TITLE            : decimaleditf033
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Jan. 04, 2006
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
!*  DESCRIPTION                : Using DECIMAL= in INQUIRE stmt
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_IN = 'decimaleditf033.dat',    &
     &                            FNAME_OUT = 'decimaleditf033.out'
      integer, parameter :: IN = 11, OUT = 22 ! unit numbers

      real :: rl1 = 3.14 , rl2

      character(20) :: str_decimal
      
      open(unit=IN, file=FNAME_IN, decimal='COMMA')
      
      str_decimal=''
      inquire(unit=IN, decimal=str_decimal)
      if( str_decimal .ne. 'COMMA' ) error stop 1

      read(IN,*, decimal='POINT') rl2

      str_decimal=''
      inquire(IN, decimal=str_decimal)
      if( str_decimal .ne. 'COMMA' ) error stop 2

      open(unit=OUT, file=FNAME_OUT)

      str_decimal=''      
      inquire(OUT, decimal=str_decimal)
      if( str_decimal .ne. 'POINT' ) error stop 3
      
      write(OUT,*,decimal='COMMA') rl1

      str_decimal=''
      inquire(OUT, decimal=str_decimal)
      if( str_decimal .ne. 'POINT' ) error stop 4

      open(unit=IN, decimal='POINT')

      str_decimal=''      
      inquire(IN, decimal=str_decimal)
      if( str_decimal .ne. 'POINT' ) error stop 5

      close(IN)
      
      str_decimal=''
      inquire(IN, decimal=str_decimal)
      if( str_decimal .ne. 'UNDEFINED' ) error stop 6

      open(unit=IN, file=FNAME_IN, form='unformatted')

      str_decimal=''
      inquire(IN, decimal=str_decimal)
      if( str_decimal .ne. 'UNDEFINED' ) error stop 7

      close(IN)

      close(OUT)

      str_decimal=''
      inquire(OUT, decimal=str_decimal)
      if( str_decimal .ne. 'UNDEFINED' ) error stop 8

      open(unit=OUT, file=FNAME_OUT, decimal='COMMA')

      str_decimal=''
      inquire(OUT, decimal=str_decimal)
      if( str_decimal .ne. 'COMMA' ) error stop 9

      close(OUT)
      
      open(unit=OUT, file=FNAME_OUT, form='unformatted')

      str_decimal=''
      inquire(OUT, decimal=str_decimal)
      if( str_decimal .ne. 'UNDEFINED' ) error stop 10

      close(OUT)

      
      
      ! test pre-connected units:
      str_decimal=''
      inquire(0, decimal=str_decimal)
      if( str_decimal .ne. 'POINT' ) error stop 11

      open(0, decimal='comma')
      str_decimal=''
      inquire(0, decimal=str_decimal)
      if( str_decimal .ne. 'COMMA' ) error stop 12

      open(0, decimal='point')
      str_decimal=''
      inquire(0, decimal=str_decimal)
      if( str_decimal .ne. 'POINT' ) error stop 13

      ! default input:
      str_decimal=''
      inquire(5, decimal=str_decimal)
      if( str_decimal .ne. 'POINT' ) error stop 14

      open(5, decimal='comma')
      str_decimal=''
      inquire(5, decimal=str_decimal)
      if( str_decimal .ne. 'COMMA' ) error stop 15

      open(5, decimal='point')
      str_decimal=''
      inquire(5, decimal=str_decimal)
      if( str_decimal .ne. 'POINT' ) error stop 16

      ! default output:
      str_decimal=''
      inquire(6, decimal=str_decimal)
      if( str_decimal .ne. 'POINT' ) error stop 17

      open(6, decimal='comma')
      str_decimal=''
      inquire(6, decimal=str_decimal)
      if( str_decimal .ne. 'COMMA' ) error stop 18

      open(6, decimal='point')
      str_decimal=''
      inquire(6, decimal=str_decimal)
      if( str_decimal .ne. 'POINT' ) error stop 19



      end
