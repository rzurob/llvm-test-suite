!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: decimaleditf018.f
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
!*  TEST CASE TITLE            : decimaleditf018
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
!*                               decimal edit mode in combination with
!*                               D, E, EN, ES, F, and G edit descriptors.
!*                               This test deals with run-time encoding
!*                               of these edit descriptors and decimal mode.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      character(50), parameter :: FNAME_C = 'decimaleditf018.c.dat',   &
     &                            FNAME_P = 'decimaleditf018.p.dat',   &
     &                            FNAME_OUT = 'decimaleditf018.out'
      integer, parameter :: IN_C = 11, IN_P = 22, OUT = 33 ! unit numbers

      real :: rl1

      character(20) my_fmt
      
      ! open the unit with the mode in which data has been written to each file
      my_fmt = 'comma'
      open(IN_C, file=FNAME_C, decimal=my_fmt)
      my_fmt='point'
      open(IN_P, file=FNAME_P, decimal=my_fmt)
      open(OUT, file=FNAME_OUT, decimal=my_fmt)

! ***********************************************      
! try reading from the file written in comma mode:
! ***********************************************
      my_fmt='(f7.5)'
      read(IN_C, my_fmt) rl1
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, f7.5)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, f7.5)'
      write(OUT, my_fmt) rl1
      my_fmt='(f7.5)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(g12.7)'
      read(IN_C, my_fmt) rl1
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, g12.7)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, g12.7)'
      write(OUT, my_fmt) rl1
      my_fmt='(g12.7)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(e9.3e2)'
      read(IN_C, my_fmt) rl1
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, e9.3e2)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, e9.3e2)'
      write(OUT, my_fmt) rl1
      my_fmt='(e9.3e2)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(e10.4)'
      read(IN_C, my_fmt) rl1
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, e10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, e10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(e10.4)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(d10.4)'
      read(IN_C, my_fmt) rl1
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, d10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, d10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(d10.4)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(en9.2)'
      read(IN_C, my_fmt) rl1
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, en9.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, en9.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(en9.2)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(es8.2)'
      read(IN_C, my_fmt) rl1
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, es8.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, es8.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(es8.2)'
      write(OUT, my_fmt, decimal='comma') rl1

      
      close(IN_C)
      open(IN_C, file=FNAME_C, decimal='point')
! same as above except use DC desc. when reading values

      my_fmt='(dc,f7.5)'
      read(IN_C, my_fmt) rl1
      my_fmt='(f7.5)'
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, f7.5)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, f7.5)'
      write(OUT, my_fmt) rl1
      my_fmt='(f7.5)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(dc,g12.7)'
      read(IN_C, my_fmt) rl1
      my_fmt='(g12.7)'
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, g12.7)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, g12.7)'
      write(OUT, my_fmt) rl1
      my_fmt='(g12.7)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(dc,e9.3e2)'
      read(IN_C, my_fmt) rl1
      my_fmt='(e9.3e2)'
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, e9.3e2)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, e9.3e2)'
      write(OUT, my_fmt) rl1
      my_fmt='(e9.3e2)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(dc,e10.4)'
      read(IN_C, my_fmt) rl1
      my_fmt='(e10.4)'
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, e10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, e10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(e10.4)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(dc,d10.4)'
      read(IN_C, my_fmt) rl1
      my_fmt='(d10.4)'
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, d10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, d10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(d10.4)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(dc,en9.2)'
      read(IN_C, my_fmt) rl1
      my_fmt='(en9.2)'
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, en9.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, en9.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(en9.2)'
      write(OUT, my_fmt, decimal='comma') rl1

      my_fmt='(dc,es8.2)'
      read(IN_C, my_fmt) rl1
      my_fmt='(es8.2)'
      write(OUT, my_fmt) rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, es8.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, es8.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(es8.2)'
      write(OUT, my_fmt, decimal='comma') rl1


      close(IN_C)
      close(OUT)
! ***********************************************
! try reading from the file written in point mode:
! ***********************************************
      my_fmt='comma'
      open(OUT, file=FNAME_OUT, decimal=my_fmt, position='append')      

      my_fmt='(f7.5)'
      read(IN_P, my_fmt) rl1
      write(OUT, my_fmt) rl1
      my_fmt='(dc, f7.5)'
      write(OUT, my_fmt) rl1
      my_fmt='(f7.5)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, f7.5)'
      write(OUT, my_fmt) rl1

      my_fmt='(g12.7)'
      read(IN_P, my_fmt) rl1
      write(OUT, my_fmt) rl1
      my_fmt='(dc, g12.7)'
      write(OUT, my_fmt) rl1
      my_fmt='(g12.7)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, g12.7)'
      write(OUT, my_fmt) rl1

      my_fmt='(e9.3e2)'
      read(IN_P, my_fmt) rl1
      write(OUT, my_fmt) rl1
      my_fmt='(dc, e9.3e2)'
      write(OUT, my_fmt) rl1
      my_fmt='(e9.3e2)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, e9.3e2)'
      write(OUT, my_fmt) rl1

      my_fmt='(e10.4)'
      read(IN_P, my_fmt) rl1
      write(OUT, my_fmt) rl1
      my_fmt='(dc, e10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(e10.4)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, e10.4)'
      write(OUT, my_fmt) rl1

      my_fmt='(d10.4)'
      read(IN_P, my_fmt) rl1
      write(OUT, my_fmt) rl1
      my_fmt='(dc, d10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(d10.4)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, d10.4)'
      write(OUT, my_fmt) rl1

      my_fmt='(en9.2)'
      read(IN_P, my_fmt) rl1
      write(OUT, my_fmt) rl1
      my_fmt='(dc, en9.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(en9.2)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, en9.2)'
      write(OUT, my_fmt) rl1

      my_fmt='(es8.2)'
      read(IN_P, my_fmt) rl1
      write(OUT, my_fmt) rl1
      my_fmt='(dc, es8.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(es8.2)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, es8.2)'
      write(OUT, my_fmt) rl1

      
      close(IN_P)
      my_fmt='comma'
      open(IN_P, file=FNAME_P, decimal=my_fmt)
! same as above except use DP desc. when reading values

      my_fmt='(dp,f7.5)'
      read(IN_P, my_fmt) rl1
      my_fmt='(f7.5)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, f7.5)'
      write(OUT, my_fmt) rl1
      my_fmt='(f7.5)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, f7.5)'
      write(OUT, my_fmt) rl1

      my_fmt='(dp,g12.7)'
      read(IN_P, my_fmt) rl1
      my_fmt='(g12.7)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, g12.7)'
      write(OUT, my_fmt) rl1
      my_fmt='(g12.7)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, g12.7)'
      write(OUT, my_fmt) rl1

      my_fmt='(dp,e9.3e2)'
      read(IN_P, my_fmt) rl1
      my_fmt='(e9.3e2)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, e9.3e2)'
      write(OUT, my_fmt) rl1
      my_fmt='(e9.3e2)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, e9.3e2)'
      write(OUT, my_fmt) rl1

      my_fmt='(dp,e10.4)'
      read(IN_P, my_fmt) rl1
      my_fmt='(e10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, e10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(e10.4)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, e10.4)'
      write(OUT, my_fmt) rl1

      my_fmt='(dp,d10.4)'
      read(IN_P, my_fmt) rl1
      my_fmt='(d10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, d10.4)'
      write(OUT, my_fmt) rl1
      my_fmt='(d10.4)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, d10.4)'
      write(OUT, my_fmt) rl1

      my_fmt='(dp,en9.2)'
      read(IN_P, my_fmt) rl1
      my_fmt='(en9.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, en9.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(en9.2)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, en9.2)'
      write(OUT, my_fmt) rl1

      my_fmt='(dp,es8.2)'
      read(IN_P, my_fmt) rl1
      my_fmt='(es8.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(dc, es8.2)'
      write(OUT, my_fmt) rl1
      my_fmt='(es8.2)'
      write(OUT, my_fmt, decimal='comma') rl1
      write(OUT, my_fmt, decimal='point') rl1
      my_fmt='(dp, es8.2)'
      write(OUT, my_fmt) rl1

      close(IN_P)

      close(OUT)

      end
