!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditf017.f
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
!*                               decimal edit mode in combination with
!*                               D, E, EN, ES, F, and G edit descriptors.
!*                               This test deals with compile-time encoding
!*                               of these edit descriptors and decimal mode.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_C = 'decimaleditf017.c.dat',   &
     &                            FNAME_P = 'decimaleditf017.p.dat',   &
     &                            FNAME_OUT = 'decimaleditf017.out'
      integer, parameter :: IN_C = 11, IN_P = 22, OUT = 33 ! unit numbers

      real :: rl1

      ! open the unit with the mode in which data has been written to each file
      open(IN_C, file=FNAME_C, decimal='comma')
      open(IN_P, file=FNAME_P, decimal='point')
      open(OUT, file=FNAME_OUT, decimal='point')

! ***********************************************
! try reading from the file written in comma mode:
! ***********************************************

      read(IN_C, '(f7.5)') rl1
      write(OUT, '(f7.5)') rl1
      write(OUT, '(f7.5)', decimal='point') rl1
      write(OUT, '(dp, f7.5)') rl1
      write(OUT, '(dc, f7.5)') rl1
      write(OUT, '(f7.5)', decimal='comma') rl1

      read(IN_C, '(g12.7)') rl1
      write(OUT, '(g12.7)') rl1
      write(OUT, '(g12.7)', decimal='point') rl1
      write(OUT, '(dp, g12.7)') rl1
      write(OUT, '(dc, g12.7)') rl1
      write(OUT, '(g12.7)', decimal='comma') rl1

      read(IN_C, '(e9.3e2)') rl1
      write(OUT, '(e9.3e2)') rl1
      write(OUT, '(e9.3e2)', decimal='point') rl1
      write(OUT, '(dp, e9.3e2)') rl1
      write(OUT, '(dc, e9.3e2)') rl1
      write(OUT, '(e9.3e2)', decimal='comma') rl1

      read(IN_C, '(e10.4)') rl1
      write(OUT, '(e10.4)') rl1
      write(OUT, '(e10.4)', decimal='point') rl1
      write(OUT, '(dp, e10.4)') rl1
      write(OUT, '(dc, e10.4)') rl1
      write(OUT, '(e10.4)', decimal='comma') rl1

      read(IN_C, '(d10.4)') rl1
      write(OUT, '(d10.4)') rl1
      write(OUT, '(d10.4)', decimal='point') rl1
      write(OUT, '(dp, d10.4)') rl1
      write(OUT, '(dc, d10.4)') rl1
      write(OUT, '(d10.4)', decimal='comma') rl1

      read(IN_C, '(en9.2)') rl1
      write(OUT, '(en9.2)') rl1
      write(OUT, '(en9.2)', decimal='point') rl1
      write(OUT, '(dp, en9.2)') rl1
      write(OUT, '(dc, en9.2)') rl1
      write(OUT, '(en9.2)', decimal='comma') rl1

      read(IN_C, '(es8.2)') rl1
      write(OUT, '(es8.2)') rl1
      write(OUT, '(es8.2)', decimal='point') rl1
      write(OUT, '(dp, es8.2)') rl1
      write(OUT, '(dc, es8.2)') rl1
      write(OUT, '(es8.2)', decimal='comma') rl1


      close(IN_C)
      open(IN_C, file=FNAME_C, decimal='point')
! same as above except use DC desc. when reading values

      read(IN_C, '(dc, f7.5)') rl1
      write(OUT, '(f7.5)') rl1
      write(OUT, '(f7.5)', decimal='point') rl1
      write(OUT, '(dp, f7.5)') rl1
      write(OUT, '(dc, f7.5)') rl1
      write(OUT, '(f7.5)', decimal='comma') rl1

      read(IN_C, '(dc, g12.7)') rl1
      write(OUT, '(g12.7)') rl1
      write(OUT, '(g12.7)', decimal='point') rl1
      write(OUT, '(dp, g12.7)') rl1
      write(OUT, '(dc, g12.7)') rl1
      write(OUT, '(g12.7)', decimal='comma') rl1

      read(IN_C, '(dc, e9.3e2)') rl1
      write(OUT, '(e9.3e2)') rl1
      write(OUT, '(e9.3e2)', decimal='point') rl1
      write(OUT, '(dp, e9.3e2)') rl1
      write(OUT, '(dc, e9.3e2)') rl1
      write(OUT, '(e9.3e2)', decimal='comma') rl1

      read(IN_C, '(dc, e10.4)') rl1
      write(OUT, '(e10.4)') rl1
      write(OUT, '(e10.4)', decimal='point') rl1
      write(OUT, '(dp, e10.4)') rl1
      write(OUT, '(dc, e10.4)') rl1
      write(OUT, '(e10.4)', decimal='comma') rl1

      read(IN_C, '(dc, d10.4)') rl1
      write(OUT, '(d10.4)') rl1
      write(OUT, '(d10.4)', decimal='point') rl1
      write(OUT, '(dp, d10.4)') rl1
      write(OUT, '(dc, d10.4)') rl1
      write(OUT, '(d10.4)', decimal='comma') rl1

      read(IN_C, '(dc, en9.2)') rl1
      write(OUT, '(en9.2)') rl1
      write(OUT, '(en9.2)', decimal='point') rl1
      write(OUT, '(dp, en9.2)') rl1
      write(OUT, '(dc, en9.2)') rl1
      write(OUT, '(en9.2)', decimal='comma') rl1

      read(IN_C, '(dc, es8.2)') rl1
      write(OUT, '(es8.2)') rl1
      write(OUT, '(es8.2)', decimal='point') rl1
      write(OUT, '(dp, es8.2)') rl1
      write(OUT, '(dc, es8.2)') rl1
      write(OUT, '(es8.2)', decimal='comma') rl1

      close(IN_C)
      close(OUT)
! ***********************************************
! try reading from the file written in point mode:
! ***********************************************

      open(OUT, file=FNAME_OUT, decimal='comma', position='append')

      read(IN_P, '(f7.5)') rl1
      write(OUT, '(f7.5)') rl1
      write(OUT, '(dc, f7.5)') rl1
      write(OUT, '(f7.5)', decimal='comma') rl1
      write(OUT, '(f7.5)', decimal='point') rl1
      write(OUT, '(dp, f7.5)') rl1

      read(IN_P, '(g12.7)') rl1
      write(OUT, '(g12.7)') rl1
      write(OUT, '(dc, g12.7)') rl1
      write(OUT, '(g12.7)', decimal='comma') rl1
      write(OUT, '(g12.7)', decimal='point') rl1
      write(OUT, '(dp, g12.7)') rl1

      read(IN_P, '(e9.3e2)') rl1
      write(OUT, '(e9.3e2)') rl1
      write(OUT, '(dc, e9.3e2)') rl1
      write(OUT, '(e9.3e2)', decimal='comma') rl1
      write(OUT, '(e9.3e2)', decimal='point') rl1
      write(OUT, '(dp, e9.3e2)') rl1

      read(IN_P, '(e10.4)') rl1
      write(OUT, '(e10.4)') rl1
      write(OUT, '(dc, e10.4)') rl1
      write(OUT, '(e10.4)', decimal='comma') rl1
      write(OUT, '(e10.4)', decimal='point') rl1
      write(OUT, '(dp, e10.4)') rl1

      read(IN_P, '(d10.4)') rl1
      write(OUT, '(d10.4)') rl1
      write(OUT, '(dc, d10.4)') rl1
      write(OUT, '(d10.4)', decimal='comma') rl1
      write(OUT, '(d10.4)', decimal='point') rl1
      write(OUT, '(dp, d10.4)') rl1

      read(IN_P, '(en9.2)') rl1
      write(OUT, '(en9.2)') rl1
      write(OUT, '(dc, en9.2)') rl1
      write(OUT, '(en9.2)', decimal='comma') rl1
      write(OUT, '(en9.2)', decimal='point') rl1
      write(OUT, '(dp, en9.2)') rl1

      read(IN_P, '(es8.2)') rl1
      write(OUT, '(es8.2)') rl1
      write(OUT, '(dc, es8.2)') rl1
      write(OUT, '(es8.2)', decimal='comma') rl1
      write(OUT, '(es8.2)', decimal='point') rl1
      write(OUT, '(dp, es8.2)') rl1


      close(IN_P)
      open(IN_P, file=FNAME_P, decimal='comma')
! same as above except use DP desc. when reading values

      read(IN_P, '(dp, f7.5)') rl1
      write(OUT, '(f7.5)') rl1
      write(OUT, '(dc, f7.5)') rl1
      write(OUT, '(f7.5)', decimal='comma') rl1
      write(OUT, '(f7.5)', decimal='point') rl1
      write(OUT, '(dp, f7.5)') rl1

      read(IN_P, '(dp, g12.7)') rl1
      write(OUT, '(g12.7)') rl1
      write(OUT, '(dc, g12.7)') rl1
      write(OUT, '(g12.7)', decimal='comma') rl1
      write(OUT, '(g12.7)', decimal='point') rl1
      write(OUT, '(dp, g12.7)') rl1

      read(IN_P, '(dp, e9.3e2)') rl1
      write(OUT, '(e9.3e2)') rl1
      write(OUT, '(dc, e9.3e2)') rl1
      write(OUT, '(e9.3e2)', decimal='comma') rl1
      write(OUT, '(e9.3e2)', decimal='point') rl1
      write(OUT, '(dp, e9.3e2)') rl1

      read(IN_P, '(dp, e10.4)') rl1
      write(OUT, '(e10.4)') rl1
      write(OUT, '(dc, e10.4)') rl1
      write(OUT, '(e10.4)', decimal='comma') rl1
      write(OUT, '(e10.4)', decimal='point') rl1
      write(OUT, '(dp, e10.4)') rl1

      read(IN_P, '(dp, d10.4)') rl1
      write(OUT, '(d10.4)') rl1
      write(OUT, '(dc, d10.4)') rl1
      write(OUT, '(d10.4)', decimal='comma') rl1
      write(OUT, '(d10.4)', decimal='point') rl1
      write(OUT, '(dp, d10.4)') rl1

      read(IN_P, '(dp, en9.2)') rl1
      write(OUT, '(en9.2)') rl1
      write(OUT, '(dc, en9.2)') rl1
      write(OUT, '(en9.2)', decimal='comma') rl1
      write(OUT, '(en9.2)', decimal='point') rl1
      write(OUT, '(dp, en9.2)') rl1

      read(IN_P, '(dp, es8.2)') rl1
      write(OUT, '(es8.2)') rl1
      write(OUT, '(dc, es8.2)') rl1
      write(OUT, '(es8.2)', decimal='comma') rl1
      write(OUT, '(es8.2)', decimal='point') rl1
      write(OUT, '(dp, es8.2)') rl1


      close(IN_P)

      close(OUT)

      end
