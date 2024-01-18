!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: decimaleditf036.f
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
!*  TEST CASE TITLE            : decimaleditf036
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
!*  REQUIRED COMPILER OPTIONS  : -qlanglvl=77/90/95std/90/95pure
!*  REQUIRED RUN-TIME OPTIONS  : langlvl=90/95std/90/95pure
!*
!*  DESCRIPTION                : This test makes sure that run-time and
!*                               compile-time langlvl settings do not
!*                               affect the functionality of the decimal
!*                               edit mode. ( testing descriptors )
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50), parameter :: FNAME_C = 'decimaleditf036.c.dat',   &
     &                            FNAME_P = 'decimaleditf036.p.dat',   &
     &                            FNAME_OUT = 'decimaleditf036.out'
      integer, parameter :: IN_C = 11, IN_P = 22, OUT = 33 ! unit numbers

      real, dimension(3) :: rl1 = (/3.14, 4.13, 0.78/), rl2

      integer, parameter :: NUM_TESTS = 4 ! number of records in each input files

      character(50) :: buffer, fmt_dc
      
      integer :: i
      
      fmt_dc = '(dc, 3f5.2)'
      
      ! open the unit with the mode in which data has been written to each file
      open(IN_C, file=FNAME_C, decimal='comma')
      open(IN_P, file=FNAME_P, decimal='point')
      open(OUT, file=FNAME_OUT, decimal='point')
      
      write(OUT, '(dp, 3f5.2)') rl1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         read(IN_C, fmt_dc) rl2
         write(OUT, '(dp, 3f5.2)') rl2
         write(OUT, fmt_dc) rl2

         read(IN_P, '(dp, 3f5.2)') rl2
         write(OUT, fmt_dc) rl2
         write(OUT, '(dp, 3f5.2)') rl2
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
      
      write(OUT, '(dp, 3f5.2)') rl1

      ! read everything from input files and output it to OUT
      do i = 1, NUM_TESTS
         read(IN_C, fmt_dc) rl2
         write(OUT, '(dp, 3f5.2)') rl2
         write(OUT, fmt_dc) rl2

         read(IN_P, '(dp, 3f5.2)') rl2
         write(OUT, fmt_dc) rl2
         write(OUT, '(dp, 3f5.2)') rl2
      end do

      close(IN_C)
      close(IN_P)
      close(OUT)

! now test for internal files:
      open(OUT, file=FNAME_OUT, position='append')
      buffer = '1.11,2.22,3.33'
      read(buffer, '(dp, 3f5.2)') rl2
      write(OUT,fmt_dc) rl2
      rl2 = (/4.44, 5.55, 6.66/)
      buffer=''
      write(buffer, '(dp, 3f5.2)') rl2
      write(OUT, *) buffer

      buffer = '1,11;2,22;3,33'
      read(buffer, fmt_dc) rl2
      write(OUT, '(dp, 3f5.2)') rl2
      rl2 = (/4.44, 5.55, 6.66/)
      buffer=''
      write(buffer, fmt_dc) rl2
      write(OUT, *) buffer

      close(OUT)
      
      end
