!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qxlf77=gedit77
! %GROUP:  fxstio215.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: diff fxstio215.dat $TR_SRC/fxstio215.vf  &&  rm -f fxstio215.dat
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : I/O Stream Access Mode
!*
!*  PROGRAMMER                 : Bahram Chehrazy
!*  DATE                       : March 2003
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test -qxlf77=gedit77 with G edit descriptors 
!*				 in formatted stream output.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments: 
!*  04/01/03   BC     Initial version 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890 

  program fxstio215 

     implicit none
     integer    ios 

     real*4                :: r4_in, r4_out  
     real*8                :: r8_in, r8_out  
     real*16               :: r16_in, r16_out  
     complex*8             :: x8_in, x8_out  
     complex*16            :: x16_in, x16_out  
     complex*32            :: x32_in, x32_out  
     real*4                :: r4_ltrl_in
     complex*8             :: x8_ltrl_in


!********************************************************** 
!       Initialization                                    *
!********************************************************** 

     r4_out = -0.099995
     r8_out = 99.5
     r16_out= huge(r16_in) / 2.
     x8_out = (huge(r4_in), -1.0 * huge(r4_in))
     x16_out= (0.0D0, -0.0D0)
     x32_out = (-0.1Q-309, 0.1Q309)


!********************************************************** 
!      Writing and Reading the file                      *
!********************************************************** 

     OPEN(1, FILE='fxstio215.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(G10.2,G10.2,G30.14)', IOSTAT=ios, ERR=91) &
    &      r4_out, r8_out, r16_out
     WRITE(1, FMT='(2G15.7,2G25.10,2G30.14Q4)', IOSTAT=ios, ERR=91) &
    &      x8_out, x16_out, x32_out

     WRITE(1, FMT='(3G11.3)', IOSTAT=ios, ERR=91) 31.4845, 999.997, -.02999


     CLOSE(1)

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90 
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91 

   end program

