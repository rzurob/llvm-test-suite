!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP:  fxstio149.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 2003
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : WRITE
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test S, SS, SP Control Edit Descriptors
!*				 in formatted stream WRITE.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/27/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  program fxstio149

     implicit none
     integer    ios
     character*30  ch_in1
     character*30  ch_in2
     character*30  ch_in3
     character*30  ch_in4
     character*30  ch_in5
     character*30  ch_in6
     character*30  ch_in7

     integer*2     i2_out
     integer*4     i4_out
     integer*8     i8_out
     real*4        r4_out
     real*8        r8_out
     real*16       r16_out
     complex       x8_out
     complex*16    x16_out
     complex*32    x32_out


!**********************************************************
!      Initialization                                     *
!**********************************************************

     i2_out = 123
     i4_out = 123456
     i8_out = -12345678
     r4_out = 1.8956
     r8_out = 0.43D20
     r16_out = 0.43D-20
     x8_out = (-0.43E20, 0.13E-22)
     x16_out = (0.43D20, 0.13D-22)
     x32_out = (0.43Q20, -0.13Q-22)


!**********************************************************
!      Writing and Reading the file                      *
!**********************************************************

     OPEN(1, FILE='fxstio149.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(I5, SP, I10, I12)', IOSTAT=ios, ERR=91) &
    &       i2_out, i4_out, i8_out
     WRITE(1, FMT='(F8.4, SP, D10.2, S, Q10.2)', IOSTAT=ios, ERR=91) &
    &       r4_out, r8_out, r16_out
     WRITE(1, FMT='(SP, 2E10.2, SS, 2D10.2, SP, 2Q10.2)', IOSTAT=ios, ERR=91) &
    &       x8_out, x16_out, x32_out


     REWIND(1)

     READ(1, FMT='(A)', IOSTAT=ios, ERR=92 ) ch_in1
     READ(1, FMT='(A)', IOSTAT=ios, ERR=92 ) ch_in2
     READ(1, FMT='(A, A)', IOSTAT=ios, ERR=92 ) ch_in3, ch_in4


!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( ch_in1 .ne. "  123   +123456   -12345678   " ) error stop 20
     if ( ch_in2 .ne. "  1.8956 +0.43D+20  0.43Q-20  " ) error stop 21
     if ( ch_in3 .ne. " -0.43E+20 +0.13E-22  0.43D+20" ) error stop 23
     if ( ch_in4 .ne. "  0.13D-22 +0.43Q+20 -0.13Q-22" ) error stop 24

     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92

   end program

