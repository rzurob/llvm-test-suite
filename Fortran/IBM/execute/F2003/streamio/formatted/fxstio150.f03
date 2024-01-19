!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 2003
!*
!*  PRIMARY FUNCTIONS TESTED   : WRITE, READ
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test P Edit descriptor
!*				 in formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/27/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  program fxstio150

     implicit none
     integer    ios
     character*30  ch_out1 /" 23.9876  -5.31981  45.37621  "/
     character*30  ch_out2 /" 3.125 -5.341  4.59822 0.23D-2"/
     character*30  ch_out3 /" -2.652Q-30 7.9821Q-25        "/

     real*4        r4_out, r4_in, r4_in1
     real*8        r8_out, r8_in
     real*16       r16_out, r16_in
     complex       x8_out, x8_in
     complex*16    x16_out, x16_in
     complex*32    x32_out, x32_in

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3


!**********************************************************
!      Initialization                                     *
!**********************************************************

     r4_out = 1.8946231
     r8_out = 0.4378241D20
     r16_out = 0.4378241D-20
     x8_out  = (-0.43E20, 0.13E-22)
     x16_out = (0.43D20, 0.13D-22)
     x32_out = (0.43Q20, -0.13Q-22)


!**********************************************************
!      Testing P edit descriptor in WRITE stmt            *
!**********************************************************

     OPEN(1, FILE='fxstio150.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(-2P, F11.7, D17.9, Q17.9)', IOSTAT=ios, ERR=91) &
    &       r4_out, r8_out, r16_out
     WRITE(1, FMT='(2E10.2, 1P, 2D10.2, 2Q10.2)', IOSTAT=ios, ERR=91) &
    &       x8_out, x16_out, x32_out
     WRITE(1, FMT='(3P, ES15.7, EN15.7)', IOSTAT=ios, ERR=91) &
    &       r4_out, r4_out
     WRITE(1, FMT='(-2P, G12.7, G17.9, G17.9)', IOSTAT=ios, ERR=91) &
    &       r4_out, r8_out, r16_out


     CLOSE(1)


!**********************************************************
!      Testing P edit descriptor in READ stmt             *
!**********************************************************

     OPEN(2, FILE='fxstio150.in', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)


     WRITE(2, FMT='(A/A,A,A)', IOSTAT=ios, ERR=91) &
    &        ch_out1, ch_out2, ch_out3, "3.1452"


     REWIND(2)


     READ(2, FMT='(2P, F9.5, D10.4, Q10.6)', IOSTAT=ios, ERR=92) &
    &       r4_in, r8_in, r16_in

     READ(2, FMT='(-1P, 2E7.3, D8.5, D8.2, 2Q11.4)', IOSTAT=ios, &
    &       ERR=92, ADVANCE='no') x8_in, x16_in, x32_in

     READ(2, FMT='( TR5, F9.5)', IOSTAT=ios, ERR=92) &
    &       r4_in1

!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( .not. precision_R4(r4_in, 23.9876E-2))   error stop 20
     if ( .not. precision_R8(r8_in, -5.31981D-2))  error stop 21
     if ( .not. precision_R8(r16_in, 45.37621Q-2)) error stop 22

     if ( .not. precision_X8(x8_in,  (3.125, -5.341)*10))       error stop 23
     if ( .not. precision_X6(x16_in, (4.59822D1, 0.23D-2)))     error stop 24
     if ( .not. precision_X3(x32_in, (-2.652Q-30, 7.9821Q-25)))error stop 25

     if ( .not. precision_R4(r4_in1, 3.1452))   error stop 26

     CLOSE(2, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92

   end program

