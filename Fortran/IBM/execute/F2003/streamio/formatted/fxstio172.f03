!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 2003
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test Non-Advancing Stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/31/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  program fxstio172

     implicit none
     integer    ios, i, pos
     character*30  ch_in1, ch_out1
     character*30  ch_in2, ch_out2
     character*30  ch_in3, ch_out3
     character*30  ch_in4, ch_out4
     character*30  ch_in5, ch_out5
     character*30  ch_in6, ch_out6

     integer*1     i1_out
     integer*2     i2_out
     integer*4     i4_out
     integer*8     i8_out
     real          r4_out
     real*8        r8_out
     complex       x8_out
     complex*32    x32_out
     byte          b_out

     integer    rec_count (5)

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3

!**********************************************************
!      Initialization                                     *
!**********************************************************

     ch_in1 = "This is the first record!"
     ch_in2 = "This is the second record!"
     ch_in3 = " 2345   3.14   -.1D23  230  "
     ch_in4 = "0.1E-2 0.8E+2 0.4Q-2  0.7Q+2  "
     ch_in5 = "This is the last record!"


!**********************************************************
!      Writing to the file                      *
!**********************************************************

     OPEN(1, FILE='fxstio172.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(A/)', IOSTAT=ios, ERR=91, ADVANCE='no') ch_in1

     WRITE(1, FMT='(A)', IOSTAT=ios, ERR=91, ADVANCE='no') ch_in2

     WRITE(1, FMT='(/A)', IOSTAT=ios, ERR=91, ADVANCE='no') ch_in3(1:20)

     WRITE(1, FMT='(A)', IOSTAT=ios, ERR=91) ch_in3(21:)

!
!  Leave a hole in the file
!
     WRITE(1, FMT='(A)', IOSTAT=ios, ERR=91, POS=170,ADVANCE='no') ch_in4(1:10)

     WRITE(1, FMT='(A)', IOSTAT=ios, ERR=91, ADVANCE='no') ch_in4(11:20)

     WRITE(1, FMT='(A)', IOSTAT=ios, ERR=91, ADVANCE='yes') ch_in4(21:)

     WRITE(1, FMT='("1101     367 ")', IOSTAT=ios, ERR=91, ADVANCE='no')

     WRITE(1, FMT='("  F              ")', IOSTAT=ios, ERR=91, ADVANCE='no')

     WRITE(1, FMT="(/'This is the last record!')", IOSTAT=ios, ERR=91, &
    &         ADVANCE='no')


!**********************************************************
!      Checking record positions                          *
!**********************************************************

     INQUIRE(1, POS= pos)
     if ( pos .ne. 256 ) error stop 20 	! End-of-file

     BACKSPACE(1)
     INQUIRE(1, POS= pos)
     if ( pos .ne. 232 ) error stop 21	! 6th record

     BACKSPACE(1)
     INQUIRE(1, POS= pos)
     if ( pos .ne. 201 ) error stop 21 	! 5th record

     BACKSPACE(1)
     INQUIRE(1, POS= pos)
     if ( pos .ne. 94 ) error stop 21 	! 4th record

     BACKSPACE(1)
     INQUIRE(1, POS= pos)
     if ( pos .ne. 63 ) error stop 21 	! 3rd record

     BACKSPACE(1)
     INQUIRE(1, POS= pos)
     if ( pos .ne. 32 ) error stop 21 	! 2nd record

     BACKSPACE(1)
     INQUIRE(1, POS= pos)
     if ( pos .ne. 1 ) error stop 21	! First record


!**********************************************************
!      Reading the file                                   *
!**********************************************************

     READ(1, FMT='(A)', IOSTAT=ios, ERR=92, ADVANCE='no' ) ch_out1

     READ(1, FMT='(/A)', IOSTAT=ios, ERR=92, ADVANCE='yes') ch_out2

     READ(1, FMT='(I7)', IOSTAT=ios, ERR=92, ADVANCE='no' ) i4_out
     READ(1, FMT='(F7.3)', IOSTAT=ios, ERR=92, ADVANCE='no' ) r4_out
     READ(1, FMT='(D8.2)', IOSTAT=ios, ERR=92, ADVANCE='no' ) r8_out
     READ(1, FMT='(I6)', IOSTAT=ios, ERR=92, ADVANCE='no' ) i8_out

     READ(1, FMT='(2E7.2)', IOSTAT=ios, ERR=92, POS=170, ADVANCE='no') x8_out
     READ(1, FMT='(2Q8.2 )', IOSTAT=ios, ERR=92, ADVANCE='no') x32_out

     READ(1, FMT='(/B8)', IOSTAT=ios, ERR=92, ADVANCE='no' ) i1_out
     READ(1, FMT='(O5)', IOSTAT=ios, ERR=92, ADVANCE='no' )  i2_out
     READ(1, FMT='(Z4)', IOSTAT=ios, ERR=92, ADVANCE='no' )  b_out

     READ(1, FMT='(/A24)', IOSTAT=ios, ERR=92, ADVANCE='no' ) ch_out5


!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( ch_in1 .ne. ch_out1 ) error stop 30
     if ( ch_in2 .ne. ch_out2 ) error stop 31
     if ( i4_out .ne. 2345)     error stop 40
     if ( .not. precision_R4(r4_out, 3.14))     error stop 41
     if ( .not. precision_R8(r8_out, -0.1D23)) error stop 42
     if ( i8_out .ne. 230)                      error stop 43
     if ( .not. precision_x8(x8_out, (0.1E-2, 0.8E+2)))      error stop 44
     if ( .not. precision_x3(x32_out, (0.4Q-2, 0.7Q+2))) error stop 45
     if ( i1_out .ne. B'1101') error stop 46
     if ( i2_out .ne. O'367')      error stop 47
     if ( b_out .ne. Z'F')        error stop 48

     if ( ch_out5 .ne. ch_in5 ) error stop 50


!**********************************************************
!        Checking the end-of-file                         *
!**********************************************************

     READ(1, FMT='(A30)', IOSTAT=ios, END=95 ) ch_out5
     error stop 60
95   if ( ios .ne. -1 ) error stop 61

     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92

   end program

