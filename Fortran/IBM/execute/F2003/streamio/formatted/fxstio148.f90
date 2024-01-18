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
!*  DESCRIPTION                : Test Control Edit Descriptors (/,:,$,',",BN,BZ)
!*				 in formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/26/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  program fxstio148

     implicit none
     integer    ios, i
     character*30  ch_in1, ch_out1
     character*30  ch_in2, ch_out2
     character*30  ch_in3, ch_out3
     character*30  ch_in4, ch_out4
     character*30  ch_in5, ch_out5
     character*30  ch_in6, ch_out6
     character*30  ch_in7, ch_out7

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
     ch_in3 = "This is record number:  3"
     ch_in4 = "This is record number:  4"
     ch_in5 = "This is record number:  5"
     ch_in6 = " 2345   3.14   -.1D23  230  "
     ch_in7 = "0.1E-2 0.8E+2 0.4Q-2  0.7Q+2  "

     do i = 1, 5
         rec_count(i) = i
     end do


!**********************************************************
!      Writing and Reading the file                      *
!**********************************************************

     OPEN(1, FILE='fxstio148.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(A/A/)', IOSTAT=ios, ERR=91, ADVANCE='no') ch_in1, ch_in2

     WRITE(1, FMT='(8(:"This is record number: ", I2 /))', IOSTAT=ios, ERR=91, &
    &       ADVANCE='no')   (rec_count(i), i=3,5)

     WRITE(1, FMT='($, A)', IOSTAT=ios, ERR=91) ch_in6(1:20)

     WRITE(1, FMT='(A)', IOSTAT=ios, ERR=91) ch_in6(21:)

     WRITE(1, FMT='(/A, $, A)', IOSTAT=ios, ERR=91, POS=170) &
    &       ch_in7(1:10), ch_in7(11:20)

     WRITE(1, FMT='(A)', IOSTAT=ios, ERR=91) ch_in7(21:)

     WRITE(1, FMT='("1101     367 ", $)', IOSTAT=ios, ERR=91)

     WRITE(1, FMT='("  F             ", $)', IOSTAT=ios, ERR=91)

     WRITE(1, FMT="(/'This is the last record')", IOSTAT=ios, ERR=91)


     REWIND(1)


     READ(1, FMT='(A)', IOSTAT=ios, ERR=92 ) ch_out1

     READ(1, FMT='(A/)', IOSTAT=ios, ERR=92) ch_out2

     READ(1, FMT='(5(:A/))', IOSTAT=ios, ERR=92) ch_out4, ch_out5

     READ(1, FMT='(A//)', IOSTAT=ios, ERR=92, POS=63) ch_out3

     READ(1, FMT='(BZ, I7, F7.3, D8.2, BN, I6 )', IOSTAT=ios, ERR=92 ) &
    &      i4_out, r4_out, r8_out, i8_out

     READ(1, FMT='(2E7.2, BZ, 2Q8.2 )', IOSTAT=ios, ERR=92 ) &
    &      x8_out, x32_out

     READ(1, FMT='(BZ, B8, O5, Z4)', IOSTAT=ios, ERR=92 ) &
    &      i1_out, i2_out, b_out

     READ(1, FMT='(A)', IOSTAT=ios, ERR=92 ) ch_out7


!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( ch_in1 .ne. ch_out1 ) error stop 20
     if ( ch_in2 .ne. ch_out2 ) error stop 21
     if ( ch_in3 .ne. ch_out3 ) error stop 22
     if ( ch_in4 .ne. ch_out4 ) error stop 23
     if ( ch_in5 .ne. ch_out5 ) error stop 24

     if ( i4_out .ne. 234500)                   error stop 30
     if ( .not. precision_R4(r4_out, 3.14))     error stop 31
     if ( .not. precision_R8(r8_out, -0.1D230)) error stop 32
     if ( i8_out .ne. 230)                      error stop 33

     if ( .not. precision_x8(x8_out, (0.1E-2, 0.8E+2)))      error stop 34
     if ( .not. precision_x3(x32_out, (0.4Q-200, 0.7Q+200))) error stop 35

     if ( i1_out .ne. B'11010000') error stop 36
     if ( i2_out .ne. O'3670')      error stop 37
     if ( b_out .ne. Z'F0')        error stop 38

     if ( ch_out7 .ne. "This is the last record       " ) error stop 40

     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92

   end program

