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
!*  DESCRIPTION                : Test derived type in list-directed
!*				 stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/27/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  include 'check_array.inc'

  program fxstio153

     implicit none
     integer    i, j, k, l, ios
     integer, parameter :: N = 10

     type dt1
        integer*4  i4 /-20000000/
        real*4     r4(N) /N*0.000001/
        complex*8  x8 /(6.87532, -0.82531)/
        logical*4  l4 /.true./
        character*15 ch15  /"New Baby Girl!"/
     end type
     type(dt1) dt_in1, dt_out1
     type(dt1) dt_in2(N), dt_out2(N)
     type(dt1) dt_in3(N,N), dt_out3(N,N)

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3

     include 'check_interface.inc'


!**********************************************************
!       Writing and Reading the file                      *
!**********************************************************

     OPEN(1, FILE='fxstio153.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, DELIM='QUOTE')

     WRITE(1, FMT=*, IOSTAT=ios, ERR=91, POS=1) dt_in1
     WRITE(1, FMT=*, IOSTAT=ios, ERR=91, POS=5000) dt_in2
     WRITE(1, FMT=*, IOSTAT=ios, ERR=91, POS=15000) dt_in3


     READ(1, FMT=*, IOSTAT=ios, ERR=92, POS=1) dt_out1
     READ(1, FMT=*, IOSTAT=ios, ERR=92, POS=5000) dt_out2
     READ(1, FMT=*, IOSTAT=ios, ERR=92, POS=15000) dt_out3


!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( dt_in1%i4 .ne. dt_out1%i4 ) error stop 10
     if ( .not. Array_Check (dt_in1%r4, dt_out1%r4) ) error stop 11
     if ( .not. precision_X8(dt_in1%x8, dt_out1%x8) ) error stop 12
     if ( dt_in1%l4 .neqv. dt_out1%l4 ) error stop 13
     if ( dt_in1%ch15 .ne. dt_out1%ch15 ) error stop 14

     i = 1
     j = 1
     do while (i .le. N)
        if ( dt_in2(i)%i4 .ne. dt_out2(i)%i4 ) error stop 15
        if ( .not. Array_Check (dt_in2(i)%r4, dt_out2(i)%r4) ) error stop 16
        if ( .not. precision_X8(dt_in2(i)%x8, dt_out2(i)%x8) ) error stop 17
        if ( dt_in2(i)%l4 .neqv. dt_out2(i)%l4 ) error stop 18
        if ( dt_in2(i)%ch15 .ne. dt_out2(i)%ch15 ) error stop 19

        do while (j .le. N)
           if ( dt_in3(i,j)%i4 .ne. dt_out3(i,j)%i4 ) error stop 20
           if ( .not. Array_Check (dt_in3(i,j)%r4, dt_out3(i,j)%r4) ) &
   &           error stop 21
           if ( .not. precision_X8(dt_in3(i,j)%x8, dt_out3(i,j)%x8) ) &
   &           error stop 22
           if ( dt_in3(i,j)%l4 .neqv. dt_out3(i,j)%l4 ) error stop 23
           if ( dt_in3(i,j)%ch15 .ne. dt_out3(i,j)%ch15 ) error stop 24

           j = j + 1
        end do
        i = i + 1
     end do

     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92
93   print *, "Error while rewinding the file: IOSTAT = ", ios
     error stop 93

   end program

