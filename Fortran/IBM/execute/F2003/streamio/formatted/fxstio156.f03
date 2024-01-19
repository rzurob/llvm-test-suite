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
!*  DESCRIPTION                : Test parameter with list-directed
!*				 stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/28/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  program fxstio156

     implicit none
     integer    ios
     integer*2, parameter    :: i2_in = 1234
     integer*2 	             :: i2_out
     integer*4, parameter    :: i4_in = -20000000
     integer*4               :: i4_out
     integer*8, parameter    :: i8_in = 1234567890
     integer*8 	             :: i8_out
     real*4, parameter       :: r4_in = -0.000001
     real*4    	             :: r4_out
     real*8, parameter       :: r8_in = -0.1D-309
     real*8                  :: r8_out
     real*16, parameter      :: r16_in = 3.14Q300
     real*16                 :: r16_out
     complex*8, parameter    :: x8_in = (1.876, -98.654)
     complex*8               :: x8_out
     complex*16, parameter   :: x16_in = (0.0D0, -0.0D0)
     complex*16              :: x16_out
     complex*32, parameter   :: x32_in = (-0.1Q-309, 0.1Q309)
     complex*32              :: x32_out
     logical*2, parameter    :: l2_in = .true.
     logical*2               :: l2_out
     logical*4, parameter    :: l4_in = .false.
     logical*4               :: l4_out
     logical*8, parameter    :: l8_in = .true.
     logical*8               :: l8_out
     character, parameter    :: ch1_in = 'A'
     character               :: ch1_out
     character*15, parameter :: ch15_in = "New Baby Girl! "
     character*15            :: ch15_out
     byte, parameter         :: b_in = b'01010111'
     byte                    :: b_out

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3


!**********************************************************
!        Writing and Reading the file                     *
!**********************************************************

     OPEN(1, FILE='fxstio156.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, DELIM='QUOTE')

     WRITE(1, FMT=*, IOSTAT=ios, ERR=91) i2_in, i4_in, i8_in
     WRITE(1, FMT=*, IOSTAT=ios, ERR=91, POS=100) &
    &      r4_in, r8_in, r16_in
     WRITE(1, FMT=*, IOSTAT=ios, ERR=91, POS=400) &
    &      x8_in, x16_in, x32_in
     WRITE(1, FMT=*, IOSTAT=ios, ERR=91) l2_in, l4_in, l8_in
     WRITE(1, FMT=*, IOSTAT=ios, ERR=91) ch1_in, ch15_in
     WRITE(1, FMT=*, IOSTAT=ios, ERR=91) b_in

     REWIND(1, IOSTAT=ios, ERR=93)

     READ(1, FMT=*, IOSTAT=ios, ERR=92) i2_out, i4_out, i8_out
     READ(1, FMT=*, IOSTAT=ios, ERR=92, POS=100) &
    &     r4_out, r8_out, r16_out
     READ(1, FMT=*, IOSTAT=ios, ERR=92, POS=400) &
    &      x8_out, x16_out, x32_out
     READ(1, FMT=*, IOSTAT=ios, ERR=92) l2_out, l4_out, l8_out
     READ(1, FMT=*, IOSTAT=ios, ERR=92) ch1_out, ch15_out
     READ(1, FMT=*, IOSTAT=ios, ERR=92) b_out


!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( i2_in .ne. i2_out ) error stop 10
     if ( i4_in .ne. i4_out ) error stop 11
     if ( i8_in .ne. i8_out ) error stop 12

     if ( .not. precision_R4(r4_in, r4_out)) error stop 13
     if ( .not. precision_R8(r8_in, r8_out)) error stop 14
     if ( .not. precision_R6(r16_in, r16_out)) error stop 15

     if ( .not. precision_x8(x8_in,  x8_out )) error stop 16
     if ( .not. precision_x6(x16_in, x16_out)) error stop 17
     if ( .not. precision_x3(x32_in, x32_out)) error stop 18

     if ( l2_in .neqv. l2_out) error stop 19
     if ( l4_in .neqv. l4_out) error stop 20
     if ( l8_in .neqv. l8_out) error stop 21

     if ( ch1_in .ne. ch1_out ) error stop 22
     if ( ch15_in .ne. ch15_out ) error stop 23

     if ( b_in .ne. b_out) error stop 24

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
