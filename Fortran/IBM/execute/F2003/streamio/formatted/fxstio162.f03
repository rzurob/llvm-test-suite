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
!*  DESCRIPTION                : Test NameList with arrays in Stream I/O
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/29/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  include 'check_array.inc'

  program fxstio162

     implicit none
     integer    ios, i
     integer, parameter :: N = 4
     integer*4  i4_in(N), i4_out(N)
     real*4     r4_in(N), r4_out(N)
     real*16    r16_in(N) , r16_out(N)
     complex*8  x8_in(N,N), x8_out(N,N)
     logical*8  l8_in(-1:N-2,2:N+1), l8_out(N,N)
     character    ch1_in(N,N), ch1_out(N,N)
     character*15 ch15_in(N), ch15_out(N)
     character*20 ch25_in(N), ch25_out(N)

     NAMELIST /name_in1/ i4_in, r4_in
     NAMELIST /name_in2/ r16_in, x8_in
     NAMELIST /name_in3/ l8_in, ch1_in, ch15_in
     NAMELIST /name_in4/ ch25_in

     NAMELIST /name_out1/ i4_out, r4_out
     NAMELIST /name_out2/ r16_out, x8_out
     NAMELIST /name_out3/ l8_out, ch1_out, ch15_out
     NAMELIST /name_out4/ ch25_out

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3

     include 'check_interface.inc'

!**********************************************************
!        Initialization of variables                      *
!**********************************************************

     do i = 1,N
        i4_out = i * 100
        r4_out = i * .01
     enddo
     r16_out = huge(r16_in) / 2.
     x8_out = (huge(r4_in), -1.0 * huge(r4_in))
     l8_out = .true.
     ch1_out = "A"
     ch15_out = "Beautiful House"
     ch25_out = "It's a newline\n here."


!**********************************************************
!        Writing Namelists to the file                    *
!**********************************************************

     OPEN(1, FILE='fxstio162.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, ACTION='WRITE', DELIM='QUOTE')

     WRITE(1, NML=name_out1, IOSTAT=ios, ERR=91)
     WRITE(1, name_out2, IOSTAT=ios, ERR=91)
     WRITE(1, NML=name_out3, IOSTAT=ios, ERR=91)
     WRITE(1, NML=name_out4, IOSTAT=ios, ERR=91)

     CLOSE(1)

!**********************************************************
!        Reading Namelists from the file                  *
!**********************************************************

     OPEN(1, FILE='fxstio162.in', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='OLD', IOSTAT=ios, ERR=90, ACTION='READ', DELIM='QUOTE')

     READ(1, NML=name_in1, IOSTAT=ios, ERR=92)
     READ(1, name_in2, IOSTAT=ios, ERR=92)
     READ(1, NML=name_in3, IOSTAT=ios, ERR=92)
     READ(1, NML=name_in4, IOSTAT=ios, ERR=92)

!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( .not. Array_Check (i4_in, i4_out)  ) error stop 20

     if ( .not. Array_Check (r4_in, r4_out)  ) error stop 21
     if ( .not. Array_Check (r16_in, r16_out)) error stop 22

     if ( .not. Array_Check (x8_in, x8_out)  ) error stop 23

     if ( .not. Array_Check (l8_in, l8_out)  ) error stop 24

     if ( .not. Array_Check ( ch1_in, ch1_out )) error stop 25
     if ( .not. Array_Check (ch15_in, ch15_out)) error stop 26
       ch25_out = "It's a newline here"
     if ( .not. Array_Check (ch25_in, ch25_out)) error stop 27

     CLOSE(1)

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
