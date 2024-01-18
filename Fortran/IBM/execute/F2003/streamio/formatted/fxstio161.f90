!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: cp $TR_SRC/fxstio161.in .
! %COMPOPTS: 
! %GROUP:  fxstio161.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: spiff fxstio161.dat $TR_SRC/fxstio161.vf && rm -f fxstio161.dat fxstio161.in 
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : I/O Stream Access
!*
!*  PROGRAMMER                 : Bahram Chehrazy
!*  DATE                       : March 2003
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test NameList with differet interinsic data 
!*                               types in Stream I/O
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments: 
!*  03/29/03   BC     Initial version 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890 

  program fxstio161 

     implicit none
     integer    ios
     integer*1 	i1_in, i1_out
     integer*2 	i2_in, i2_out
     integer*4 	i4_in, i4_out
     integer*8 	i8_in, i8_out
     real*4    	r4_in, r4_out
     real*8     r8_in  , r8_out
     real*16    r16_in , r16_out
     double precision  d8_in  , d8_out
     complex*8  x8_in, x8_out
     complex*16 x16_in, x16_out
     complex*32 x32_in, x32_out
     logical*2 	l2_in, l2_out
     logical*4 	l4_in, l4_out
     logical*8 	l8_in, l8_out
     character    ch1_in, ch1_out  
     character*15 ch15_in, ch15_out  
     character*20 ch25_in, ch25_out  
     byte       b_in, b_out

     NAMELIST /name_in1/ i1_in, i2_in, i4_in, i8_in, r4_in, r8_in, r16_in
     NAMELIST /name_in2/ d8_in, x8_in, x16_in, x32_in
     NAMELIST /name_in3/ l2_in, l4_in, l8_in, ch1_in, ch15_in, b_in         
     NAMELIST /name_in4/ ch25_in

     NAMELIST /name_out1/ i1_out, i2_out, i4_out, i8_out, r4_out,r8_out,r16_out
     NAMELIST /name_out2/ d8_out, x8_out, x16_out, x32_out
     NAMELIST /name_out3/ l2_out, l4_out, l8_out, ch1_out, ch15_out, b_out 
     NAMELIST /name_out4/ ch25_out

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3
	
!********************************************************** 
!        Initialization of variables                      *
!********************************************************** 

     i1_out = 12
     i2_out = 1234
     i4_out = -20000000
     i8_out = huge(i8_in)
     r4_out = -0.000001
     r8_out = huge(r8_in)
     r16_out = huge(r16_in) / 2.
     d8_out = -0.1D-309
     x8_out = (huge(r4_in), -1.0 * huge(r4_in))
     x16_out = (0.0D0, -0.0D0)
     x32_out = (-0.1Q-309, 0.1Q309)
     l2_out = .true.
     l4_out = .false.
     l8_out = .true.
     ch1_out = "A"
     ch15_out = "Beautiful House"
     ch25_out = "It's a newline\n here."
     b_out = b'01010111'


!********************************************************** 
!        Writing Namelists to the file                    *
!********************************************************** 

     OPEN(1, FILE='fxstio161.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, ACTION='WRITE', DELIM='QUOTE')

     WRITE(1, NML=name_out1, IOSTAT=ios, ERR=91)  
     WRITE(1, name_out2, IOSTAT=ios, ERR=91)  
     WRITE(1, NML=name_out3, IOSTAT=ios, ERR=91)
     WRITE(1, NML=name_out4, IOSTAT=ios, ERR=91)

     CLOSE(1)

!********************************************************** 
!        Reading Namelists from the file                  *
!********************************************************** 

     OPEN(1, FILE='fxstio161.in', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='OLD', IOSTAT=ios, ERR=90, ACTION='READ', DELIM='QUOTE')

     READ(1, NML=name_in1, IOSTAT=ios, ERR=92) 
     READ(1, name_in2, IOSTAT=ios, ERR=92) 
     READ(1, NML=name_in3, IOSTAT=ios, ERR=92) 
     READ(1, NML=name_in4, IOSTAT=ios, ERR=92) 

!********************************************************** 
!        Checking the Results                             *
!********************************************************** 

     if ( i1_in .ne. i1_out ) error stop 10
     if ( i2_in .ne. i2_out ) error stop 11
     if ( i4_in .ne. i4_out ) error stop 12
     if ( i8_in .ne. i8_out ) error stop 13

     if ( .not. precision_R4(r4_in, r4_out)) error stop 14
     if ( .not. precision_R8(r8_in, r8_out)) error stop 15
     if ( .not. precision_R6(r16_in, r16_out)) error stop 16

     if ( .not. precision_R8(d8_in, d8_out) ) error stop 17

     if ( .not. precision_x8(x8_in,  x8_out )) error stop 18
     if ( .not. precision_x6(x16_in, x16_out)) error stop 19
     if ( .not. precision_x3(x32_in, x32_out)) error stop 20

     if ( l2_in .neqv. l2_out) error stop 21
     if ( l4_in .neqv. l4_out) error stop 22
     if ( l8_in .neqv. l8_out) error stop 23

     if ( ch1_in .ne. ch1_out ) error stop 24
     if ( ch15_in .ne. ch15_out ) error stop 25
     if ( ch25_in .ne. "It's a newline here" ) error stop 26

     if ( b_in .ne. b_out) error stop 27

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
