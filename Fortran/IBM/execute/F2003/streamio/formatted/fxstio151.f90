!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP:  fxstio151.f
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
!*  DESCRIPTION                : Test List-Directed Stream I/O with 
!*                               differet interinsic data types 
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments: 
!*  03/27/03   BC     Initial version 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890 

  program fxstio151 

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
     byte       b_in, b_out

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3
	
!********************************************************** 
!        Initialization of variables                      *
!********************************************************** 

     i1_in = 12
     i2_in = 1234
     i4_in = -20000000
     i8_in = huge(i8_in)
     r4_in = -0.000001
     r8_in = huge(r8_in)
     r16_in = huge(r16_in) / 2.
     d8_in = -0.1D-309
     x8_in = (huge(r4_in), -1.0 * huge(r4_in))
     x16_in = (0.0D0, -0.0D0)
     x32_in = (-0.1Q-309, 0.1Q309)
     l2_in = .true.
     l4_in = .false.
     l8_in = .true.
     ch1_in = "A"
     ch15_in = " New Baby Girl!"
     b_in = b'01010111'


!********************************************************** 
!        Writing and Reading the file                     *
!********************************************************** 

     OPEN(1, FILE='fxstio151.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, DELIM="QUOTE")

     WRITE(1, FMT=*, IOSTAT=ios, ERR=91, POS=10) i1_in, i2_in, i4_in, i8_in

     WRITE(1, FMT=*, IOSTAT=ios, ERR=91, POS=300) r4_in, r8_in, r16_in

     WRITE(1, FMT=*, IOSTAT=ios, ERR=91) d8_in

     WRITE(1, FMT=*, IOSTAT=ios, ERR=91) x8_in, x16_in, x32_in

     WRITE(1, FMT=*, IOSTAT=ios, ERR=91) l2_in, l4_in, l8_in

     WRITE(1, FMT=*, IOSTAT=ios, ERR=91, POS=700) ch1_in, ch15_in

     WRITE(1, FMT=*, IOSTAT=ios, ERR=91) b_in


     REWIND(1, IOSTAT=ios, ERR=93)


     READ(1, FMT=*, IOSTAT=ios, ERR=92, POS=10) i1_out, i2_out, i4_out, i8_out

     READ(1, FMT=*, IOSTAT=ios, ERR=92, POS=300) r4_out, r8_out, r16_out

     READ(1, FMT=*, IOSTAT=ios, ERR=92) d8_out

     READ(1, FMT=*, IOSTAT=ios, ERR=92) x8_out, x16_out, x32_out

     READ(1, FMT=*, IOSTAT=ios, ERR=92) l2_out, l4_out, l8_out

     READ(1, FMT=*, IOSTAT=ios, ERR=92, POS=700) ch1_out, ch15_out

     READ(1, FMT=*, IOSTAT=ios, ERR=92) b_out

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

     if ( b_in .ne. b_out) error stop 26

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
