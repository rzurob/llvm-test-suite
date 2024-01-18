!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: cp $TR_SRC/check_array.inc .; cp $TR_SRC/check_interface.inc .
! %COMPOPTS: 
! %GROUP:  fxstio102.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f check_array.inc check_interface.inc
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
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test arrays and implied-do list with
!*				 formatted synchronous stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments: 
!*  03/03/03   BC     Initial version 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890 

  include 'check_array.inc'

  program fxstio102 

     implicit none
     integer    i, j, k, l, ios
     integer, parameter :: N = 10
     integer*4 	i4_in(N), i4_out(N)
     real*4    	r4_in(N), r4_out(N)
     real*16    r16_in(N) , r16_out(N)
     complex*8  x8_in(N,N), x8_out(N,N)
     logical*8 	l8_in(-1:N-2,2:N+1), l8_out(N,N)
     character    ch1_in(N,N), ch1_out(N,N)  
     character*15 ch15_in(N), ch15_out(N)  

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3
	
     include 'check_interface.inc'
     
!********************************************************** 
!        Initialization of arrays                         *
!********************************************************** 

     i4_in = -20000000
     r4_in = -0.000001
     r16_in = huge(r16_in) / 2.
     x8_in = (huge(r4_in), -1.0 * huge(r4_in))
     l8_in = .true.
     ch1_in = "A"
     ch15_in = "New Baby Girl!"

!********************************************************** 
!       Writing and Reading the file                      *
!********************************************************** 

     OPEN(1, FILE='fxstio102.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(10I10)', IOSTAT=ios, ERR=91) (i4_in(i),i=1,N)
     WRITE(1, FMT='(10F9.6, 10Q40.32)', IOSTAT=ios, ERR=91) &
    &     (r4_in(i), i=1,N/2), (r4_in(i), i=N/2+1,N) , r16_in
     WRITE(1, FMT='(200E15.7)', IOSTAT=ios, ERR=91) ((x8_in(i,j),i=1,N),j=1,N)
     WRITE(1, FMT='(100L5)', IOSTAT=ios, ERR=91) &
    &     ((l8_in(i,j),i=-1,N-2),j=2,N+1)
     WRITE(1, FMT='(100A1, 10A16)', IOSTAT=ios, ERR=91) ch1_in, ch15_in

     REWIND(1, IOSTAT=ios, ERR=93)

     READ(1, FMT='(10I10)', IOSTAT=ios, ERR=92) (i4_out(i),i=1,N)
     READ(1, FMT='(10F9.6,10Q40.32)', IOSTAT=ios, ERR=92) &
   &     r4_out, (r16_out(i), i=1,N/2), (r16_out(i), i=N/2+1,N)
     READ(1, FMT='(200E15.7)', IOSTAT=ios, ERR=92) ((x8_out(i,j),i=1,N),j=1,N)
     READ(1, FMT='(100L5)', IOSTAT=ios, ERR=92) l8_out
     READ(1, FMT='(100A1,10A16)', IOSTAT=ios, ERR=92) ch1_out, ch15_out


!********************************************************** 
!        Checking the Results                             *
!********************************************************** 

     if ( .not. Array_Check (i4_in, i4_out)  ) error stop 10

     if ( .not. Array_Check (r4_in, r4_out)  ) error stop 11
     if ( .not. Array_Check (r16_in, r16_out)) error stop 12

     if ( .not. Array_Check (x8_in, x8_out)  ) error stop 13

     if ( .not. Array_Check (l8_in, l8_out)  ) error stop 14

     if ( .not. Array_Check ( ch1_in, ch1_out )) error stop 15
     if ( .not. Array_Check (ch15_in, ch15_out)) error stop 16

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

