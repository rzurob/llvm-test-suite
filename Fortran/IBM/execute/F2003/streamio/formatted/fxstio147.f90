!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: cp $TR_SRC/fxstio147.in .
! %COMPOPTS: -qqcount
! %GROUP:  fxstio147.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f fxstio147.in
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
!*  PRIMARY FUNCTIONS TESTED   : OPEN, READ
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test Q & Qw.d edit descriptors with
!*				 formatted stream input.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments: 
!*  03/26/03   BC     Initial version 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890 


  program fxstio147 

     implicit none
     integer  ios, qcount1, qcount2, qcount3
     integer, parameter :: N = 10
     real*16      :: r16_in
     complex*32   :: x32_in

     logical precision_R6, precision_X3


!********************************************************** 
!      Open and Read the file                             *
!********************************************************** 

     OPEN(1, FILE='fxstio147.in', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='OLD', IOSTAT=ios, ERR=90)


     READ(1, FMT='(Q, Q10.4, Q, 2Q16.9, Q)', IOSTAT=ios, ERR=92) &
    &     qcount1, r16_in, qcount2, x32_in, qcount3

     if ( .not. precision_R6(r16_in, -0.1Q-2))   error stop 21
     if ( .not. precision_X3(x32_in, (0.179769313Q309, 0.179769313Q-309))) &
    &         error stop 22

     if ( qcount1 .ne. 67 ) error stop 23
     if ( qcount2 .ne. 57 ) error stop 23
     if ( qcount3 .ne. 25 ) error stop 23


     READ(1, FMT='(Q, Q11.2, Q, 2Q11.4, Q)', IOSTAT=ios, ERR=92, ADVANCE='no') &
    &     qcount1, r16_in, qcount2, x32_in, qcount3 

     if ( .not. precision_R6(r16_in, -0.31Q10))   error stop 30
     if ( .not. precision_X3(x32_in, (0.1797Q-5, 0.17979Q+20))) error stop 31

     if ( qcount1 .ne. 33 ) error stop 32
     if ( qcount2 .ne. 22 ) error stop 33
     if ( qcount3 .ne. 0 ) error stop 34

     READ(1, FMT='(Q)', IOSTAT=ios, ERR=92) qcount1 
     if ( qcount1 .ne. 0 ) error stop 36

     READ(1, FMT='(Q, T5, Q, TR5, Q)', IOSTAT=ios, ERR=92) &
    &       qcount1, qcount2, qcount3 

     if ( qcount1 .ne. 24 ) error stop 40
     if ( qcount2 .ne. 20 ) error stop 41
     if ( qcount3 .ne. 15 ) error stop 42

     READ(1, FMT='(Q, TL10, Q, 15X, Q)', IOSTAT=ios, ERR=92, POS=120) &
    &       qcount1, qcount2, qcount3 

      print *, qcount1, qcount2, qcount3
     if ( qcount1 .ne. 7 ) error stop 50
     if ( qcount2 .ne. 7 ) error stop 51
     if ( qcount3 .ne. 0 ) error stop 52

     CLOSE(1)

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90 
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92 

   end program

