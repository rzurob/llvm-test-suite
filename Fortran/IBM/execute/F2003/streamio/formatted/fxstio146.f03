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
!*  DESCRIPTION                : Test T, TL, TR & X edit descriptors with
!*				 formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/26/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  program fxstio146

     implicit none
     integer    ios, pos
     character             :: ch1_in, ch1_out
     character*5           :: ch5_in, ch5_out
     character*10          :: ch10_in, ch10_out
     character*15          :: ch15_in, ch15_out
     character*20          :: ch20_in, ch20_out
     character*25          :: ch25_in, ch25_out
     character*5           :: ch5_ltrl_out /"     "/
     character*6           :: ch6_ltrl_out /"      "/
     character*10          :: ch10_ltrl_out /"          "/
     character*20          :: ch20_ltrl_out /"                    "/
     character*25          :: ch25_ltrl_out /"                         "/

!**********************************************************
!       Allocation, Association & Initialization          *
!**********************************************************

     ch1_in = "A"
     ch5_in = "v811"
     ch10_in = "XLFortran "
     ch20_in = "Beautiful flower!"
     ch25_in = "1234567890 1234567890 123"


!**********************************************************
!      Writing and Reading the file                      *
!**********************************************************

     OPEN(1, FILE='fxstio146.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(T5, A1)', IOSTAT=ios, ERR=91, ADVANCE='no') ch1_in

     WRITE(1, FMT='(TR2, A20)', IOSTAT=ios, ERR=91, POS=10) ch20_in

     WRITE(1, FMT='(T15, A5, TL15, A10)', IOSTAT=ios, ERR=91) ch5_in, ch10_in

     WRITE(1, FMT='(10X, A25)', IOSTAT=ios, ERR=91) ch25_in

      inquire(1, POS=pos)
     print *, pos
     WRITE(1, FMT='(A5)', IOSTAT=ios, ERR=91, ADVANCE='no') "This"
      inquire(1, POS=pos)
     print *, pos

     WRITE(1, FMT='(T5, A10)', IOSTAT=ios, ERR=91, POS=100) "position 5"

     WRITE(1, FMT='(A14, T5, A4, /, "wrong", TL15, A)', IOSTAT=ios, ERR=91) &
    &             "A new line\n is","here", "right!"

     WRITE(1, FMT='(30H    This is the last record!  )', IOSTAT=ios, ERR=91)



     READ(1, FMT='(A1)', IOSTAT=ios, ERR=92, POS=5) ch1_out

     READ(1, FMT='(A20)', IOSTAT=ios, ERR=92, POS=12) ch20_out

     READ(1, FMT='(A5)', IOSTAT=ios, ERR=92, POS=47) ch5_out

     READ(1, FMT='(A10)', IOSTAT=ios, ERR=92, POS=37) ch10_out

     READ(1, FMT='(A25)', IOSTAT=ios, ERR=92, POS=63) ch25_out

     READ(1, FMT='(A5)', IOSTAT=ios, ERR=92) ch5_ltrl_out

     READ(1, FMT='(A10)', IOSTAT=ios, ERR=92, POS=104) ch10_ltrl_out

     READ(1, FMT='(A10)', IOSTAT=ios, ERR=92) ch20_ltrl_out(1:10)

     READ(1, FMT='(A9)', IOSTAT=ios, ERR=92) ch20_ltrl_out(12:20)

     READ(1, FMT='(A)', IOSTAT=ios, ERR=92) ch6_ltrl_out

     READ(1, FMT='(T5,A4,X,A2,TR5,A4,TL8,A3,6X,A7)', IOSTAT=ios, ERR=92)&
    &      ch25_ltrl_out(1:4), ch25_ltrl_out(6:7), ch25_ltrl_out(13:16), &
    &      ch25_ltrl_out(9:11) , ch25_ltrl_out(18:25)


!**********************************************************
!        Checking the Results                             *
!**********************************************************

       print * , ch1_in, ch1_out
     if ( ch1_in .ne. ch1_out ) error stop 20
     if ( ch20_in .ne. ch20_out) error stop 24
     if ( ch5_in .ne. ch5_out) error stop 23
     if ( ch10_in .ne. ch10_out) error stop 22
     if ( ch25_in .ne. ch25_out) error stop 25
     if ( ch5_ltrl_out .ne. " This" ) error stop 30
     if ( ch10_ltrl_out .ne. "position 5" ) error stop 31
     if ( ch20_ltrl_out .ne. "A new line  is here " ) error stop 32
     if ( ch6_ltrl_out .ne. "right!" ) error stop 32
     if ( ch25_ltrl_out .ne. "This is the last record!" ) error stop 34


!     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92

   end program

