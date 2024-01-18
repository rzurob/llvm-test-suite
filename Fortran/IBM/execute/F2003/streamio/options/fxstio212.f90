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
!*  DESCRIPTION                : Test -qxlf77=noblankpad option with Stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  04/01/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  program fxstio212

     implicit none
     integer    ios, i, pos, eof_pos, size
     character*30  ch22_in, ch22_out
     character*30  ch30_in, ch30_out
     character*30  ch25_in, ch25_out


!**********************************************************
!      Initialization                                     *
!**********************************************************

     ch22_in = "This is a short record"
     ch30_in = "This  one  is  a  long  record"
     ch25_in = "This is a medium record !"


!*************************************************************
! Writing and Reading with PAD='no' while checking EOR & EOF *
!*************************************************************

     OPEN(1, FILE='fxstio212.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(A22)', IOSTAT=ios, ERR=91) ch22_in
     WRITE(1, FMT='(A25)', IOSTAT=ios, ERR=91) ch30_in
     WRITE(1, FMT='(A25)', IOSTAT=ios, ERR=91) ch25_in

     REWIND(1)

     READ(1, FMT='(A25)', SIZE=size, IOSTAT=ios, EOR=93, ERR=92, &
    &                                               ADVANCE='no') ch22_out
     error stop 20
93   if ( ios .ne. -4 ) error stop 21
     if ( size .ne. 22 ) error stop 22
     INQUIRE(1, POS=pos)
     if ( pos .ne. 24 ) error stop 23

     READ(1, FMT='(A30)', SIZE=size, IOSTAT=ios, EOR=94, ERR=92, &
    &                                                ADVANCE='no') ch30_out
     error stop 25
94   if ( ios .ne. -4 ) error stop 26
     if ( size .ne. 25 ) error stop 27
     INQUIRE(1, POS=pos)
     if ( pos .ne. 50 ) error stop 28

     READ(1, FMT='(A25)', SIZE=size, IOSTAT=ios, EOR=95, ERR=92, &
    &                                                ADVANCE='no') ch25_out
     goto 96
95   error stop 25
96   if ( size .ne. 25 ) error stop 30
     INQUIRE(1, POS=pos)
     if ( pos .ne. 75 ) error stop 31



!**********************************************************
!        Checking the end-of-file condition               *
!**********************************************************

!
! EOF for ADVANCE='yes' and noblankpad at the end of file
!
     READ(1, FMT='(A25)', IOSTAT=ios, END=99, POS=pos+1, ERR=92) ch25_out
     error stop 37                      ! No Error
99   if (ios .ne. -1) error stop 38
     INQUIRE(1, POS=eof_pos)
     if ( eof_pos .ne. pos+1 ) error stop 39


!
! EOF for ADVANCE='no' and noblankpad at the end of file
!
     READ(1, FMT='(A25)', SIZE=size, IOSTAT=ios, END=100, POS=pos+1, ERR=92, &
    &                                            ADVANCE='no') ch25_out
     error stop 40                      ! No Error
100  if (ios .ne. -1) error stop 41
     if ( size .ne. 0 ) error stop 42
     INQUIRE(1, POS=eof_pos)
     if ( eof_pos .ne. pos+1 ) error stop 43


!
! EOF for ADVANCE='yes' and noblankpad in the last record
!
     READ(1, FMT='(A25)', IOSTAT=ios, END=101, POS=pos-10, ERR=92) ch25_out
     error stop 44                      ! if no Error
101  if (ios .ne. -1) error stop 45
     if ( ch25_out(:10) .ne. "m record !" ) error stop 46
     INQUIRE(1, POS=eof_pos)
     if ( eof_pos .ne. pos+1 ) error stop 47


!
! EOF for ADVANCE='no' and noblankpad in the last record
!
     READ(1, FMT='(A25)', SIZE=size, IOSTAT=ios, END=102, POS=pos-10, ERR=92, &
    &                                            ADVANCE='no') ch25_out
     error stop 48                      ! No Error
102  if (ios .ne. -1) error stop 49
     if ( size .ne. 10 ) error stop 50
     if ( ch25_out(:10) .ne. "m record !" ) error stop 51
     INQUIRE(1, POS=eof_pos)
     if ( eof_pos .ne. pos+1 ) error stop 52


     CLOSE(1)



!*****************************************************************
! Reading the file again with PAD='yes' while checking EOR & EOF *
!*****************************************************************

     OPEN(1, FILE='fxstio212.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='OLD', IOSTAT=ios, ERR=90, PAD='yes')


     READ(1, FMT='(A25)', SIZE=size, IOSTAT=ios, EOR=201, ERR=92, &
    &                                                ADVANCE='no') ch22_out
     error stop 55
201  if ( ios .ne. -4 ) error stop 56
     if ( size .ne. 22 ) error stop 57
     INQUIRE(1, POS=pos)
     if ( pos .ne. 24 ) error stop 58

     READ(1, FMT='(A30)', SIZE=size, IOSTAT=ios, EOR=202, ERR=92, &
    &                                                ADVANCE='no') ch30_out
     error stop 59
202  if ( ios .ne. -4 ) error stop 60
     if ( size .ne. 25 ) error stop 61
     INQUIRE(1, POS=pos)
     if ( pos .ne. 50 ) error stop 62

     READ(1, FMT='(A25)', SIZE=size, IOSTAT=ios, EOR=203, ERR=92, &
    &                                                ADVANCE='no') ch25_out
     goto 204
203  error stop 63
204  if ( size .ne. 25 ) error stop 64
     INQUIRE(1, POS=pos)
     if ( pos .ne. 75 ) error stop 65



!**********************************************************
!        Checking the end-of-file                         *
!**********************************************************

!
! EOF for PAD='yes', ADVANCE='yes' at the end of file
!
     READ(1, FMT='(A25)', IOSTAT=ios, END=205, POS=pos+1, ERR=92) ch25_out

     error stop 70                          ! No error
205  if( ios .ne. -1 ) error stop 71        ! EOF for PAD='yes', ADVANCE='yes'
     if ( ch25_out .ne. "                         " ) error stop 72
     INQUIRE(1, POS=eof_pos)
     if ( eof_pos .ne. pos+1 ) error stop 73


!
! EOF for PAD='yes', ADVANCE='no' at the end of file
!
     READ(1, FMT='(A25)', SIZE=size, IOSTAT=ios, END=207, POS=pos+1, ERR=92, &
    &                          ADVANCE='no') ch25_out
     error stop 75
207  if ( ios .ne. -1 ) error stop 76       ! EOF for PAD='yes', ADVANCE='no'
     if ( size .ne. 0 ) error stop 77
     if ( ch25_out .ne. "                         " ) error stop 78
     INQUIRE(1, POS=eof_pos)
     if ( eof_pos .ne. pos+1 ) error stop 79


!
! EOF for PAD='yes', ADVANCE='yes' in the last record
!
     READ(1, FMT='(A25)', IOSTAT=ios, END=208, POS=pos-10, ERR=92) ch25_out

     error stop 80                          ! No error
208  if( ios .ne. -1 ) error stop 81        ! EOF for PAD='yes', ADVANCE='yes'
     if ( ch25_out .ne. "m record !               " ) error stop 82
     INQUIRE(1, POS=eof_pos)
     if ( eof_pos .ne. pos+1 ) error stop 83

!
! EOF for PAD='yes', ADVANCE='no' in the last record
!
     READ(1, FMT='(A25)', SIZE=size, IOSTAT=ios, END=209, POS=pos-10, ERR=92, &
    &                          ADVANCE='no') ch25_out
     error stop 85                          ! No error
209  if ( ios .ne. -1 ) error stop 86
     if ( size .ne. 10 ) error stop 87
     if ( ch25_out .ne. "m record !               " ) error stop 88
     INQUIRE(1, POS=eof_pos)
     if ( eof_pos .ne. pos+1 ) error stop 89



     CLOSE(1, STATUS='DELETE')
     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92

   end program

