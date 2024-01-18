!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 2003
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ, INQUIRE
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test POSITION= specifier in OPEN statement
!*                               with formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/19/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  program fxstio123

     implicit none
     integer    ios
     character*25 :: ch1_in='This is the first record.', ch1_out
     integer*4 	  :: i4_in /1234567/ , i4_out
     real*4    	  :: r4_in /-0.000001/, r4_out
     complex*8    :: x8_in /(-0.1Q-39, 0.1Q39)/, x8_out
     logical*4 	  :: l4_in /.false./, l4_out
     character*25 :: ch2_in='This is the last record.' , ch2_out

     character*15  access, form, asynch, stream, position
     integer    pos, old_size, new_size

!**********************************************************
!   Opening a new file and writing some data into it      *
!**********************************************************

     OPEN(1, FILE='fxstio123.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='NEW', IOSTAT=ios, ERR=90, ACTION='WRITE' )

     WRITE(1, FMT='(A25)', IOSTAT=ios, ERR=91) ch1_in
     WRITE(1, FMT='(I7)', IOSTAT=ios, ERR=91) i4_in
     WRITE(1, FMT='(F9.6)', IOSTAT=ios, ERR=91) r4_in
     WRITE(1, FMT='(2E15.7)',IOSTAT=ios,ERR=91) x8_in
     WRITE(1, FMT='(L5)', IOSTAT=ios, ERR=91) l4_in
     WRITE(1, FMT='(A25)', IOSTAT=ios, ERR=91) ch2_in

     CLOSE(1)


!**********************************************************
!   Opening the file again with POSITION='APPEND'         *
!**********************************************************

     OPEN(2, FILE='fxstio123.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &  STATUS='OLD', IOSTAT=ios, ERR=90, ACTION='READWRITE', POSITION='APPEND')

     INQUIRE(2, ACCESS=access, FORM=form, STREAM=stream, ASYNCH=asynch, &
    &        POS=pos, POSITION=position, SIZE=old_size)

     if ( access .ne. 'STREAM' ) error stop 20
     if ( stream .ne. 'YES' ) error stop 21
     if ( form .ne. 'FORMATTED' ) error stop 22
     if ( asynch .ne. 'NO' ) error stop 23
     if ( pos .ne. 108 ) error stop 24
     if ( position .ne. 'APPEND' ) error stop 25

!
!    Writing a record to the end of file
!
     WRITE(2, FMT='(A25)', IOSTAT=ios, ERR=91) ch2_in
     INQUIRE(2, POS=pos, SIZE=new_size)
     if ( pos .ne. 134 ) error stop 27
     if ( new_size .ne. old_size + 26) error stop 28

     READ(2, FMT='(A25)', IOSTAT=ios, ERR=92, POS=108) ch2_out
     if ( ch2_out .ne. ch2_in ) error stop 29
     INQUIRE(2, POS=pos)
     if ( pos .ne. 134 ) error stop 30

     CLOSE(2)


!**********************************************************
!   Opening the file again with POSITION='REWIND'         *
!**********************************************************

     OPEN(3, FILE='fxstio123.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &  STATUS='OLD', IOSTAT=ios, ERR=90, ACTION='READWRITE', POSITION='REWIND')

     INQUIRE(3, POS=pos, POSITION=position, SIZE=old_size)

     if ( pos .ne. 1 ) error stop 35
     if ( position .ne. 'REWIND' ) error stop 36

!
!    Writing a record to the beginning of file
!
     WRITE(3, FMT='(A25)', IOSTAT=ios, ERR=91) ch2_in
     INQUIRE(3, POS=pos, SIZE=new_size)
     if ( pos .ne. 27 ) error stop 37
     if ( new_size .ne. old_size ) error stop 38

     READ(3, FMT='(A25)', IOSTAT=ios, ERR=92, POS=1) ch2_out
     if ( ch2_out .ne. ch2_in ) error stop 39
     INQUIRE(3, POS=pos)
     if ( pos .ne. 27 ) error stop 40

     CLOSE(3)

!**********************************************************
!   Opening the file again with POSITION='ASIS'         *
!**********************************************************

     OPEN(4, FILE='fxstio123.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &  STATUS='OLD', IOSTAT=ios, ERR=90, ACTION='READWRITE', POSITION='ASIS')

     INQUIRE(4, POS=pos, POSITION=position, SIZE=old_size)

     if ( pos .ne. 1 ) error stop 45
     if ( position .ne. 'REWIND' ) error stop 46

!
!    Writing a record to the current position of file
!
     WRITE(4, FMT='(A25)', IOSTAT=ios, ERR=91) ch1_in
     INQUIRE(4, POS=pos, SIZE=new_size)
     if ( pos .ne. 27 ) error stop 47
     if ( new_size .ne. old_size ) error stop 48

     READ(4, FMT='(A25)', IOSTAT=ios, ERR=92, POS=1) ch1_out
     if ( ch1_out .ne. ch1_in ) error stop 49
     INQUIRE(4, POS=pos )
     if ( pos .ne. 27 ) error stop 50

     CLOSE(4, STATUS='DELETE')
     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92
93   print *, "Error while reading beyond the EOF: IOSTAT = ", ios
     error stop 93

   end program
