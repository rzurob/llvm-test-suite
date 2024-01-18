!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP:  fxstio124.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 2003
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ, INQUIRE
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test ENDFILE, REWIND and BACKSPACE
!*                               with formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/19/03   BC     Initial version
!*
!234567890124456789012445678901244567890124456789012445678901244567890

  program fxstio124

     implicit none
     integer    ios
     character*25 :: ch1_in='This is the first record.', ch1_out
     integer*4 	  :: i4_in /1244567/ , i4_out
     real*4    	  :: r4_in /-0.000001/, r4_out
     complex*8    :: x8_in /(-0.1E-39, 0.1E39)/, x8_out
     logical*4 	  :: l4_in /.false./, l4_out
     character*24 :: ch2_in='This is the last record.' , ch2_out

     integer    pos, old_size, new_size

!**********************************************************
!   Opening a new file and writing some data into it      *
!**********************************************************

     OPEN(1, FILE='fxstio124.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, ACTION='WRITE' )

     WRITE(1, FMT='(A25)', IOSTAT=ios, ERR=91) ch1_in
     WRITE(1, FMT='(I7)', IOSTAT=ios, ERR=91) i4_in
     WRITE(1, FMT='(F9.6)', IOSTAT=ios, ERR=91) r4_in
     WRITE(1, FMT='(2E15.7)',IOSTAT=ios,ERR=91) x8_in
     WRITE(1, FMT='(L5)', IOSTAT=ios, ERR=91) l4_in
     WRITE(1, FMT='(A24)', IOSTAT=ios, ERR=91) ch2_in

     CLOSE(1)

!***************************************************************
! Opening the file again to test ENDFILE, BACKSPACE and REWIND *
!***************************************************************

     OPEN(2, FILE='fxstio124.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &  STATUS='OLD', IOSTAT=ios, ERR=90)

     INQUIRE(2, SIZE=old_size)
     if ( old_size .ne. 106 ) error stop 15
!
!    Read the 5th record and put EOF after that
!
     READ(2, FMT='(L5)', IOSTAT=ios, ERR=92, POS=76) l4_out
     if ( l4_out .neqv. l4_in ) error stop 16
     INQUIRE(2, POS=pos)
       print *, pos
     if ( POS .ne. 82) error stop 17

     ENDFILE(2)
     INQUIRE(2, POS=pos, SIZE=new_size)
       print *, old_size, new_size, pos
     if ( new_size .ne. 81 ) error stop 18
     if ( POS .ne. 82) error stop 19

!
!    Testing BACKSPACE
!
     BACKSPACE(2)
     INQUIRE(2, POS=pos)
      print *, pos
     if ( POS .ne. 76) error stop 21

     BACKSPACE(2)
     INQUIRE(2, POS=pos)
      print *, pos
     if ( POS .ne. 45) error stop 22

     BACKSPACE(2)
     INQUIRE(2, POS=pos)
      print *, pos
     if ( POS .ne. 35) error stop 23

     BACKSPACE(2)
     INQUIRE(2, POS=pos)
      print *, pos
     if ( POS .ne. 27) error stop 24

     BACKSPACE(2)
     INQUIRE(2, POS=pos)
      print *, pos
     if ( POS .ne. 1) error stop 25

     BACKSPACE(2)
     INQUIRE(2, POS=pos)
      print *, pos
     if ( POS .ne. 1) error stop 26

!
!    Testing REWIND
!
     READ(2, FMT='(L5)', IOSTAT=ios, ERR=92, POS=76) l4_out
     INQUIRE(2, POS=pos)
     if ( POS .ne. 82) error stop 30
     REWIND(2)
     INQUIRE(2, POS=pos)
     if ( POS .ne. 1) error stop 31

     CLOSE(2, STATUS='DELETE')
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
