!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP:  fxstio126.f
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
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ, INQUIRE
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test SIZE, POS, ACCESS, STREAM specifiers 
!*                               in INQUIRE stmt with formatted I/O. 
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments: 
!*  03/20/03   BC     Initial version 
!* 
!234567890126456789012645678901264567890126456789012645678901264567890 


  program fxstio126 

     implicit none
     integer    ios
     character*25 :: ch1_in='This is the first record.'
     integer*4 	  :: i4_in /1264567/ 
     real*4    	  :: r4_in /-0.000001/
     complex*8    :: x8_in /(-0.1Q-39, 0.1Q39)/
     logical*4 	  :: l4_in /.false./
     character*25 :: ch2_in='This is the last record.'

     character*15  access, stream
     integer       pos, size

!********************************************************** 
!    Create a direct file and check the SIZE              *
!********************************************************** 

     OPEN(1, FILE='fxstio126.dat', FORM='FORMATTED', ACCESS='DIRECT', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, RECL=30 )

     INQUIRE(1, SIZE=size, STREAM=stream, ACCESS=access) 

     if ( access .ne. 'DIRECT' ) error stop 20
     if ( stream .ne. 'NO' ) error stop 21
     if ( size .ne. 0 ) error stop 22

     WRITE(1, FMT='(A25)', IOSTAT=ios, ERR=91, REC=1) ch1_in
     WRITE(1, FMT='(I7)', IOSTAT=ios, ERR=91, REC=2) i4_in
     WRITE(1, FMT='(F9.6)', IOSTAT=ios, ERR=91, REC=3) r4_in
     WRITE(1, FMT='(2E15.7)',IOSTAT=ios,ERR=91, REC=4) x8_in
     WRITE(1, FMT='(L5)', IOSTAT=ios, ERR=91, REC=5) l4_in
     WRITE(1, FMT='(A)', IOSTAT=ios, ERR=91, REC=6) ch2_in

     INQUIRE(1, SIZE=size) 
     if ( size .ne. 6*30 ) error stop 23

     CLOSE(1, STATUS='DELETE')


!********************************************************** 
!    Create a sequential file and check the SIZE          *
!********************************************************** 

     OPEN(1, FILE='fxstio126.dat', FORM='FORMATTED', ACCESS='SEQUENTIAL', &
    &     STATUS='NEW', IOSTAT=ios, ERR=90 )

     INQUIRE(1, SIZE=size, STREAM=stream, ACCESS=access) 

     if ( access .ne. 'SEQUENTIAL' ) error stop 30
     if ( stream .ne. 'NO' ) error stop 31
     if ( size .ne. 0 ) error stop 32

     WRITE(1, FMT='(A25)', IOSTAT=ios, ERR=91) ch1_in
     WRITE(1, FMT='(I7)', IOSTAT=ios, ERR=91) i4_in
     WRITE(1, FMT='(F9.6)', IOSTAT=ios, ERR=91) r4_in
     WRITE(1, FMT='(2E15.7)',IOSTAT=ios,ERR=91) x8_in
     WRITE(1, FMT='(L5)', IOSTAT=ios, ERR=91) l4_in
     WRITE(1, FMT='(A)', IOSTAT=ios, ERR=91) ch2_in

     INQUIRE(1, SIZE=size) 
     if ( size .ne. 107 ) error stop 33

     CLOSE(1, STATUS='DELETE')


!***************************************************************** 
!   Create a Stream file and check the SIZE, POS, STREAM, ACCESS * 
!***************************************************************** 

     OPEN(1, FILE='fxstio126.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &  STATUS='REPLACE', IOSTAT=ios, ERR=90)

     INQUIRE(1, ACCESS=access, STREAM=stream, POS=pos, SIZE=size)

     if ( access .ne. 'STREAM' ) error stop 40
     if ( stream .ne. 'YES' ) error stop 41
     if ( pos .ne. 1 ) error stop 44 
     if ( size .ne. 0 ) error stop 45 

     WRITE(1, FMT='(A25)', IOSTAT=ios, ERR=91, POS=1) ch1_in
     INQUIRE(1, SIZE=size, POS=pos)
     if ( size .ne. 26) error stop 50
     if ( pos  .ne. 27) error stop 51
 
     WRITE(1, FMT='(F9.6)', IOSTAT=ios, ERR=91, POS=60) r4_in
     INQUIRE(1, SIZE=size, POS=pos)
     if ( size .ne. 69) error stop 52
     if ( pos  .ne. 70) error stop 53

     WRITE(1, FMT='(I7)', IOSTAT=ios, ERR=91, POS=30) i4_in
     INQUIRE(1, SIZE=size, POS=pos)
     if ( size .ne. 69) error stop 54
     if ( pos  .ne. 38) error stop 55

     WRITE(1, FMT='(L5)', IOSTAT=ios, ERR=91, POS=120) l4_in
     INQUIRE(1, SIZE=size, POS=pos)
     if ( size .ne. 125) error stop 56
     if ( pos  .ne. 126) error stop 57

     WRITE(1, FMT='(2E15.7)',IOSTAT=ios,ERR=91, POS=80) x8_in
     INQUIRE(1, SIZE=size, POS=pos)
     if ( size .ne. 125) error stop 58
     if ( pos  .ne. 111) error stop 59

     WRITE(1, FMT='(A)', IOSTAT=ios, ERR=91, POS=110) ch2_in
     INQUIRE(1, SIZE=size, POS=pos)
     if ( size .ne. 135) error stop 60
     if ( pos  .ne. 136) error stop 61


     CLOSE(1, STATUS='DELETE')
     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90 
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91 
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92 

   end program
