!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP:  fxstio122.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f read_write.mod
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
!*  DESCRIPTION                : Test formatted synchronous stream I/O
!*				 with an old record file. Testing initial
!*                               point, Terminal Point, end-of-record etc.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/18/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


   module read_write

     implicit none
     integer    ios
     integer*2 	i2_in, i2_out
     integer*4 	i4_in
     integer*8 	i8_in
     real*4    	r4_in
     real*8     r8_in, r8_out
     real*16    r16_in
     double precision  d8_in
     complex*8  x8_in
     complex*16 x16_in
     complex*32 x32_in
     logical*2 	l2_in
     logical*4 	l4_in
     logical*8 	l8_in
     character    ch1_in
     character*15 ch15_in
     byte       b_in, b_out

     contains
	subroutine write_stream(unit)
	   integer unit

           OPEN(unit, FILE='fxstio122.dat', FORM='FORMATTED', ACCESS='STREAM', &
          &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

           WRITE(unit, FMT='(I5, I10, I20)', IOSTAT=ios, ERR=91) &
          &      i2_in, i4_in, i8_in
           WRITE(unit, FMT='(F9.6,D25.17,Q40.32)', IOSTAT=ios, ERR=91) &
          &      r4_in, r8_in, r16_in
           WRITE(unit, FMT='(D25.17)', IOSTAT=ios, ERR=91) d8_in
           WRITE(unit, FMT='(2E15.7,2D25.17,2Q40.32)',IOSTAT=ios,ERR=91) &
          &      x8_in, x16_in, x32_in
           WRITE(unit, FMT='(3L5)', IOSTAT=ios, ERR=91) &
          &      l2_in, l4_in, l8_in
           WRITE(unit, FMT='(A1,A16)', IOSTAT=ios, ERR=91) &
          &      ch1_in, ch15_in
           WRITE(unit, FMT='(B8)', IOSTAT=ios, ERR=91) b_in

           CLOSE(unit)

           return
90         print *, "Error while openning the file: IOSTAT = ", ios
           error stop 90
91         print *, "Error while writing to the file: IOSTAT = ", ios
           error stop 91
        end subroutine

  end module

  program fxstio122

     use read_write
     implicit none
     character*15  access, form, asynch, stream
     integer    pos, size, iolength
     character  c_out(10)

     logical precision_R4, precision_R8, precision_R6

!**********************************************************
!        Initialization of variables                      *
!**********************************************************

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
     ch15_in = "New Baby Girl! "
     b_in = b'01110111'


!**********************************************************
!        Writing to the file using direct access          *
!**********************************************************

     call write_stream(1)

!**********************************************************
!   Opening and Reading the file with stream access       *
!**********************************************************

     OPEN(1, FILE='fxstio122.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='OLD', IOSTAT=ios, ERR=90, ACTION='READWRITE')

     INQUIRE(1, ACCESS=access, FORM=form, STREAM=stream, ASYNCH=asynch, POS=pos)

     if ( access .ne. 'STREAM' ) error stop 20
     if ( stream .ne. 'YES' ) error stop 21
     if ( form .ne. 'FORMATTED' ) error stop 22
     if ( asynch .ne. 'NO' ) error stop 23
     if ( pos .ne. 1 ) error stop 24

!
!    Reading record and checking the file position
!
     READ(1, FMT='(I5)', IOSTAT=ios, ERR=92, POS=1) i2_out
     if ( i2_out .ne. i2_in ) error stop 26
     INQUIRE(1, POS=pos)
     if ( pos .ne. 37 ) error stop 27

     READ(1, FMT='(D25.17)', IOSTAT=ios, ERR=92, POS=48) r8_out
     if ( .not. precision_R8(r8_in, r8_out)) error stop 28
     INQUIRE(1, POS=pos)
     if ( pos .ne. 112 ) error stop 29

!
!    Checking the end-of-record condition
!
     READ(1, FMT='(A2)', IOSTAT=ios, EOR=50, POS=36, ADVANCE='NO') c_out(1:2)
     error stop 31
50   if ( ios .ne. -4 ) error stop 32
     INQUIRE(1, POS=pos)
     if ( pos .ne. 37 ) error stop 33

!
!    Checking the end-of-file condition
!
     READ(1, FMT='(A5)', IOSTAT=ios, END=55, POS=342) c_out(1:5)
     error stop 36
55   if ( ios .ne. -1 ) error stop 37
     INQUIRE(1, POS=pos)
     if ( pos .ne. 342 ) error stop 38

!
!    Trying to write beyond the end-of-file
!
     INQUIRE(1, SIZE=size)
     if ( size .ne. 341 ) error stop 40

     WRITE(1, FMT='(A30)', IOSTAT=ios, ERR=93, POS=345) &
    & "This will extend the file size"

     INQUIRE(1, SIZE=size)
     if ( size .ne. (341 + 34) ) error stop 41

!
!  Extending the file size with empty output list
!
     WRITE(1, FMT='(A30)', IOSTAT=ios, ERR=93, POS=380)
     INQUIRE(1, SIZE=size)
     if ( size .ne. (380) ) error stop 42  ! File should extend when ADVANCE=yes

     WRITE(1, FMT='(A30)', IOSTAT=ios, ERR=93, POS=400, ADVANCE='no')
     INQUIRE(1, SIZE=size)
     if ( size .ne. (380) ) error stop 43  ! No file extention when ADVANCE=no

     CLOSE(1, STATUS='DELETE')
     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92
93   print *, "Error while reading beyond the EOF: IOSTAT = ", ios
     error stop 93

   end program
