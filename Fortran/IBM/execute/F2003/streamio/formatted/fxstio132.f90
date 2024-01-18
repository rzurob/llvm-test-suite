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
!*  DESCRIPTION                : Reading a record file (created by sequential
!*                               access) using formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/21/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  program fxstio132

     implicit none
     integer    ios
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
!        Writing and Reading the file                     *
!**********************************************************

     OPEN(1, FILE='fxstio132.dat', FORM='FORMATTED', ACCESS='SEQUENTIAL', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     call write_sequential(1)

     CLOSE(1)

     OPEN(2, FILE='fxstio132.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='OLD', IOSTAT=ios, ERR=90)

     call read_stream(2)


!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( i2_in .ne. i2_out ) error stop 20
     if ( i4_in .ne. i4_out ) error stop 21
     if ( i8_in .ne. i8_out ) error stop 22

     if ( .not. precision_R4(r4_in, r4_out)) error stop 23
     if ( .not. precision_R8(r8_in, r8_out)) error stop 24
     if ( .not. precision_R6(r16_in, r16_out)) error stop 25

     if ( .not. precision_R8(d8_in, d8_out) ) error stop 22

     if ( .not. precision_x8(x8_in,  x8_out )) error stop 26
     if ( .not. precision_x6(x16_in, x16_out)) error stop 27
     if ( .not. precision_x3(x32_in, x32_out)) error stop 28

     if ( l2_in .neqv. l2_out) error stop 29
     if ( l4_in .neqv. l4_out) error stop 30
     if ( l8_in .neqv. l8_out) error stop 31

     if ( ch1_in .ne. ch1_out ) error stop 32
     if ( ch15_in .ne. ch15_out ) error stop 33

     if ( b_in .ne. b_out) error stop 34

!**********************************************************
!    Checking the end-of-file and end-of-file             *
!**********************************************************

     READ(2, FMT='(A1)', IOSTAT=ios, END=51, POS=342) ch1_out
     error stop 50
51   if ( ios .ne. -1 ) error stop 51		! End-of-file

     READ(2, FMT='(A15)', IOSTAT=ios, EOR=53, POS=332, ADVANCE='no') ch15_out
     error stop 52
53   if ( ios .ne. -4 ) error stop 53		! Emd-of-record

     READ(2, FMT='(A15)', IOSTAT=ios, EOR=55, POS=320, ADVANCE='no') ch15_out
     error stop 54
55   if ( ios .ne. -4 ) error stop 55		! Emd-of-record

     CLOSE(2, STATUS='DELETE')
     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90

     contains
	subroutine write_sequential(unit)
	   integer unit

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

           return
91         print *, "Error while writing to the file: IOSTAT = ", ios
           error stop 91
        end subroutine

	subroutine read_stream(unit)
	   integer unit

           READ(unit, FMT='(I5)', IOSTAT=ios,ERR=92,POS=1,ADVANCE='no') i2_out
           READ(unit, FMT='(I10)', IOSTAT=ios, ERR=92, ADVANCE='no') i4_out
           READ(unit, FMT='(I20)', IOSTAT=ios, ERR=92) i8_out

           READ(unit, FMT='(F9.6)', IOSTAT=ios,ERR=92,POS=37,ADVANCE='no') r4_out
           READ(unit, FMT='(D25.17)', IOSTAT=ios, ERR=92, ADVANCE='no') r8_out
           READ(unit, FMT='(Q40.32)', IOSTAT=ios, ERR=92) r16_out

           READ(unit, FMT='(D25.17)', IOSTAT=ios, ERR=92, POS=112) d8_out

           READ(unit, FMT='(2E15.7)',IOSTAT=ios,ERR=92,POS=138) x8_out
           READ(unit, FMT='(2D25.17)',IOSTAT=ios,ERR=92,POS=168) x16_out
           READ(unit, FMT='(2Q40.32)',IOSTAT=ios,ERR=92,POS=218) x32_out

           READ(unit, FMT='(L1)', IOSTAT=ios, ERR=92, POS=303) l2_out
           READ(unit, FMT='(L1)', IOSTAT=ios, ERR=92, POS=308) l4_out
           READ(unit, FMT='(L1)', IOSTAT=ios, ERR=92, POS=313) l8_out

           READ(unit, FMT='(A1)', IOSTAT=ios, ERR=92, POS=315) ch1_out
           READ(unit, FMT='(A15)', IOSTAT=ios, ERR=92, POS=317) ch15_out

           READ(unit, FMT='(B8)', IOSTAT=ios, ERR=92, POS=333) b_out

           return
92         print *, "Error while reading from the file: IOSTAT = ", ios
           error stop 92
        end subroutine
   end program
