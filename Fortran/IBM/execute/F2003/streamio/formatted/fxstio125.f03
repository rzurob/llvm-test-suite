!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 2003
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, INQUIRE
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test SIZE= specifier in INQUIRE & READ stmts
!*                               with formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/20/03   BC     Initial version
!*
!234567890125456789012545678901254567890125456789012545678901254567890

  program fxstio125

     implicit none
     integer      :: i, ios
     integer, parameter :: N=6
     character*25 :: ch1_in='This is the first record.', ch1_out
     integer*4 	  :: i4_in /1254567/ , i4_out
     integer*8    :: i8_in /-12234567890_8/, i8_out
     real*4    	  :: r4_in /-0.000001/, r4_out
     real*8       :: r8_in /-0.1D-309/, r8_out
     complex*8    :: x8_in /(-0.1E-39, 0.1E39)/, x8_out
     complex*16   :: x16_in /(huge(r8_in), -1.0 * huge(r8_in))/, x16_out
     complex*32   :: x32_in /(-0.1Q-309, 0.1Q309)/, x32_out
     logical*4 	  :: l4_in /.false./, l4_out
     logical*8    :: l8_in /.true./, l8_out
     byte         :: b_in /b'01010111'/, b_out
     character    :: ch_in(N) /'O','R','A','N','G','E'/, ch_out(N)
     character*24 :: ch2_in='This is the last record.' , ch2_out

     integer    pos, size, old_size, new_size

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3

!**********************************************************
!   Opening a new file and writing some data into it      *
!   while checking the file size                          *
!**********************************************************

     OPEN(1, FILE='fxstio125.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, ACTION='WRITE' )

     INQUIRE(1, SIZE=size)
     if ( size .ne. 0 ) error stop 20

     WRITE(1, FMT='(A25)', IOSTAT=ios, ERR=91) ch1_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (25+1) ) error stop 21

     old_size = new_size
     WRITE(1, FMT='(I7)', IOSTAT=ios, ERR=91) i4_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+7+1) ) error stop 22

     old_size = new_size
     WRITE(1, FMT='(I12)', IOSTAT=ios, ERR=91) i8_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+12+1) ) error stop 23

     old_size = new_size
     WRITE(1, FMT='(F9.6)', IOSTAT=ios, ERR=91) r4_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+9+1) ) error stop 24

     old_size = new_size
     WRITE(1, FMT='(D25.17)', IOSTAT=ios, ERR=91) r8_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+25+1) ) error stop 25

     old_size = new_size
     WRITE(1, FMT='(2E15.7)',IOSTAT=ios,ERR=91) x8_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+30+1) ) error stop 26

     old_size = new_size
     WRITE(1, FMT='(2D25.17)',IOSTAT=ios,ERR=91) x16_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+50+1) ) error stop 27

     old_size = new_size
     WRITE(1, FMT='(2Q25.17)',IOSTAT=ios,ERR=91) x32_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+50+1) ) error stop 28

     old_size = new_size
     WRITE(1, FMT='(L2)', IOSTAT=ios, ERR=91) l4_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+2+1) ) error stop 29

     old_size = new_size
     WRITE(1, FMT='(L2)', IOSTAT=ios, ERR=91) l8_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+2+1) ) error stop 30

     old_size = new_size
     WRITE(1, FMT='(B8)', IOSTAT=ios, ERR=91) b_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+8+1) ) error stop 31

     old_size = new_size
     INQUIRE(1, POS=pos)
     pos = pos + 10 		! Leave a hole in the file
     do i = 1, N
        WRITE(1, FMT='(A1)', IOSTAT=ios, ERR=91, POS=(pos+i-1)) ch_in(i)
     enddo
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+10+N+1) ) error stop 32

     old_size = new_size
     WRITE(1, FMT='(A)', IOSTAT=ios, ERR=91) ch2_in
     INQUIRE(1, SIZE=new_size)
     if ( new_size .ne. (old_size+24+1) ) error stop 33

     CLOSE(1)

!**********************************************************
!   Opening and read the same file again                  *
!   while checking the file SIZE= specifier               *
!**********************************************************

     OPEN(1, FILE='fxstio125.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='OLD', IOSTAT=ios, ERR=90, ACTION='READ' )

     READ(1, FMT='(A25)', SIZE=size, ADVANCE='no', IOSTAT=ios, ERR=92) ch1_out
     if ( size .ne. 25 ) error stop 41

     READ(1, FMT='(/I7)', SIZE=size, ADVANCE='no', IOSTAT=ios, ERR=92) i4_out
     if ( size .ne. 7 ) error stop 42

!
!  Passing the End_of_record
!
     READ(1, FMT='(/I15)', SIZE=size, ADVANCE='no', IOSTAT=ios, ERR=92) i8_out
     if ( size .ne. 12 ) error stop 43

     READ(1, FMT='(F12.6)', SIZE=size, ADVANCE='no', IOSTAT=ios, ERR=92) r4_out
     if ( size .ne. 9 ) error stop 44

     READ(1, FMT='(D25.17)', SIZE=size, ADVANCE='no', IOSTAT=ios, ERR=92) r8_out
     if ( size .ne. 25 ) error stop 45

     READ(1, FMT='(/2E15.7)', SIZE=size, ADVANCE='no', IOSTAT=ios,ERR=92) x8_out
     if ( size .ne. 30 ) error stop 46

     READ(1, FMT='(/2D25.17)', SIZE=size, ADVANCE='no', IOSTAT=ios,ERR=92) x16_out
     if ( size .ne. 50 ) error stop 47

     READ(1, FMT='(/2Q25.17)', SIZE=size, ADVANCE='no', IOSTAT=ios,ERR=92) x32_out
     if ( size .ne. 50 ) error stop 48

     READ(1, FMT='(/L2)', SIZE=size, ADVANCE='no', IOSTAT=ios, ERR=92) l4_out
     if ( size .ne. 2 ) error stop 49

!
!  Passing the End_of_record
!
     READ(1, FMT='(/L8)', SIZE=size, ADVANCE='no', IOSTAT=ios, ERR=92) l8_out
      if ( size .ne. 2 ) error stop 50

     READ(1, FMT='(B8)', SIZE=size, ADVANCE='no', IOSTAT=ios, ERR=92) b_out
     if ( size .ne. 8 ) error stop 51

     do i = 1, N
       READ(1, FMT='(A1)', SIZE=size, ADVANCE='no', IOSTAT=ios, ERR=92, &
    &                                                 POS=241+i) ch_out(i)
       if ( size .ne. 1 ) call zzrc(51 + i)
     enddo

     READ(1, FMT='(/A)', SIZE=size, ADVANCE='no', IOSTAT=ios, ERR=92) ch2_out
     if ( size .ne. 24 ) error stop 63

!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( ch1_in .ne. ch1_out ) error stop 71

     if ( i4_in .ne. i4_out ) error stop 72
     if ( i8_in .ne. i8_out ) error stop 73

     if ( .not. precision_R4(r4_in, r4_out)) error stop 74
     if ( .not. precision_R8(r8_in, r8_out)) error stop 75

     if ( .not. precision_x8(x8_in,  x8_out )) error stop 76
     if ( .not. precision_x6(x16_in, x16_out)) error stop 77
     if ( .not. precision_x3(x32_in, x32_out)) error stop 78

     if ( l4_in .neqv. l4_out) error stop 79
     if ( l8_in .neqv. l8_out) error stop 80

     if ( b_in .ne. b_out) error stop 81

     do i = 1, N
        if ( ch_in(i) .ne. ch_out(i) ) call zzrc(81+i)
     enddo

     if ( ch2_in .ne. ch2_out ) call zzrc(82+N)

     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading the the file: IOSTAT = ", ios
     error stop 92

   end program
