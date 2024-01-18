!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fxstio111.dat
! %COMPOPTS:
! %GROUP:  fxstio111.f
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
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test formatted synchronous stream I/O
!*				 with an old file created by sequential access
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/13/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


   module read_write

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

     contains
	subroutine write_seq(unit)
	   integer unit

           WRITE(unit, FMT='(I5, I10, I20)', IOSTAT=ios, ERR=91) &
          &      i2_in, i4_in, i8_in
           WRITE(unit, FMT='(F9.6,D25.17,Q40.32)', IOSTAT=ios, ERR=91) &
          &      r4_in, r8_in, r16_in
           WRITE(unit, FMT='(D25.17)', IOSTAT=ios, ERR=91) d8_in
           WRITE(unit, FMT='(2E15.7,2D25.17,2Q40.32)', IOSTAT=ios, ERR=91) &
          &      x8_in, x16_in, x32_in
           WRITE(unit, FMT='(3L5)', IOSTAT=ios, ERR=91) l2_in, l4_in, l8_in
           WRITE(unit, FMT='(A1,A16)', IOSTAT=ios, ERR=91) ch1_in, ch15_in
           WRITE(unit, FMT='(B8)', IOSTAT=ios, ERR=91) b_in

           return
91         print *, "Error while writing to the file: IOSTAT = ", ios
           error stop 91
        end subroutine

	subroutine read_stream(unit)
	   integer unit

           READ(unit, FMT='(I5, I10, I20)', IOSTAT=ios, ERR=92) &
          &     i2_out, i4_out, i8_out
           READ(unit, FMT='(F9.6,D25.17,Q40.32)', IOSTAT=ios, ERR=92) &
          &     r4_out, r8_out, r16_out
           READ(unit, FMT='(D25.17)', IOSTAT=ios, ERR=92) d8_out
           READ(unit, FMT='(2E15.7,2D25.17,2Q40.32)', IOSTAT=ios, ERR=92) &
          &     x8_out, x16_out, x32_out
           READ(unit, FMT='(3L5)', IOSTAT=ios, ERR=92) l2_out, l4_out, l8_out
           READ(unit, FMT='(A1,A16)', IOSTAT=ios, ERR=92) ch1_out, ch15_out
           READ(unit, FMT='(B8)', IOSTAT=ios, ERR=92) b_out

           return
92         print *, "Error while reading from the file: IOSTAT = ", ios
           error stop 92
        end subroutine
  end module

  program fxstio111

     use read_write
     implicit none

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

     OPEN(1, FILE='fxstio111.dat', FORM='FORMATTED', ACCESS='SEQUENTIAL', &
    &     STATUS='NEW', IOSTAT=ios, ERR=90)

     call write_seq(1)

     CLOSE(1)

     OPEN(2, FILE='fxstio111.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='OLD', IOSTAT=ios, ERR=90)

     call read_stream(2)


!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( i2_in .ne. i2_out ) error stop 10
     if ( i4_in .ne. i4_out ) error stop 11
     if ( i8_in .ne. i8_out ) error stop 12

     if ( .not. precision_R4(r4_in, r4_out)) error stop 13
     if ( .not. precision_R8(r8_in, r8_out)) error stop 14
     if ( .not. precision_R6(r16_in, r16_out)) error stop 15

     if ( .not. precision_R8(d8_in, d8_out) ) error stop 12

     if ( .not. precision_x8(x8_in,  x8_out )) error stop 16
     if ( .not. precision_x6(x16_in, x16_out)) error stop 17
     if ( .not. precision_x3(x32_in, x32_out)) error stop 18

     if ( l2_in .neqv. l2_out) error stop 19
     if ( l4_in .neqv. l4_out) error stop 20
     if ( l8_in .neqv. l8_out) error stop 21

     if ( ch1_in .ne. ch1_out ) error stop 22
     if ( ch15_in .ne. ch15_out ) error stop 23

     if ( b_in .ne. b_out) error stop 24

     CLOSE(2, STATUS='DELETE')
     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90

   end program
