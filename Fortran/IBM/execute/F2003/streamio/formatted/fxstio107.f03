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
!*  DESCRIPTION                : Test formatted synchronous stream I/O
!*				 with allocatable objects
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/13/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  include 'check_array.inc'

  program fxstio107

     implicit none
     integer    ios
     integer, parameter      :: N = 10
     integer*2, allocatable  :: i2_in, i2_out
     integer*4, allocatable  :: i4_in, i4_out
     integer*8, allocatable  :: i8_in(:), i8_out(:)
     real*4, allocatable     :: r4_in, r4_out
     real*8, allocatable     :: r8_in(:,:)  , r8_out(:,:)
     real*16, allocatable    :: r16_in , r16_out
     complex*8, allocatable  ::  x8_in, x8_out
     complex*16, allocatable :: x16_in(:), x16_out(:)
     complex*32, allocatable :: x32_in, x32_out
     logical*2, allocatable  ::	l2_in, l2_out
     logical*4, allocatable  :: l4_in, l4_out
     logical*8, allocatable  :: l8_in(:,:), l8_out(:,:)
     character, allocatable  :: ch1_in, ch1_out
     character*15, allocatable :: ch15_in(:), ch15_out(:)
     byte, allocatable       :: b_in, b_out

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3

     include 'check_interface.inc'

!**********************************************************
!        Allocate Objects                                 *
!**********************************************************

     allocate(i2_in, i2_out)
     allocate(i4_in, i4_out)
     allocate(i8_in(N), i8_out(N))
     allocate(r4_in, r4_out)
     allocate(r8_in(N,N), r8_out(N,N))
     allocate(r16_in, r16_out)
     allocate(x8_in, x8_out)
     allocate(x16_in(N), x16_out(N))
     allocate(x32_in, x32_out)
     allocate(l2_in, l2_out)
     allocate(l4_in, l4_out)
     allocate( l8_in(N,N), l8_out(N,N) )
     allocate(ch1_in, ch1_out)
     allocate(ch15_in(N), ch15_out(N))
     allocate(b_in, b_out)

!**********************************************************
!        Initialization of variables                      *
!**********************************************************

     i2_in = 1234
     i4_in = -20000000
     i8_in = huge(i8_in)
     r4_in = -0.000001
     r8_in = huge(r8_in)
     r16_in = huge(r16_in) / 2.
     x8_in = (huge(r4_in), -1.0 * huge(r4_in))
     x16_in = (0.0D0, -0.0D0)
     x32_in = (-0.1Q-309, 0.1Q309)
     l2_in = .true.
     l4_in = .false.
     l8_in = .true.
     ch1_in = "A"
     ch15_in = "New Baby Girl! "
     b_in = b'01010111'


!**********************************************************
!        Writing and Reading the file                     *
!**********************************************************

     OPEN(1, FILE='fxstio107.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='NEW', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(I5, I10, 10I20)', IOSTAT=ios, ERR=91) i2_in, i4_in, i8_in
     WRITE(1, FMT='(F9.6,100D25.17,Q40.32)', IOSTAT=ios, ERR=91) &
    &      r4_in, r8_in, r16_in
     WRITE(1, FMT='(2E15.7,20D25.17,2Q40.32)', IOSTAT=ios, ERR=91) &
    &      x8_in, x16_in, x32_in
     WRITE(1, FMT='(2L5, 100L5)', IOSTAT=ios, ERR=91) l2_in, l4_in, l8_in
     WRITE(1, FMT='(A1,10A16)', IOSTAT=ios, ERR=91) ch1_in, ch15_in
     WRITE(1, FMT='(B8)', IOSTAT=ios, ERR=91) b_in

     REWIND(1, IOSTAT=ios, ERR=93)

     READ(1, FMT='(I5, I10, 10I20)', IOSTAT=ios, ERR=92) i2_out, i4_out, i8_out
     READ(1, FMT='(F9.6,100D25.17,Q40.32)', IOSTAT=ios, ERR=92) &
    &     r4_out, r8_out, r16_out
     READ(1, FMT='(2E15.7,20D25.17,2Q40.32)', IOSTAT=ios, ERR=92) &
    &      x8_out, x16_out, x32_out
     READ(1, FMT='(2L5, 100L5)', IOSTAT=ios, ERR=92) l2_out, l4_out, l8_out
     READ(1, FMT='(A1,10A16)', IOSTAT=ios, ERR=92) ch1_out, ch15_out
     READ(1, FMT='(B8)', IOSTAT=ios, ERR=92) b_out

     CLOSE(1, STATUS='DELETE')

!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( i2_in .ne. i2_out ) error stop 10
     if ( i4_in .ne. i4_out ) error stop 11
     if ( .not. Array_Check (i8_in, i8_out)) error stop 12

     if ( .not. precision_R4(r4_in, r4_out)) error stop 13
     if ( .not. Array_Check (r8_in, r8_out) ) error stop 14
     if ( .not. precision_R6(r16_in, r16_out)) error stop 15

     if ( .not. precision_x8(x8_in,  x8_out )) error stop 16
     if ( .not. Array_Check (x16_in, x16_out) ) error stop 17
     if ( .not. precision_x3(x32_in, x32_out)) error stop 18

     if ( l2_in .neqv. l2_out) error stop 19
     if ( l4_in .neqv. l4_out) error stop 20
     if ( .not. Array_Check (l8_in, l8_out)) error stop 21

     if ( ch1_in .ne. ch1_out ) error stop 22
     if ( .not. Array_Check (ch15_in, ch15_out) ) error stop 23

     if ( b_in .ne. b_out) error stop 24

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92
93   print *, "Error while rewinding the file: IOSTAT = ", ios
     error stop 93

   end program
