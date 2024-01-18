!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: cp $TR_SRC/check_array.inc .; cp $TR_SRC/check_interface.inc .
! %COMPOPTS:
! %GROUP:  fxstio103.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f check_array.inc check_interface.inc
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 2003
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ, BACKSPACE
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test array sections, assumed-shap array and
!*				 deffered-shape array, allocatable array and
!*                               pointer array with formatted synchronous
!*                               stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/10/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  include 'check_array.inc'

  program fxstio103

     implicit none
     integer    i, j, k, l, ios
     integer, parameter :: N = 10
     integer*8 	i8_in(N), i8_out(N)
     real*8    	r8_in(N), r8_out(N)
     real*16    r16_in(N) , r16_out(N)
     complex*16 x16_in(N,N), x16_out(N,N)
     logical*4, allocatable :: l4_in(:,:), l4_out(:,:)
     character*15, pointer :: ch15_pin(:), ch15_pout(:)
     character*15, target  :: ch15_tin(N), ch15_tout(N)

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3

     include 'check_interface.inc'

     interface
        function write_it( arg1, arg2 ) result(ios)
            real*16    arg1(:)
            complex*16 arg2(:,:)
        end function write_it

        function read_it( arg1, arg2 ) result(ios)
            real*16    arg1(:)
            complex*16 arg2(:,:)
        end function read_it
     end interface

!**********************************************************
!        Initialization of arrays                         *
!**********************************************************

     i8_in = -20000000
     r8_in = -0.00000000000001
     r16_in = huge(r16_in) / 2.
     x16_in = (huge(r8_in), -1.0 * huge(r8_in))
     allocate( l4_in(-1:N-2,2:N+1), l4_out(N,N) )
     l4_in = .true.
     ch15_tin = "New Baby Girl!!"

     ch15_pin => ch15_tin
     ch15_pout => ch15_tout

!**********************************************************
!       Writing and Reading the file                      *
!**********************************************************

     OPEN(1, FILE='fxstio103.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

!
!   Testing array sections
!
     WRITE(1, FMT='(10I10)', IOSTAT=ios, ERR=91) i8_in(:N/2), i8_in(N/2+1:)
     WRITE(1, FMT='(10D25.17)', IOSTAT=ios, ERR=91) &
    &     r8_in((/1,3,5,7,9/)), r8_in(2:N:2)

     BACKSPACE(1, IOSTAT=ios, ERR=93)
     BACKSPACE(1, IOSTAT=ios, ERR=93)

     READ(1, FMT='(10I10)', IOSTAT=ios, ERR=92) &
   &      i8_out(:N/3), i8_out(N/3+1:2*N/3), i8_out(2*N/3+1:)

     READ(1, FMT='(10D25.17)', IOSTAT=ios, ERR=92) &
   &     r8_out(:N:2), r8_out((/2,4,6,8,10/))
!
!   Testing Assumed-shape arrays
!
     ios = write_it( r16_in, x16_in )
     if ( ios .ne. 0 ) goto 91

     BACKSPACE(1, IOSTAT=ios, ERR=93)
     BACKSPACE(1, IOSTAT=ios, ERR=93)

     ios = read_it( r16_out, x16_out )
     if ( ios .ne. 0 ) goto 91

!
!   Testing Deffered-shape and allocatable arrays
!
     WRITE(1, FMT='(100L5)', IOSTAT=ios, ERR=91) &
    &     ((l4_in(i,j),i=-1,N-2),j=2,N+1)

     BACKSPACE(1, IOSTAT=ios, ERR=93)

     READ(1, FMT='(100L5)', IOSTAT=ios, ERR=92) l4_out

!
!   Testing array pointer
!
     WRITE(1, FMT='(10A16)', IOSTAT=ios, ERR=91) ch15_pin

     BACKSPACE(1, IOSTAT=ios, ERR=93)

     READ(1, FMT='(10A16)', IOSTAT=ios, ERR=92) ch15_pout


!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( .not. Array_Check (i8_in, i8_out) ) error stop 10

     if ( .not. Array_Check (r8_in, r8_out) ) error stop 11

     if ( .not. Array_Check (r16_in, r16_out) ) error stop 12
     if ( .not. Array_Check (x16_in, x16_out) ) error stop 13

     if ( .not. Array_Check (l4_in, l4_out) ) error stop 14

     if ( .not. Array_Check (ch15_tin, ch15_tout)) error stop 15
     if ( .not. Array_Check (ch15_pin, ch15_pout)) error stop 16

     CLOSE(1, STATUS='DELETE')

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



   function write_it( arg1, arg2 ) result(ios)
       real*16    arg1(:)
       complex*16 arg2(:,:)
       integer ios

       WRITE(1, FMT='(10Q40.32)', IOSTAT=ios) arg1
       WRITE(1, FMT='(200D25.17)', IOSTAT=ios) arg2

       return
   end function write_it

   function read_it( arg1, arg2 ) result(ios)
       real*16    arg1(:)
       complex*16 arg2(:,:)
       integer ios

       READ(1, FMT='(10Q40.32)', IOSTAT=ios) arg1
       READ(1, FMT='(200D25.17)', IOSTAT=ios) arg2

       return
   end function read_it
