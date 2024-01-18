!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: cp $TR_SRC/check_array.inc .; cp $TR_SRC/check_interface.inc .
! %COMPOPTS:
! %GROUP:  fxstio141.f
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
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test A & H edit descriptors with
!*				 formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/24/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  include 'check_array.inc'

  program fxstio141

     implicit none
     integer    i, j, k, l, ios
     integer, parameter    :: N = 10
     character             :: ch1_in, ch1_out
     character*25          :: ch25_in, ch25_out
     character             :: ch1_arr_in(N,N), ch1_arr_out(N,N)
     character*25          :: ch25_arr_in(N), ch25_arr_out(N)
     character, pointer    :: ch1_pin, ch1_pout
     character, target     :: ch1_tin, ch1_tout
     character*25, pointer :: ch25_arr_pin(:), ch25_arr_pout(:)
     character*25, target  :: ch25_arr_tin(N), ch25_arr_tout(N)
     character, allocatable    :: ch1_all_in, ch1_all_out
     character*25, allocatable :: ch25_all_in(:,:), ch25_all_out(:,:)
     character, parameter      :: ch1_par_in = 'z'
     character                 :: ch1_par_out
     character*25, parameter   :: ch25_par_in = "XLFortran V8.1.1 for AIX."
     character*25              :: ch25_par_out
     character*25              :: ch25_ltrl_out1
     character*25              :: ch25_ltrl_out2

     include 'check_interface.inc'

!**********************************************************
!       Allocation, Association & Initialization          *
!**********************************************************

     allocate(ch1_all_in, ch1_all_out)
     allocate(ch25_all_in(N,N), ch25_all_out(N,N))

     ch1_pin => ch1_tin
     ch1_pout => ch1_tout
     ch25_arr_pin => ch25_arr_tin
     ch25_arr_pout => ch25_arr_tout

     ch1_in = "A"
     ch25_in = "Beautiful flower pot"
     ch1_arr_in = "B"
     ch25_arr_in = "1234567890 1234567890 123"
     ch1_tin = "a"
     ch25_arr_tin = "bcdefghigklmnopqrstuvwxyz"
     ch1_all_in = "f"
     ch25_all_in = "{[(@#$%^&*|:<>?,./\;'`)]}"


!**********************************************************
!      Writing and Reading the file                      *
!**********************************************************

     OPEN(1, FILE='fxstio141.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(A25)', IOSTAT=ios, ERR=91) "This is the first record!"
     WRITE(1, FMT='(A1)', IOSTAT=ios, ERR=91, ADVANCE='no') ch1_in
     WRITE(1, FMT='(A26)', IOSTAT=ios, ERR=91) ch25_in
     WRITE(1, FMT='(100A1)', IOSTAT=ios, ERR=91) ch1_arr_in
     WRITE(1, FMT='(10A)', IOSTAT=ios, ERR=91) ch25_arr_in
     WRITE(1, FMT='(A1)', IOSTAT=ios, ERR=91) ch1_pin
     WRITE(1, FMT='(10A26)', IOSTAT=ios, ERR=91) ch25_arr_tin
     WRITE(1, FMT='(A1)', IOSTAT=ios, ERR=91) ch1_all_in
     WRITE(1, FMT='(100A25)', IOSTAT=ios, ERR=91) ch25_all_in
     WRITE(1, FMT='(A1)', IOSTAT=ios, ERR=91) ch1_par_in
     WRITE(1, FMT='(A20, 5H AIX.)', IOSTAT=ios, ERR=91) ch25_par_in
     WRITE(1, FMT='(25HThis is the last record!!)', IOSTAT=ios, ERR=91)


     READ(1, FMT='(A)', IOSTAT=ios, ERR=92, POS=1) ch25_ltrl_out1
     READ(1, FMT='(A1)', IOSTAT=ios, ERR=92) ch1_out
     READ(1, FMT='(A25)', IOSTAT=ios, ERR=92, POS=29) ch25_out
     READ(1, FMT='(100A1)', IOSTAT=ios, ERR=92) ch1_arr_out
     READ(1, FMT='(10A25)', IOSTAT=ios, ERR=92) ch25_arr_out
     READ(1, FMT='(A1)', IOSTAT=ios, ERR=92) ch1_pout
     READ(1, FMT='(10A26)', IOSTAT=ios, ERR=92) ch25_arr_tout
     READ(1, FMT='(A1)', IOSTAT=ios, ERR=92) ch1_all_out
     READ(1, FMT='(100A25)', IOSTAT=ios, ERR=92) ch25_all_out
     READ(1, FMT='(A1)', IOSTAT=ios, ERR=92) ch1_par_out
     READ(1, FMT='(A25)', IOSTAT=ios, ERR=92) ch25_par_out
     READ(1, FMT='(A20)', IOSTAT=ios, ERR=92) ch25_ltrl_out2


!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( ch25_ltrl_out1 .ne. "This is the first record!" ) error stop 19
     if ( ch25_in .ne. ch25_out) error stop 20
     if ( ch1_in .ne. ch1_out ) error stop 21
     if ( .not. Array_Check (ch1_arr_in, ch1_arr_out) ) error stop 22
     if ( .not. Array_Check (ch25_arr_in, ch25_arr_out) ) error stop 23
     if ( ch1_tin .ne. ch1_tout ) error stop 24
     if ( .not. Array_Check (ch25_arr_tin, ch25_arr_tout) ) error stop 25
     if ( ch1_all_in .ne. ch1_all_out ) error stop 26
     if ( .not. Array_Check (ch25_all_in, ch25_all_out) ) error stop 27
     if ( ch1_par_in .ne. ch1_par_out ) error stop 28
     if ( ch25_par_in .ne. ch25_par_out ) error stop 29
     if ( ch25_ltrl_out2 .ne. "This is the last rec     " ) error stop 29


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

