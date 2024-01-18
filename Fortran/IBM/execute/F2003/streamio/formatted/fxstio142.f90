!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 2003
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE , READ
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test F, E, D, Q, ES, EN edit descriptors
!*				 in formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/24/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  include 'check_array.inc'

  program fxstio142

     implicit none
     integer  ios, pos
     integer, parameter   :: N = 10
     real                 :: r4_out , r4_in
     real*8               :: r8_out , r8_in
     real*16              :: r16_out , r16_in
     complex              :: x8_out , x8_in
     complex*16           :: x16_out , x16_in
     complex*32           :: x32_out , x32_in
     real*4               :: r4_arr_out(N), r4_arr_in(N)
     complex*16           :: x16_arr_out(N), x16_arr_in(N)
     real*8, pointer      :: r8_pout
     real*8, target       :: r8_tout
     complex*8, pointer   :: x8_pout
     complex*8, target    :: x8_tout
     real*16, pointer     :: r16_arr_pout(:)
     real*16, target      :: r16_arr_tout(N)
     complex*16, pointer  :: x16_arr_pout(:)
     complex*16, target   :: x16_arr_tout(N)
     real, allocatable    :: r4_all_out
     real, allocatable    :: r4_all_arr_out(:)
     complex, allocatable :: x8_all_out
     complex, allocatable :: x8_all_arr_out(:)
     real*8, parameter    :: r8_par_out = -0.0314D-3
     complex, parameter   :: x8_par_out = (-.8314D-3, +9.0653D+2)

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3

     include 'check_interface.inc'


!**********************************************************
!       Allocation, Association & Initialization          *
!**********************************************************

     allocate(r4_all_out)
     allocate(r4_all_arr_out(N))
     allocate(x8_all_out)
     allocate(x8_all_arr_out(N))

     r8_pout => r8_tout
     x8_pout => x8_tout
     r16_arr_pout => r16_arr_tout
     x16_arr_pout => x16_arr_tout

     r4_out = -0.001
     r8_out = huge(r8_out)
     r16_out = huge(r16_out)
     x8_out = (huge(r4_out), -1.23)
     x16_out = (-0.179769313D-300, huge(r8_out))
     x32_out = (-3.8764Q-24, 3.8764Q-24)
     r4_arr_out = 297.389
     x16_arr_out = (97.1D+21, 4.9D+23)
     r8_tout = 3.14D0
     x8_tout = (3.14, -9.68)
     r16_arr_tout = -3.14987Q+7
     x16_arr_tout = (2.87,-9.087)
     r4_all_out = .1487
     r4_all_arr_out = -.1487E9
     x8_all_out = (1.12345678, -.0112344579)
     x8_all_arr_out = (0.0, -0.0)

!**********************************************************
!      Writing and Reading the file                      *
!**********************************************************

     OPEN(1, FILE='fxstio142.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, ACTION='write')

     WRITE(1, FMT='(F9.5, D16.9, Q20.13)', IOSTAT=ios, ERR=91) &
    &      r4_out, r8_out, r16_out

     WRITE(1, FMT='(E9.3, F0.1, 2D19.11, 2H  ,2Q13.7)', IOSTAT=ios, ERR=91) &
    &      x8_out, x16_out, x32_out

     WRITE(1, FMT='(10F7.2)', IOSTAT=ios, ERR=91) r4_arr_out

     WRITE(1, FMT='(2E15.3D4)', IOSTAT=ios, ERR=91) x16_arr_out

     WRITE(1, FMT='(E12.5D3, 2E12.5E3)', IOSTAT=ios, ERR=91) r8_pout, x8_tout

     WRITE(1, FMT='(10E12.5Q1, 2E12.5D1)', IOSTAT=ios, ERR=91) &
    &      r16_arr_tout, x16_arr_tout(5)

     WRITE(1, FMT='(EN12.3, EN15.5E2)', IOSTAT=ios, ERR=91) &
    &      r4_all_out, r4_all_arr_out(2:3)

     WRITE(1, FMT='(EN15.6, EN15.6)', IOSTAT=ios, ERR=91) &
    &      x8_all_out, x8_all_arr_out(1)

     WRITE(1, FMT='(ES15.5, 2ES15.5E1)', IOSTAT=ios, ERR=91) &
    &      r8_par_out, x8_par_out

     WRITE(1, FMT='(F7.1, F10.5, 2H  , F0.1)', IOSTAT=ios, ERR=91) &
    &        -9.863, -0.1234D-23, +3.387345765982Q+12

!
!  Testing non_advancing write
!
     WRITE(1, FMT='(2(F0.3,1H ))', IOSTAT=ios,ERR=91, ADVANCE='no') &
    &        (23.89, -3.14E+2)

     WRITE(1, FMT='(2E25.9)', IOSTAT=ios,ERR=91, ADVANCE='no') &
    &        (huge(r8_out), -1*huge(r8_out)/2)

!
!    Leaving a hole in the last record
!
     WRITE(1, FMT='(2Q30.9)', IOSTAT=ios,ERR=91, ADVANCE='yes', POS=1000) &
    &        (huge(r16_out), 0.0Q0)
     CLOSE(1)


!**********************************************************
!      Open and Read the file input file                  *
!**********************************************************

     OPEN(1, FILE='fxstio142.in', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='OLD', IOSTAT=ios, ERR=90)


     READ(1, FMT='(F9.5, D16.9, Q20.13)', IOSTAT=ios, ERR=92) &
    &     r4_in, r8_in, r16_in

     if ( .not. precision_R4 (r4_in, -0.001) )    error stop 20
     if ( .not. precision_R8 (r8_in, 0.179769313D+309) )    error stop 21
     if ( .not. precision_R6 (r16_in, 0.1797693134862Q+309))   error stop 22

     READ(1, FMT='(E9.3, F5.2)', IOSTAT=ios, ERR=92, ADVANCE='no', POS=47) x8_in

     if ( .not. precision_X8(x8_in, (0.340E+39,-1.2)) )    error stop 23

     READ(1, FMT='(2D19.11, 1H ,2Q13.7)', IOSTAT=ios, ERR=92) &
    &     x16_in, x32_in

     if ( .not. precision_X6(x16_in,(-0.17976931300D-300,0.7976931349D+308)))&
    &      error stop 24
     if ( .not. precision_X3 (x32_in, (-.3876400Q-23, 0.3876400Q-23))) &
    &       error stop 25

     READ(1, FMT='(10F7.2)', IOSTAT=ios, ERR=92, POS=127) r4_arr_in

     if ( .not. Array_Check (r4_arr_in, r4_arr_out ))   error stop 26

     READ(1, FMT='(2E15.3D4)', IOSTAT=ios, ERR=92) x16_arr_in

     if ( .not. Array_Check (x16_arr_in, x16_arr_out )) error stop 27

     READ(1, FMT='(F8.5, F10.9, F13.4)', IOSTAT=ios, ERR=92) &
    &     r4_in, r8_in, r16_in

     if ( .not. precision_R4 (r4_in, -0.00001) )    error stop 28
     if ( .not. precision_R8 (r8_in, 0.1797D-5) )    error stop 29
     if ( .not. precision_R6 (r16_in, 0.179769Q+20))   error stop 30

     CLOSE(1)

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading the file: IOSTAT = ", ios
     error stop 92

   end program

