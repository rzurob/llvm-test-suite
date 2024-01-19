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
!*  DESCRIPTION                : Test I & L edit descriptors with
!*				 formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/24/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  include 'check_array.inc'

  program fxstio143

     implicit none
     integer    i, j, k, l, ios
     integer, parameter    :: N = 10
     integer*1             :: i1_in, i1_out
     integer*2             :: i2_in, i2_out
     integer*4             :: i4_in, i4_out
     integer*8             :: i8_in, i8_out
     logical*1             :: l1_in, l1_out
     logical*2             :: l2_in, l2_out
     logical*4             :: l4_in, l4_out
     logical*8             :: l8_in, l8_out
     integer*4             :: i4_arr_in(N), i4_arr_out(N)
     logical*4             :: l4_arr_in(N), l4_arr_out(N)
     integer*8, parameter  :: i8_par_out = 1234567890
     integer*8             :: i8_par_in
     logical*8, parameter  :: l8_par_out = .true._8
     logical*8             :: l8_par_in
     integer*4             :: i4_ltrl_in
     logical*2             :: l2_ltrl_in
     logical*4             :: l4_ltrl_in
     logical*8             :: l8_ltrl_in

     include 'check_interface.inc'

!**********************************************************
!       Initialization                                    *
!**********************************************************

     i1_out = 13
     i2_out = 133
     i4_out = 0
     i8_out = 133133133133_8
     l1_out = .true.
     l2_out = .false.
     l4_out = .true.
     l8_out = .false.

     do i=1,N
        i4_arr_out(i) = i
     enddo

     l4_arr_out = .true.

!**********************************************************
!      Writing and Reading the file                      *
!**********************************************************

     OPEN(1, FILE='fxstio143.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(I10)', IOSTAT=ios, ERR=91, ADVANCE='no') i1_out
     WRITE(1, FMT='(I10.8)', IOSTAT=ios, ERR=91, ADVANCE='no') i2_out
     WRITE(1, FMT='(I10.0)', IOSTAT=ios, ERR=91, ADVANCE='yes') i4_out
     WRITE(1, FMT='(I0)', IOSTAT=ios, ERR=91) i8_out
     WRITE(1, FMT='(L4)', IOSTAT=ios, ERR=91, ADVANCE='no') l1_out
     WRITE(1, FMT='(L4)', IOSTAT=ios, ERR=91, ADVANCE='no') l2_out
     WRITE(1, FMT='(L4)', IOSTAT=ios, ERR=91, ADVANCE='yes') l4_out
     WRITE(1, FMT='(L4)', IOSTAT=ios, ERR=91) l8_out

     do i = 1, N
        WRITE(1, FMT='(I10)', IOSTAT=ios, ERR=91, ADVANCE='no') i4_arr_out(i)
        WRITE(1, FMT='(L5)', IOSTAT=ios, ERR=91, ADVANCE='yes') l4_arr_out(i)
     enddo

     WRITE(1, FMT='(I12.12)', IOSTAT=ios, ERR=91, ADVANCE='no') i8_par_out
     WRITE(1, FMT='(L1)', IOSTAT=ios, ERR=91, ADVANCE='yes') l8_par_out

     WRITE(1, FMT='(I8)', IOSTAT=ios, ERR=91, ADVANCE='no') 9876543_4
     WRITE(1, FMT='(L2, 15H .true. .false.)', IOSTAT=ios, ERR=91, ADVANCE='no') &
    &     .false._4

     ENDFILE(1)

     READ(1, FMT='(I10)', IOSTAT=ios, ERR=92, ADVANCE='no', POS=1) i1_in
     READ(1, FMT='(I10)', IOSTAT=ios, ERR=92, ADVANCE='no') i2_in
     READ(1, FMT='(I10)', IOSTAT=ios, ERR=92, ADVANCE='yes') i4_in
     READ(1, FMT='(I12)', IOSTAT=ios, ERR=92) i8_in
     READ(1, FMT='(L4)', IOSTAT=ios, ERR=92, ADVANCE='no') l1_in
     READ(1, FMT='(L4)', IOSTAT=ios, ERR=92, ADVANCE='no') l2_in
     READ(1, FMT='(L4)', IOSTAT=ios, ERR=92, ADVANCE='yes') l4_in
     READ(1, FMT='(L4)', IOSTAT=ios, ERR=92) l8_in

     do i = 1, N
        READ(1, FMT='(I10)', IOSTAT=ios, ERR=92, ADVANCE='no') i4_arr_in(i)
        READ(1, FMT='(L5)', IOSTAT=ios, ERR=92, ADVANCE='yes') l4_arr_in(i)
     enddo

     READ(1, FMT='(I12)', IOSTAT=ios, ERR=92, ADVANCE='no') i8_par_in
     READ(1, FMT='(L1)', IOSTAT=ios, ERR=92) l8_par_in
     READ(1, FMT='(I9.9)', IOSTAT=ios, ERR=92, ADVANCE='no') i4_ltrl_in
     READ(1, FMT='(L2, 2L7)', IOSTAT=ios, ERR=92) &
    &      l2_ltrl_in, l4_ltrl_in, l8_ltrl_in

!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( i1_in .ne. i1_out) error stop 20
     if ( i2_in .ne. i2_out) error stop 21
     if ( i4_in .ne. i4_out) error stop 22
     if ( i8_in .ne. i8_out) error stop 23
     if ( l1_in .neqv. l1_out) error stop 24
     if ( l2_in .neqv. l2_out) error stop 25
     if ( l4_in .neqv. l4_out) error stop 26
     if ( l8_in .neqv. l8_out) error stop 27
     if ( .not. Array_Check (i4_arr_in, i4_arr_out) ) error stop 28
     if ( .not. Array_Check (l4_arr_in, l4_arr_out) ) error stop 29
     if ( i8_par_in .ne. i8_par_out) error stop 30
     if ( l8_par_in .neqv. l8_par_out) error stop 31
     if ( i4_ltrl_in .ne. 9876543) error stop 32
     if ( l2_ltrl_in .neqv. .false.) error stop 33
     if ( l4_ltrl_in .neqv. .true.) error stop 34
     if ( l8_ltrl_in .neqv. .false.) error stop 35

     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92

   end program

