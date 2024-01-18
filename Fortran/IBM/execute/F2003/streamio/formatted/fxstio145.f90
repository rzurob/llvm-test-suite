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
!*  DESCRIPTION                : Test G edit descriptors with
!*				 formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/24/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  program fxstio145

     implicit none
     integer    ios , pos
     integer*2             :: i2_in, i2_out
     integer*4             :: i4_in, i4_out
     integer*8             :: i8_in, i8_out
     real*4                :: r4_in, r4_out
     real*8                :: r8_in, r8_out
     real*16               :: r16_in, r16_out
     complex*8             :: x8_in, x8_out
     complex*16            :: x16_in, x16_out
     complex*32            :: x32_in, x32_out
     logical*2             :: l2_in, l2_out
     logical*4             :: l4_in, l4_out
     logical*8             :: l8_in, l8_out
     character             :: ch1_in, ch1_out
     character*25          :: ch25_in, ch25_out
     byte                  :: b_in, b_out
     integer*4             :: i4_ltrl_in
     logical*4             :: l4_ltrl_in
     real*4                :: r4_ltrl_in
     complex*8             :: x8_ltrl_in
     character             :: ch1_ltrl_in
     character*20          :: ch20_ltrl_in
     byte                  :: b_ltrl_in

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3

!**********************************************************
!       Initialization                                    *
!**********************************************************

     i2_out = 133
     i4_out = 0
     i8_out = 133133133133_8
     r4_out = -0.000001
     r8_out = huge(r8_in)
     r16_out= huge(r16_in) / 2.
     x8_out = (huge(r4_in), -1.0 * huge(r4_in))
     x16_out= (0.0D0, -0.0D0)
     x32_out = (-0.1Q-309, 0.1Q309)
     l2_out = .false.
     l4_out = .true.
     l8_out = .false.
     ch1_out = '*'
     ch25_out = "This is just for fun!"
     b_out = 120


!**********************************************************
!      Writing and Reading the file                      *
!**********************************************************

     OPEN(1, FILE='fxstio145.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(G10.8)', IOSTAT=ios, ERR=91, ADVANCE='no') i2_out
     WRITE(1, FMT='(G10.0)', IOSTAT=ios, ERR=91, ADVANCE='yes') i4_out
     WRITE(1, FMT='(G12)', IOSTAT=ios, ERR=91) i8_out
     WRITE(1, FMT='(G7.1,G25.17D4,G40.32)', IOSTAT=ios, ERR=91) &
    &      r4_out, r8_out, r16_out
     WRITE(1, FMT='(2G15.7,2G25.17D3,2G40.32Q4)', IOSTAT=ios, ERR=91) &
    &      x8_out, x16_out, x32_out
     WRITE(1, FMT='(G4)', IOSTAT=ios, ERR=91, ADVANCE='no') l2_out
     WRITE(1, FMT='(G4)', IOSTAT=ios, ERR=91, ADVANCE='yes') l4_out
     WRITE(1, FMT='(G4)', IOSTAT=ios, ERR=91) l8_out
     WRITE(1, FMT='(G1,G25)', IOSTAT=ios, ERR=91) ch1_out, ch25_out
     WRITE(1, FMT='(G3)', IOSTAT=ios, ERR=91) b_out

     WRITE(1, FMT='(G8)', IOSTAT=ios, ERR=91, ADVANCE='no') 9876543_4
     WRITE(1, FMT='(15H 3.14 +1.5 -.20, G2)', IOSTAT=ios, ERR=91) .false._4
     WRITE(1, FMT='(G22)', IOSTAT=ios, ERR=91, ADVANCE='no') &
    &     "A beautiful flower pot"
     WRITE(1, FMT='(G3)', IOSTAT=ios, ERR=91) 99_1


     REWIND(1)


     READ(1, FMT='(G15)', IOSTAT=ios, ERR=92, ADVANCE='no') i2_in
     READ(1, FMT='(G5)', IOSTAT=ios, ERR=92, ADVANCE='yes') i4_in
     READ(1, FMT='(G15)', IOSTAT=ios, ERR=92) i8_in
     READ(1, FMT='(G7.1,G25.10,G40.15)', IOSTAT=ios, ERR=92) &
    &     r4_in, r8_in, r16_in
     READ(1, FMT='(2G15.5,2G25.14,2G40.20)', IOSTAT=ios, ERR=92) &
    &      x8_in, x16_in, x32_in
     READ(1, FMT='(G6)', IOSTAT=ios, ERR=92, ADVANCE='no') l2_in
     READ(1, FMT='(G2)', IOSTAT=ios, ERR=92, ADVANCE='yes') l4_in
     READ(1, FMT='(G6)', IOSTAT=ios, ERR=92) l8_in
     READ(1, FMT='(G1,G25)', IOSTAT=ios, ERR=92) ch1_in, ch25_in
     READ(1, FMT='(G5)', IOSTAT=ios, ERR=92) b_in

     READ(1, FMT='(G9.9)', IOSTAT=ios, ERR=92, ADVANCE='no') i4_ltrl_in
     READ(1, FMT='(G5.2, 2G5.1, G2)', IOSTAT=ios, ERR=92) &
    &       r4_ltrl_in, x8_ltrl_in, l4_ltrl_in
     READ(1, FMT='(G1, G21)', IOSTAT=ios, ERR=92) ch1_ltrl_in, ch20_ltrl_in

     READ(1, FMT='(G2)', IOSTAT=ios, ERR=92, POS=363) b_ltrl_in


!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( i2_in .ne. i2_out) error stop 21
     if ( i4_in .ne. i4_out) error stop 22
     if ( i8_in .ne. i8_out) error stop 23

     if ( .not. precision_R4(r4_in, r4_out)) error stop 24
     if ( .not. precision_R8(r8_in, r8_out)) error stop 25
     if ( .not. precision_R6(r16_in, r16_out)) error stop 26

     if ( .not. precision_x8(x8_in,  x8_out )) error stop 28
     if ( .not. precision_x6(x16_in, x16_out)) error stop 29
     if ( .not. precision_x3(x32_in, x32_out)) error stop 30

     if ( l2_in .neqv. l2_out) error stop 35
     if ( l4_in .neqv. l4_out) error stop 36
     if ( l8_in .neqv. l8_out) error stop 37

     if ( i4_ltrl_in .ne. 9876543) error stop 42
     if ( r4_ltrl_in .ne. 3.14 ) error stop 43
     if ( x8_ltrl_in .ne. (+1.5,-.2) ) error stop 44
     if ( l4_ltrl_in .neqv. .false.) error stop 45
     if ( ch1_ltrl_in .ne. 'A') error stop 46
     if ( ch20_ltrl_in .ne. 'beautiful flower pot') error stop 47

         print *, b_ltrl_in
     if ( b_ltrl_in .ne. 99) error stop 48

!     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92

   end program

