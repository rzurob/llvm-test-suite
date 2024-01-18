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
!*  DESCRIPTION                : Test -qxlf77=oldboz with B, O, Z edit
!*				 descriptors in Stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  04/01/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  program fxstio216

     implicit none
     integer    ios
     integer*1             :: i1_in, i1_out
     integer*2             :: i2_in, i2_out
     integer*4             :: i4_in, i4_out
     integer*8             :: i8_in, i8_out
     real*4                :: r4_in, r4_out
     real*8                :: r8_in, r8_out
     real*16               :: r16_in, r16_out
     logical*2             :: l2_in, l2_out
     logical*4             :: l4_in, l4_out
     logical*8             :: l8_in, l8_out


!**********************************************************
!       Initialization                                    *
!**********************************************************

     i1_out = 13
     i2_out = 133
     i4_out = 0
     i8_out = 133133133133_8
     r4_out = 3.14
     r8_out = huge(r8_out)
     r16_out = -0.123Q300
     l2_out = .false.
     l4_out = .true.
     l8_out = .false.

!**********************************************************
!      Writing and Reading the file                      *
!**********************************************************

     OPEN(1, FILE='fxstio216.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(B8, TR5, O3, TR5, Z3)', IOSTAT=ios, ERR=91) &
    &          i1_out, i1_out, i1_out

     WRITE(1, FMT='(B20.16, TR5, O4, TR5, Z4)', IOSTAT=ios, ERR=91) &
    &          i2_out, i2_out, i2_out

     WRITE(1, FMT='(B32.0, TR5, O5, TR5, Z5)', IOSTAT=ios, ERR=91) &
    &          i4_out, i4_out, i4_out

     WRITE(1, FMT='(B0, TR5, O4.2, TR5, Z4.2)', IOSTAT=ios, ERR=91)  &
    &          i8_out, i8_out, i8_out

     WRITE(1, FMT='(B34.32, TR5, O5.2, TR5, Z5.2)', IOSTAT=ios, ERR=91) &
    &          r4_out, r4_out, r4_out

     WRITE(1, FMT='(B66, TR5, O8.3, TR5, Z3)', IOSTAT=ios, ERR=91)  &
    &          r8_out, r8_out, r8_out

     WRITE(1, FMT='(B130, TR5, O8, TR5, Z8)', IOSTAT=ios, ERR=91) &
    &          r16_out, r16_out, r16_out

     WRITE(1, FMT='(B17.16, TR5, O3, TR5, Z3)', IOSTAT=ios, ERR=91) &
    &          l2_out, l2_out, l2_out

     WRITE(1, FMT='(B3, TR5, O3, TR5, Z3)', IOSTAT=ios, ERR=91)   &
    &          l4_out, l4_out, l4_out

     WRITE(1, FMT='(B1, TR5, O3, TR5, Z3)', IOSTAT=ios, ERR=91)  &
    &          l8_out, l8_out, l8_out

     WRITE(1, FMT='(B22.16, TR5, O5, TR5, Z3)', IOSTAT=ios, ERR=91) 9876543_4, 9876543_4, 9876543_4



     CLOSE(1)



     OPEN(1, FILE='fxstio216.in', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='OLD', IOSTAT=ios, ERR=90)

     READ(1, FMT='(B8, O5, Z5, BN, B30)', IOSTAT=ios, ERR=92) &
    &       i1_in, i2_in, i4_in, i8_in

     READ(1, FMT='(B5, O4, Z3)', IOSTAT=ios, ERR=92) &
    &       l2_in, l4_in, l8_in

!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( i1_in .ne. B'00110100') error stop 20
     if ( i2_in .ne. O'02050') error stop 21
     if ( i4_in .ne. Z'00DF0') error stop 22
     if ( i8_in .ne. B'000000001000010100000000000000') error stop 23
     if ( l2_in .neqv. .false.) error stop 28
     if ( l4_in .neqv. .true.) error stop 29
     if ( l8_in .neqv. .false.) error stop 30

     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92

   end program

