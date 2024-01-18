!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: 
! %GROUP:  fxstio109.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : I/O Stream Access
!*
!*  PROGRAMMER                 : Bahram Chehrazy
!*  DATE                       : March 2003
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test formatted synchronous stream I/O
!*				 with different expression and literal
!*                               constants including Hollerith constant.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments: 
!*  03/14/03   BC     Initial version 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890 

  program fxstio109 

     implicit none
     integer       ios
     integer*2    :: i2_in = 1234
     integer*2    :: i2_out
     integer*4    :: i4_in = -20000000
     integer*4    :: i4_out  
     integer*8    :: i8_in = 1234567890
     integer*8    :: i8_out
     real*4       :: r4_in = -0.000001
     real*4    	  :: r4_out
     real*8       :: r8_in = -0.1D-309
     real*8       :: r8_out
     real*16      :: r16_in
     real*16      :: r16_out
     complex*8    :: x8_in = (1.876, -98.654)
     complex*8    :: x8_out
     complex*16   :: x16_in = (0.0D0, -0.0D0)
     complex*16   :: x16_out
     complex*32   :: x32_in = (.5 * huge(r16_in), -0.5 * huge(r16_in))
     complex*32   :: x32_out
     logical*2    :: l2_in = .true.
     logical*2    :: l2_out
     logical*4    :: l4_in = .false.
     logical*4    :: l4_out
     logical*8    :: l8_in = .true. 
     logical*8    :: l8_out
     character    :: ch1_in = 'A' 
     character    :: ch1_out  
     character*15 :: ch15_in = "New Baby Girl! " 
     character*15 :: ch15_out, H_out  
     byte         :: b_in=B'01010111', O_in=O'137', Z_in=Z'EF'
     byte         :: b_out, O_out, Z_out

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3
	

     r16_in = huge(r16_in) / 2.Q0

!********************************************************** 
!        Writing and Reading the file                     *
!********************************************************** 

     OPEN(1, FILE='fxstio109.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(I5, I10, I20)', IOSTAT=ios, ERR=91) &
    &      1234, -20000000, (i8_in - 8*4 + 32) 
     WRITE(1, FMT='(F9.6,D25.17,Q40.32)', IOSTAT=ios, ERR=91) & 
    &      -0.000001, -0.1D-1 / 0.1D309, (huge(r16_in) * (r4_in / (-0.2E-5))) 
     WRITE(1, FMT='(2E15.7,2D25.17,2Q40.32)', IOSTAT=ios, ERR=91) &
    &     (1.876, -98.654), &
    &     (0.1D100, -0.71D-30) - (0.1D100, -0.71D-30),  &
    &     (huge(r16_in) / 2.0, -0.25Q0 * huge(r16_in) * .2Q1)
     WRITE(1, FMT='(3L5)', IOSTAT=ios, ERR=91) &
    &      .true._2, .false., (l8_in .or. .false._8)
     WRITE(1, FMT='(A1,A16)', IOSTAT=ios, ERR=91) &
    &      'A', "New "//"Baby "//"Girl! "
     WRITE(1, FMT='(B8, O3, Z2)', IOSTAT=ios, ERR=91) &
    &      B'01010111', O'137', Z'EF'

     WRITE(1, FMT='(15H The last\n one! )', IOSTAT=ios, ERR=91)

     REWIND(1, IOSTAT=ios, ERR=93)

     READ(1, FMT='(I5, I10, I20)', IOSTAT=ios, ERR=92) i2_out, i4_out, i8_out
     READ(1, FMT='(F9.6,D25.17,Q40.32)', IOSTAT=ios, ERR=92) &
    &      r4_out, r8_out, r16_out
     READ(1, FMT='(2E15.7,2D25.17,2Q40.32)', IOSTAT=ios, ERR=92) &
    &      x8_out, x16_out, x32_out
     READ(1, FMT='(3L5)', IOSTAT=ios, ERR=92) l2_out, l4_out, l8_out
     READ(1, FMT='(A1,A16)', IOSTAT=ios, ERR=92) ch1_out, ch15_out
     READ(1, FMT='(B8, O3, Z2)', IOSTAT=ios, ERR=92) b_out, O_out, Z_out
     READ(1, FMT='(A9, /, A)', IOSTAT=ios, ERR=92) H_out(:9), H_out(10:)


!********************************************************** 
!        Checking the Results                             *
!********************************************************** 

     if ( i2_in .ne. i2_out ) error stop 10
     if ( i4_in .ne. i4_out ) error stop 11
     if ( i8_in .ne. i8_out ) error stop 12

     if ( .not. precision_R4(r4_in, r4_out)) error stop 13
     if ( .not. precision_R8(r8_in, r8_out)) error stop 14
     if ( .not. precision_R6(r16_in, r16_out)) error stop 15

     if ( .not. precision_x8(x8_in,  x8_out )) error stop 16
     if ( .not. precision_x6(x16_in, x16_out)) error stop 17
     if ( .not. precision_x3(x32_in, x32_out)) error stop 18

     if ( l2_in .neqv. l2_out) error stop 19
     if ( l4_in .neqv. l4_out) error stop 20
     if ( l8_in .neqv. l8_out) error stop 21

     if ( ch1_in .ne. ch1_out ) error stop 22
     if ( ch15_in .ne. ch15_out ) error stop 23

     if ( b_in .ne. b_out) error stop 24
     if ( O_in .ne. O_out) error stop 25
     if ( Z_in .ne. Z_out) error stop 26

      print *, H_out 
     if ( H_out .ne. " The last one! ") error stop 27

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
