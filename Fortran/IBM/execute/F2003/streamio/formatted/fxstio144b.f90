!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP:  fxstio144b.f
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
!*  TEST CASE TITLE            : I/O Stream Access Mode
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
!*  DESCRIPTION                : Test O edit descriptors in
!*				 formatted stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments: 
!*  03/25/03   BC     Initial version 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890 

  program fxstio144b 

     implicit none
     integer    i, j, k, l, ios
     integer, parameter    :: N = 10
     integer*1             :: i1_in, i1_out  
     integer*2             :: i2_in, i2_out  
     integer*4             :: i4_in, i4_out  
     integer*8             :: i8_in, i8_out  
     real*4                :: r4_in, r4_out  
     real*8                :: r8_in, r8_out  
     real*16               :: r16_in, r16_out  
     logical*1             :: l1_in, l1_out  
     logical*2             :: l2_in, l2_out  
     logical*4             :: l4_in, l4_out  
     logical*8             :: l8_in, l8_out  
     integer*8, parameter  :: i8_par_out = 1234567890
     integer*8             :: i8_par_in
     logical*8, parameter  :: l8_par_out = .true._8
     logical*8             :: l8_par_in
     byte                  :: b1_in, b1_out  
     character             :: ch1_in, ch1_out  
     integer*2             :: i2_ltrl_in
     integer*4             :: i4_ltrl_in
     integer*8             :: i8_ltrl_in
     real*4                :: r4_ltrl_in
     real*8                :: r8_ltrl_in
     real*16               :: r16_ltrl_in
     logical*4             :: l4_ltrl_in
     byte                  :: b1_ltrl_in

    logical precision_R4, precision_R8, precision_R6
     
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
     l1_out = .true.
     l2_out = .false.
     l4_out = .true.
     l8_out = .false.
     b1_out = O'347'
     ch1_out = 'A'

!********************************************************** 
!      Writing and Reading the file                      *
!********************************************************** 

     OPEN(1, FILE='fxstio144b.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90)

     WRITE(1, FMT='(O4.3 O7.6, O12.0)', IOSTAT=ios, ERR=91, ADVANCE='no') &
    &      i1_out, i2_out, i4_out
     WRITE(1, FMT='(O0)', IOSTAT=ios, ERR=91) i8_out
     WRITE(1, FMT='(O12.5, O22, O45)', IOSTAT=ios, ERR=91) &
    &      r4_out, r8_out, r16_out
     WRITE(1, FMT='(O1, O2, O3, O4)', IOSTAT=ios, ERR=91) &
    &      l1_out, l2_out, l4_out, l8_out 
     WRITE(1, FMT='(O3)', IOSTAT=ios, ERR=91) b1_out
     WRITE(1, FMT='(O3)', IOSTAT=ios, ERR=91) ch1_out
     WRITE(1, FMT='(O25.22)', IOSTAT=ios, ERR=91) i8_par_out
     WRITE(1, FMT='(O0.0)', IOSTAT=ios, ERR=91) l8_par_out
     WRITE(1, FMT='(O8)', IOSTAT=ios, ERR=91) 9876543_4
     WRITE(1, FMT='(O11)', IOSTAT=ios, ERR=91) 3.14 
     WRITE(1, FMT='(O22)', IOSTAT=ios, ERR=91) 3.1456789D-20 
     WRITE(1, FMT='(O44)', IOSTAT=ios, ERR=91) -3.1456789Q+120 
     WRITE(1, FMT='(O2, 3H 57))', IOSTAT=ios, ERR=91) .false._4

     REWIND(1)

     READ(1, FMT='(O4, O7, O12.0, O13)', IOSTAT=ios, ERR=92) &
    &       i1_in, i2_in, i4_in, i8_in
     READ(1, FMT='(O12.5, O22, O45)', IOSTAT=ios, ERR=92) &
    &       r4_in, r8_in, r16_in
     READ(1, FMT='(O1, O2, O3, O4)', IOSTAT=ios, ERR=92) &
    &       l1_in, l2_in, l4_in, l8_in
     READ(1, FMT='(O3)', IOSTAT=ios, ERR=92) b1_in
     READ(1, FMT='(O3)', IOSTAT=ios, ERR=92) ch1_in
     READ(1, FMT='(O25)', IOSTAT=ios, ERR=92) i8_par_in
     READ(1, FMT='(O22)', IOSTAT=ios, ERR=92) l8_par_in
     READ(1, FMT='(O11)', IOSTAT=ios, ERR=92) i4_ltrl_in
     READ(1, FMT='(O11)', IOSTAT=ios, ERR=92) r4_ltrl_in
     READ(1, FMT='(O22)', IOSTAT=ios, ERR=92) r8_ltrl_in
     READ(1, FMT='(O44)', IOSTAT=ios, ERR=92) r16_ltrl_in
     READ(1, FMT='(O2, O3)', IOSTAT=ios, ERR=92) l4_ltrl_in, b1_ltrl_in

!********************************************************** 
!        Checking the Results                             *
!********************************************************** 

     if ( i1_in .ne. i1_out) error stop 20
     if ( i2_in .ne. i2_out) error stop 21
     if ( i4_in .ne. i4_out) error stop 22
     if ( i8_in .ne. i8_out) error stop 23
     if ( .not. precision_R4(r4_in, r4_out)) error stop 24
     if ( .not. precision_R8(r8_in, r8_out)) error stop 25
     if ( .not. precision_R6(r16_in, r16_out)) error stop 26
     if ( l1_in .neqv. l1_out) error stop 27
     if ( l2_in .neqv. l2_out) error stop 28
     if ( l4_in .neqv. l4_out) error stop 29
     if ( l8_in .neqv. l8_out) error stop 30
     if ( i8_par_in .ne. i8_par_out) error stop 31
     if ( l8_par_in .neqv. l8_par_out) error stop 32
     if ( i4_ltrl_in .ne. 9876543) error stop 33
     if ( .not. precision_R4(r4_ltrl_in, 3.14)) error stop 34
     if ( .not. precision_R8(r8_ltrl_in, 3.1456789D-20)) error stop 35
     if ( .not. precision_R6(r16_ltrl_in, -3.1456789Q+120)) error stop 36
     if ( l4_ltrl_in .neqv. .false.) error stop 37
     if ( b1_ltrl_in .ne. O'57') error stop 38

     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90 
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91 
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92 

   end program

