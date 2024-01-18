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
!*  DESCRIPTION                : Test NameList with derived type
!*                               in Stream I/O
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  03/31/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  include 'check_array.inc'

  program fxstio163

     implicit none
     integer    ios, i
     integer, parameter :: N = 4

     type dt_i
        integer*2  i2
        integer*4  i4(N)
        integer*8  i8
     end type

     type dt_r
        real*4     r4
        real*8     r8
        real*16    r16
     end type

     type dt_x
        complex*8  x8
        complex*16 x16
        complex*32 x32
     end type

     type dt_l
        logical*2  l2(N)
        logical*4  l4
        logical*8  l8
     end type

     type dt_c
         character    ch1(N)
         character*15 ch15
         character*25 ch25
     end type

     type(dt_i)    dti_in, dti_out
     type(dt_r)    dtr_in, dtr_out
     type(dt_x)    dtx_in, dtx_out
     type(dt_l)    dtl_in, dtl_out
     type(dt_c)    dtc_in(N), dtc_out(N)


     NAMELIST /name_in1/ dti_in, dtr_in
     NAMELIST /name_in2/ dtx_in, dtl_in
     NAMELIST /name_in3/ dtc_in

     NAMELIST /name_out1/ dti_out, dtr_out
     NAMELIST /name_out2/ dtx_out, dtl_out
     NAMELIST /name_out3/ dtc_out

     logical precision_R4, precision_R8, precision_R6
     logical precision_x8, precision_x6, precision_x3

     include 'check_interface.inc'

!**********************************************************
!        Initialization of variables                      *
!**********************************************************

     dti_out = dt_i(12, (/-1234, 234, 673, 0/), 123456789)
     dtr_out = dt_r(-0.000001, 0.1D300, -0.1Q-300)
     dtx_out = dt_x((3.14, -1.0), (0.0D0, -0.0D0), (-0.1Q-309, 0.1Q309))
     dtl_out = dt_l((/.true.,.false.,.true.,.false./), .false., .true.)
     dtc_out = dt_c((/"C", "D","e","f"/) , " XLFortran V8.1", &
    &                "\'This is just for fun /\'" )


!**********************************************************
!        Writing Namelists to the file                    *
!**********************************************************

     OPEN(1, FILE='fxstio163.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, ACTION='WRITE', DELIM='QUOTE')

     WRITE(1, NML=name_out1, IOSTAT=ios, ERR=91)
     WRITE(1, name_out2, IOSTAT=ios, ERR=91)
     WRITE(1, NML=name_out3, IOSTAT=ios, ERR=91)

     CLOSE(1)

!**********************************************************
!        Reading Namelists from the file                  *
!**********************************************************

     OPEN(1, FILE='fxstio163.in', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='OLD', IOSTAT=ios, ERR=90, ACTION='READ', DELIM='QUOTE')

     READ(1, NML=name_in1, IOSTAT=ios, ERR=92)
     READ(1, name_in2, IOSTAT=ios, ERR=92)
     READ(1, NML=name_in3, IOSTAT=ios, ERR=92)

!**********************************************************
!        Checking the Results                             *
!**********************************************************

     if ( dti_in%i2 .ne. dti_out%i2 ) error stop 21
     if ( .not. Array_Check (dti_in%i4, dti_out%i4)) error stop 22
     if ( dti_in%i8 .ne. dti_out%i8 ) error stop 23

     if ( .not. precision_R4(dtr_in%r4, dtr_out%r4)) error stop 24
     if ( .not. precision_R8(dtr_in%r8, dtr_out%r8)) error stop 25
     if ( .not. precision_R6(dtr_in%r16, dtr_out%r16)) error stop 26

     if ( .not. precision_X8(dtx_in%x8, dtx_out%x8)) error stop 27
     if ( .not. precision_X6(dtx_in%x16, dtx_out%x16)) error stop 28
     if ( .not. precision_X3(dtx_in%x32, dtx_out%x32)) error stop 29

     if ( .not. Array_Check (dtl_in%l2, dtl_out%l2) ) error stop 31
     if ( dtl_in%l4 .neqv. dtl_out%l4 ) error stop 32
     if ( dtl_in%l8 .neqv. dtl_out%l8 ) error stop 33

     do i= 1,N
       if ( .not. Array_Check (dtc_in(i)%ch1, dtc_out(i)%ch1) ) error stop 34
       if ( dtc_in(i)%ch15 .ne. dtc_out(i)%ch15 ) error stop 35
       if ( dtc_in(i)%ch25 .ne. dtc_out(i)%ch25 ) error stop 36
     end do


     CLOSE(1)

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92

   end program
