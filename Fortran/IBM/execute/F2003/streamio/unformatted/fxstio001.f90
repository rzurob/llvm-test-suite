! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:  rm -f fort.*
! %COMPOPTS:
! %GROUP: fxstio001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 07, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test all intrinsic data types with
!*                               Unformatted Synchronous Stream I/O.
!*
!=======================================================================

!* Declare and initialize variables.

   implicit none
   integer ios
   integer*1 int1 /127/ , int11        !* max value for integer*1
   integer*2 int2 /-128/, int21
   integer*4 int4 /2147483647/ , int41 !* max value for integer*4
   integer*8 int8 /1000000000/, int81

   real*4  real4 /3.402823E+38/,real41       !* Approximate absolute max
   real*8  real8 /2.225074D-308/,real81      !* Appoximate absolute nonzero min
   real*16 real16 /1.797693Q+308/,real61   !* Appoximate absolute max

   complex*8  complex8 /(3.4E30, 0.1E-1)/, complex81
   complex*16 complex16 /(3.4D28, 0.1D-1)/, complex161
   complex*32 complex32 /(3.4Q30, 0.1Q-1)/, complex321

   character*1  char1 /'a'/, char11
   character*20 char20 /'abcdefghijklmnobpqrs'/, char201

   logical*1 log1 /.true./, log11
   logical*2 log2 /.false./, log21
   logical*4 log4 /.true./, log41
   logical*8 log8 /.false./, log81
   logical precision_r4, precision_r8, precision_r6
   logical precision_x8, precision_x6, precision_x3

   byte byte1 /48/, byte11

!* TEST1 : integer

   open(1, access='stream', form='unformatted', action='readwrite',&
      iostat=ios, err=100)
   write(1 , iostat=ios, err=200) int1, int2, int4, int8
   rewind(1, iostat=ios, err=500)
   read (1, iostat=ios, err=400) int11, int21, int41, int81

   if ((int1 .ne. int11) .or. (int2 .ne. int21) .or. &
       (int4 .ne. int41) .or. (int8 .ne. int81))     error stop 111

   close(1, status='delete')

!* TEST2 : real

   open(1, access='sequential', form='unformatted', action='readwrite', &
      iostat=ios, err=100)
   write(1,iostat=ios, err=200) real4, real8, real16
   rewind(1, iostat=ios, err=500)
   read(1, iostat=ios, err=400) real41, real81, real61
   print *, real4, real41
   print *, real8, real81
   print *, real16, real61

   if((.not. precision_r4(real4, real41)) .or. &
      (.not. precision_r8(real8, real81)) .or. &
      (.not. precision_r6(real16,real61)))      error stop 2

   close(1, status='delete')

!* TEST3 : complex

   open(1, access='stream', form='unformatted', action='readwrite', &
      iostat=ios, err=100)

   write(1, iostat=ios, err=200) complex8, complex16, complex32
   rewind(1, iostat=ios, err=500)
   read(1, iostat=ios, err=400) complex81, complex161, complex321

   if ((.not.precision_x8(complex81, (3.4E30, 0.1E-1))) .or. &
       (.not.precision_x6(complex161, (3.4D28, 0.1D-1))) .or. &
       (.not.precision_x3(complex321, (3.4Q30, 0.1Q-1)))) error stop 3

   close(1, status='delete')

!* TEST4 : character

   open(1, access='stream', form='unformatted', action='readwrite', &
      iostat=ios, err=100)

   write(1, iostat=ios, err=200) char1, char20
   rewind(1, iostat=ios, err=500)
   read(1, iostat=ios, err=400 ) char11, char201

   if ((char1 .ne. char11) .or. (char20 .ne. char201)) error stop 4

   close(1, status='delete')

!* TEST5 : logical

   open(1, access='stream', form='unformatted', action='readwrite', &
      iostat=ios, err=100)
   write(1, iostat=ios, err=200) log1, log2, log4, log8
   rewind(1, iostat=ios, err=500)
   read(1, iostat=ios, err=400 ) log11, log21, log41, log81
   print *, log11, log21, log41, log81

   if ((log11 .neqv. .true.) .or. (log21 .neqv. .false.) .or. &
      (log41 .neqv. .true.) .or. (log81 .neqv. .false.)) error stop 5

   close(1, status='delete')

!* TEST6 : byte

   open(1, access='stream', form='unformatted', action='readwrite', &
      iostat=ios, err=100)

   write(1, iostat=ios, err=200) byte1
   rewind(1, iostat=ios, err=500)
   read(1, iostat=ios, err=400) byte11

   if (byte1 .ne. byte11) error stop 6

   close(1, status='delete')

stop

100 print *, "open error: iostat = ", ios
    error stop 100
200 print *, "write error: iostat = ", ios
    error stop 200
300 print *, "inquire error: iostat = ", ios
    error stop 300
400 print *, "read error: iostat = ", ios
    error stop 400
500 print *, "rewind error: iostat = ", ios
    error stop 500

end

