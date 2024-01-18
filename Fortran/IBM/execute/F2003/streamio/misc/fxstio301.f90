! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 25, 2003
!*
!*  Primary Function Tested    : Impilictly-connected file with stream
!*                               I/O
!*
!*  Description                : using implicit connection to connect
!*                               a file to a unit.
!*
!***********************************************************************

!* Declare and initialize Variables.

   integer ios
   integer*1 int1 /127/, int11         !* max value for integer*1
   integer*2 int2 /-128/, int21        !* min value for integer*2
   integer*4 int4 /2147483647/, int41  !* max value for integer*4
   integer*8 int8 /1000000000/, int81

   real*4  real4 /3.402823E+38/       !* Approximate absolute max
   real*8  real8 /2.225074D-308/      !* Appoximate absolute nonzero min
   real*16 real16 /1.797693Q+308/     !* Appoximate absolute max
   real*4  real41
   real*8  real81
   real*16 real161

   complex*8  complex8 /(3.4E30, 0.1E-1)/
   complex*16 complex16 /(3.4D28, 0.1D-1)/
   complex*32 complex32 /(3.4Q30, 0.1Q-1)/
   complex*8  complex81
   complex*16 complex161
   complex*32 complex321

   character*1  char1 /'a'/
   character*20 char20 /'abcdefghijklmnobpqrs'/
   character*1  char11
   character*20 char201

   logical*1 log1 /.true./, log11
   logical*2 log2 /.false./, log21
   logical*4 log4 /.true./, log41
   logical*8 log8 /.false./, log81
   logical precision_x6, precision_x8, precision_x3

   byte byte1 /48/
   byte byte11

!* TEST1 : integer

   close(6)
   open(6, access='stream', form='unformatted', action='readwrite',&
      iostat=ios, err=100)
   write(6, iostat=ios, err=200) int1, int2, int4, int8
   rewind(6, iostat=ios, err=500)
   read (6, iostat=ios, err=400) int11, int21, int41, int81

   if ((int1 .ne. int11) .or. (int2 .ne. int21) .or. &
       (int4 .ne. int41) .or. (int8 .ne. int81))  then
       stop 1
   endif
   close(6, status='delete')

!* TEST2 : complex
!  close(6)
   open(6, access='stream', form='unformatted', action='readwrite', &
      iostat=ios, err=100)
   write(6, iostat=ios, err=200) complex8, complex16, complex32
   rewind(6, iostat=ios, err=500)
   read(6, iostat=ios, err=400) complex81, complex161, complex321
   if ((.not.precision_x8(complex81, (3.4E30, 0.1E-1))) .or. &
       (.not.precision_x6(complex161, (3.4D28, 0.1D-1))) .or. &
       (.not.precision_x3(complex321, (3.4Q30, 0.1Q-1)))) error stop 3
!  if ((complex8 .ne. complex81) .or. (complex16 .ne. complex161) .or. &
!      (complex32 .ne. complex321))  error stop 3
   close(6, status='delete')

!* TEST3 : logical

!  close(6)
   open(6, access='stream', form='unformatted', action='readwrite', &
      iostat=ios, err=100)
   write(6, iostat=ios, err=200) log1, log2, log4, log8
   rewind(6, iostat=ios, err=500)
   read(6, iostat=ios, err=400 ) log11, log21, log41, log81
   if ((log11 .neqv. .true.) .or. (log21 .neqv. .false.) .or. &
      (log41 .neqv. .true.) .or. (log81 .neqv. .false.)) error stop 5
   close(6, status='delete')

!* TEST6 : byte
!  close(6)
   open(6, access='stream', form='unformatted', action='readwrite', &
      iostat=ios, err=100)
   write(6, iostat=ios, err=200) byte1
   rewind(6, iostat=ios, err=500)
   read(6, iostat=ios, err=400) byte11
   if (byte1 .ne. byte11) error stop 6
   close(6, status='delete')

stop

100 stop 100
200 stop 200
300 stop 300
400 stop 400
500 stop 500

end

