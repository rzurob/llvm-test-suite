! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 22, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test size=specifer & stream=specifer
!*                               in inquire statment with a file
!*                               connected for direct/sequential access
!*                               I/O
!*
!=======================================================================

!* Declare and initialize variables.

   integer ios
   integer position /0/, fsize(3) /3*0/
   integer number /0/, iol /0/
   integer ivar1 /2000/

   real rarr1(3), rarr2(3)
   real rvar1 /-1.2e-30/, rvar2, rvar3
   real, parameter :: rpararr(3) = (/-1.175494e-38, 0.0e0, 3.402823e38/)

   complex carr1(3), carr2(3)
   complex cvar1 /(-1.175494e-38,3.402823e38)/, cvar2, cvar3
   complex, parameter :: cpararr(3) = (/(-3.402823e38,3.402823e38), &
  & (0.0e0, -1.0e-1), (1.175494e-38,-1.175494e-38)/)

   logical larr1(2), larr2(2)
   logical lvar1 /.true./, lvar2, lvar3
   logical, parameter :: lpararr(2) = (/.true.,.false./)

   character harr1(3), harr2(3)
   character hvar1 /'a'/, hvar2, hvar3
   character, parameter :: hpararr(3) = (/'x','1','z'/)
   character*20 acc, str

   byte barr1(3), barr2(3)
   byte bvar1 /'i'/, bvar2, bvar3
   byte, parameter :: bpararr(3) = (/'1', '2', 't'/)

!* TEST1 : integer

   open(1, access='stream', form='unformatted', iostat=ios,&
      err=100)
   inquire( iolength=iol ) ivar1
   write(1, num=number, err=200) ivar1
   if ( number .ne. iol )   error stop 11

   inquire(1, access=acc, size=fsize(1), stream=str, iostat=ios, err=200)

   if ( acc .ne. "STREAM" )    error stop 12
   if ( str .ne. "YES")        error stop 13
   if ( fsize(1) .ne. number )  error stop 15
   close(1)

!* connect to the file with direct access method
   open(1, access='direct', form='unformatted',recl=10, iostat=ios, err=100)

   inquire(1, access=acc, size=fsize(2), stream=str, iostat=ios, err=200)
   if ( acc .ne. "DIRECT" )    error stop 117
   if ( str .ne. "NO")        error stop 118
   if ( fsize(1) .ne. fsize(2)) error stop 119
   close(1)

!* connect to the file with default access method(sequential)
   open(1, form='unformatted', iostat=ios, err=100)

   inquire(1, access=acc, size=fsize(3), stream=str, iostat=ios, err=200)

   if ( acc .ne. "SEQUENTIAL" )    error stop 17
   if ( str .ne. "NO")        error stop 18
   if ( fsize(1) .ne. fsize(3)) error stop 19
   close(1, status='delete')

!* TEST2 : real

   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100)

   inquire( iolength=iol ) rvar1
   write(1, num=number, iostat=ios, err=200) rvar1
   if ( number .ne. iol )   error stop 21

   inquire(1, access=acc, size=fsize(1), stream=str, iostat=ios, err=200)

   if ( acc .ne. "STREAM" )    error stop 22
   if ( str .ne. "YES")        error stop 23
   if ( fsize(1) .ne. number)  error stop 25
   close(1)

!* connect to the file with direct access method
   open(1, access='direct', form='unformatted',recl=10, iostat=ios, err=100)

   inquire(1, access=acc, size=fsize(2), stream=str, iostat=ios, err=200)
   if ( acc .ne. "DIRECT" )    error stop 227
   if ( str .ne. "NO")        error stop 228
   if ( fsize(1) .ne. fsize(2)) error stop 229
   close(1)

!* connect to the file with default access method(sequential)
   open(1, form='unformatted', iostat=ios, err=100)

   inquire(1, access=acc, size=fsize(3), stream=str, iostat=ios, err=200)
   if ( acc .ne. "SEQUENTIAL" )    error stop 27
   if ( str .ne. "NO")        error stop 28
   if ( fsize(1) .ne. fsize(3)) error stop 29
   close(1, status='delete')

!* TEST3 : logical
   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100)

   inquire( iolength=iol ) lvar1
   write(1, num=number, err=200) lvar1
   if ( number .ne. iol )   error stop 31

   inquire(1, access=acc, size=fsize(1), stream=str, iostat=ios, err=200)

   if ( acc .ne. "STREAM" )    error stop 32
   if ( str .ne. "YES")        error stop 33
   if ( fsize(1) .ne. number )  error stop 35
   close(1)

!* connect to the file with direct access method
   open(1, access='direct', form='unformatted',recl=10, iostat=ios, err=100)

   inquire(1, access=acc, size=fsize(2), stream=str, iostat=ios, err=200)
   if ( acc .ne. "DIRECT" )    error stop 337
   if ( str .ne. "NO")        error stop 338
   if ( fsize(1) .ne. fsize(2)) error stop 339
   close(1)

!* connect to the file with default access method(sequential)
   open(1, form='unformatted', iostat=ios, err=100)

   inquire(1, access=acc, size=fsize(3), stream=str, iostat=ios, err=200)

   if ( acc .ne. "SEQUENTIAL" )    error stop 37
   if ( str .ne. "NO")        error stop 38
   if ( fsize(1) .ne. fsize(3)) error stop 39
   close(1, status='delete')

!* TEST4 : character

   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100)

   inquire( iolength=iol ) hvar1
   write(1, num=number, err=200) hvar1
   if ( number .ne. iol )   error stop 41

   inquire(1, access=acc, size=fsize(1), stream=str, iostat=ios, err=200)

   if ( acc .ne. "STREAM" )    error stop 42
   if ( str .ne. "YES")        error stop 43
   if ( fsize(1) .ne. number)  error stop 45
   close(1)

!* connect to the file with direct access method
   open(1, access='direct', form='unformatted',recl=10, iostat=ios, err=100)

   inquire(1, access=acc, size=fsize(2), stream=str, iostat=ios, err=200)
   if ( acc .ne. "DIRECT" )    error stop 447
   if ( str .ne. "NO")        error stop 448
   if ( fsize(1) .ne. fsize(2)) error stop 449
   close(1)

!* connect to the file with default access method(sequential)
   open(1, form='unformatted', iostat=ios, err=100)

   inquire(1, access=acc, size=fsize(3), stream=str, iostat=ios, err=200)

   if ( acc .ne. "SEQUENTIAL" )    error stop 47
   if ( str .ne. "NO")        error stop 48
   if ( fsize(1) .ne. fsize(3)) error stop 49
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

