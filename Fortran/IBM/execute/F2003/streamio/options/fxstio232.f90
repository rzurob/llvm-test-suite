! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.* ; export XLFRTEOPTS=multconn=yes
! %COMPOPTS:
! %GROUP: fxstio232.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : April 1, 2003
!*
!*  Primary Function Tested    : multconnect run-time option
!*
!*  Description                : Test stream access I/O with multconnect
!*                               run-time option enable.
!*
!=======================================================================

!* Declare Variables.

   integer ios
   integer filesize(2) /2*0/
   integer ivar1 /2000/, ivar2, ivar3
   integer, parameter :: ipararr(3) = (/2000, 0, 2000/)

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
   open(1, file='int.dat', access='stream', form='unformatted', iostat=ios,&
      err=100)
   write(1, iostat=ios, err=200) ivar1, ipararr
   rewind(1, iostat=ios, err=500)
   read(1, iostat=ios, err=400) ivar2
   inquire(1, access=acc, size=filesize(1), stream=str, iostat=ios, err=300)
   if ( acc .ne. "STREAM" )    error stop 12
   if ( str .ne. "YES")        error stop 13
   call flush_(1)

!* connect to the file with unit 2
   open(2, file='int.dat', access='stream', form='unformatted', iostat=ios,&
      err=100)
   rewind(2, iostat=ios, err=500)
   read(2, iostat=ios, err=400) ivar3
   inquire(2, size=filesize(2), iostat=ios, err=300)
   if ( ivar2 .ne. ivar3 )         error stop 16
   if ( filesize(1) .ne. filesize(2)) error stop 19

   close(1, status='delete')

!* real
   open(1, file='real.dat', access='stream', form='unformatted', iostat=ios,&
      err=100)
   write(1, iostat=ios, err=200) rvar1, rpararr
   rewind(1, iostat=ios, err=500)
   read(1, iostat=ios, err=400) rvar2
   inquire(1, access=acc, size=filesize(1), stream=str, iostat=ios, err=300)
   if ( acc .ne. "STREAM" )    error stop 22
   if ( str .ne. "YES")        error stop 23
   call flush_(1)

!* connect to the file with unit 2
   open(2, file='real.dat', access='stream', form='unformatted', iostat=ios,&
      err=100)
   rewind(2, iostat=ios, err=500)
   read(2, iostat=ios, err=400) rvar3
   inquire(2, size=filesize(2), iostat=ios, err=300)
   if ( rvar2 .ne. rvar3 )         error stop 26
   if ( filesize(1) .ne. filesize(2)) error stop 29

   close(1, status='delete')

!* logical

   open(1, file='log.dat', access='stream', form='unformatted', iostat=ios,&
      err=100)
   write(1, iostat=ios, err=200) lvar1, lpararr
   rewind(1, iostat=ios, err=500)
   read(1, iostat=ios, err=400) lvar2
   inquire(1, access=acc, size=filesize(1), stream=str, iostat=ios, err=300)
   if ( acc .ne. "STREAM" )    error stop 32
   if ( str .ne. "YES")        error stop 33
   call flush_(1)

!* connect to the file with unit 2
   open(2, file='log.dat', access='stream', form='unformatted', &
      iostat=ios, err=100)
   rewind(2, iostat=ios, err=500)
   read(2, iostat=ios, err=400) lvar3
   inquire(2, size=filesize(2), iostat=ios, err=300)
   if ( lvar2 .neqv. lvar3 )         error stop 36
   if ( filesize(1) .ne. filesize(2)) error stop 39

   close(1, status='delete')

!* TEST4: byte

   open(1, file='byt.dat', access='stream', form='unformatted', iostat=ios,&
      err=100)
   write(1, iostat=ios, err=200) bvar1, bpararr
   rewind(1, iostat=ios, err=500)
   read(1, iostat=ios, err=400) bvar2
   inquire(1, access=acc, size=filesize(1), stream=str, iostat=ios, err=300)
   if ( acc .ne. "STREAM" )    error stop 42
   if ( str .ne. "YES")        error stop 43
   call flush_(1)

!* connect to the file with unit 2
   open(2, file='byt.dat', access='stream', form='unformatted', &
      iostat=ios, err=100)
   rewind(2, iostat=ios, err=500)
   read(2, iostat=ios, err=400) bvar3
   inquire(2, size=filesize(2), iostat=ios, err=300)
   if ( bvar2 .ne. bvar3 )         error stop 46
   if ( filesize(1) .ne. filesize(2)) error stop 49

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

