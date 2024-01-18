! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 25, 2003
!*
!*  Primary Function Tested    : options with stream I/O
!*
!*  Description                : Test -qpostion=appendunknown option
!*
!***********************************************************************
!* in following cases, the default value for the position=specifer is
!* APPEND at the time the WRITE statment is executed :
!* 1. the status=specifer is unknown and the -qposition compiler
!*    option specifies appendunknown
!* 2. the status=specifer is old and the -qposition compiler option
!*    specifies appendold.

  integer ios
  integer fsize(2) /2*0/
  integer ivar1 /-20000000/
  integer, parameter :: ipararr(3) = (/-2147483648, 0, 2147483647/)

  complex cvar1 /(-1.175494e-38,3.402823e38)/
  complex, parameter :: cpararr(3) = (/(-3.402823e38,3.402823e38), &
 & (0.0e0, -1.0e-1), (1.175494e-38,-1.175494e-38)/)

  character hvar1 /'a'/
  character, parameter :: hpararr(3) = (/'x','1','z'/)

  byte bvar1 /'i'/
  byte, parameter :: bpararr(3) = (/'1', '2', 't'/)

!* TEST1 : integer
   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100)
   write(1, iostat=ios, err=200) ivar1, ipararr
   inquire(1, size=fsize(1), iostat=ios, err=300)
   close(1)

@PROCESS position(appendunknown)
   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100, status='unknown')
   write(1, iostat=ios, err=200) ivar1, ipararr

@PROCESS position(appendold)
   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100, status='old')
   write(1, iostat=ios, err=200) ivar1, ipararr
   inquire(1, size=fsize(2), iostat=ios, err=300)

   print *, fsize(1), fsize(2)
   if (fsize(2) .ne. 3*fsize(1))     error stop 111
   close(1, status='delete')

!* TEST2 : complex

   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100)
   write(1, iostat=ios, err=200) cvar1, cpararr
   inquire(1, size=fsize(1), iostat=ios, err=300)
   close(1)

@PROCESS position(appendunknown)
   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100, status='unknown')
   write(1, iostat=ios, err=200) cvar1, cpararr

@PROCESS position(appendold)
   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100, status='old')
   write(1, iostat=ios, err=200) cvar1, cpararr
   inquire(1, size=fsize(2), iostat=ios, err=300)

   if (fsize(2) .ne. 3*fsize(1))     error stop 21
   close(1, status='delete')

!* TEST3 : character

   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100)
   write(1, iostat=ios, err=200) hvar1, hpararr
   inquire(1, size=fsize(1), iostat=ios, err=300)
   close(1)

@PROCESS position(appendunknown)
   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100, status='unknown')
   write(1, iostat=ios, err=200) hvar1, hpararr
   inquire(1, size=fsize(2), iostat=ios, err=300)

@PROCESS position(appendold)
   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100, status='old')
   write(1, iostat=ios, err=200) hvar1, hpararr
   inquire(1, size=fsize(2), iostat=ios, err=300)

   if (fsize(2) .ne. 3*fsize(1))     error stop 31
   close(1, status='delete')

!* TEST4 : byte

   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100)
   write(1, iostat=ios, err=200) bvar1, bpararr
   inquire(1, size=fsize(1), iostat=ios, err=300)
   close(1)

@PROCESS position(appendunknown)
   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100, status='unknown')
   write(1, iostat=ios, err=200) bvar1, bpararr
   inquire(1, size=fsize(2), iostat=ios, err=300)

@PROCESS position(appendold)
   open(1, access='stream', form='unformatted', iostat=ios, &
      err=100, status='old')
   write(1, iostat=ios, err=200) bvar1, bpararr
   inquire(1, size=fsize(2), iostat=ios, err=300)

   if (fsize(2) .ne. 3*fsize(1))     error stop 41
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

