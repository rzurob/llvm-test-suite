! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:
! %GROUP: fxstio306.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 31, 2003
!*
!*  Primary Function Tested    : Size of the files with holes inside
!*
!*  Description                : Test size of the stream files with
!*                               holes inside by using the inquire
!*                               statment.
!*
!=======================================================================

!* Declare Variables.

   integer ios
   integer posi /1024/, fsize(20) /20*0/
   integer iol /0/
   integer ivar1 /2000/
   complex*8 cvar1 /(-1.175494e-38,3.402823e38)/
   character*1 hvar1 /'a'/
   byte bvar1 /'i'/

!* TEST1 : integer
   open(1, access='stream', form='unformatted', iostat=ios,&
      err=100)
   inquire( iolength=iol ) ivar1
   write(1, iostat=ios, err=200) ivar1

   inquire(1, size=fsize(1), iostat=ios, err=300)

!* creat a hole with 1020 bytes in the file, the file will occupy
!* 2 blocks after the write
   write(1, pos=posi, iostat=ios, err=200) ivar1

   inquire(1, size=fsize(2), iostat=ios, err=300)
!* current file size equals to (fsize(1)+1020-1+iol) or (posi-1+iol)

   if(fsize(1) .ne. iol)          error stop 111
   if(fsize(2) .ne. (posi-1+iol))   error stop 12
   print *, fsize(2)
   close(1, status='keep')

!* open the file with formatted I/O access
   open(1, access='stream', form='formatted', iostat=ios,&
      err=100)
   write(1, fmt='(I7)', pos=posi, iostat=ios, err=200) ivar1
   inquire(1, size=fsize(3), iostat=ios, err=300)
   print *, fsize(2), fsize(3)
   if((fsize(2)+iol) .ne. fsize(3))   error stop 13

! TEST2 : complex
   open(2, access='stream', form='unformatted', iostat=ios,&
      err=100)
   inquire( iolength=iol ) cvar1
   write(2, iostat=ios, err=200) cvar1

   inquire(2, size=fsize(4), iostat=ios, err=300)

!* creat a hole with 1020 bytes in the file, the file will occupy
!* 2 blocks after the write
   write(2, pos=posi, iostat=ios, err=200) cvar1

   inquire(2, size=fsize(5), iostat=ios, err=300)

   print *, fsize(4), iol
   if(fsize(4) .ne. iol)          error stop 21
   if(fsize(5) .ne. (posi-1+iol))   error stop 22
   print *, fsize(4)
   close(2, status='keep')

!* open the file with formatted I/O access
   open(2, access='stream', form='formatted', iostat=ios,&
      err=100)
   write(2, fmt='(2E15.7)', pos=posi, iostat=ios, err=200) ivar1
   inquire(2, size=fsize(6), iostat=ios, err=300)
   print *, fsize(2), fsize(3)
   if((fsize(5)+iol) .ne. fsize(6))   error stop 23

!* TEST3: character
   open(3, access='stream', form='unformatted', iostat=ios,&
      err=100)
   inquire( iolength=iol ) hvar1
   write(3, iostat=ios, err=200) hvar1
   inquire(3, size=fsize(7), iostat=ios, err=300)

!* creat a hole with 1020 bytes in the file, the file will occupy
!* 2 blocks after the write
   write(3, pos=posi, iostat=ios, err=200) hvar1

   inquire(3, size=fsize(8), iostat=ios, err=300)

   if(fsize(7) .ne. iol)            error stop 31
   if(fsize(8) .ne. (posi-1+iol))   error stop 32
   close(3, status='keep')
!* open the file with formatted I/O access
   open(3, access='stream', form='formatted', iostat=ios,&
      err=100)
   write(3, fmt='(A1)', pos=posi, iostat=ios, err=200) hvar1
   inquire(3, size=fsize(9), iostat=ios, err=300)
   if((fsize(8)+iol) .ne. fsize(9))   error stop 33

stop
100 print *, "open error: iostat = ", ios
    error stop 100
200 print *, "write error: iostat = ", ios
    error stop 200
300 print *, "inquire error: iostat = ", ios
    error stop 300
end

