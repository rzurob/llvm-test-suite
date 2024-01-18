! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:
! %GROUP: fxstio006.f
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
!*  Description                : Test Integer pointers with
!*                               Unformatted Synchronous Stream I/O.
!*
!=======================================================================

!* Declare pointer based and local scalar variables.

   implicit none

   integer ios
   integer     var1 /2147483647/ , var11
   real        var2 /3.402823E+38/ ,var21
   complex     var3 /(3.4E30, 0.1E-1)/ , var31
   character   var4 /'a'/, var41
   logical     var5 /.true./, var51
   byte        var6 /48/, var61
   pointer (p1,var11),(p2,var21),(p3,var31),(p4,var41), &
           (p5,var51),(p6,var61)

!* initialize pointers
   p1 = loc(var1)
   p2 = loc(var2)
   p3 = loc(var3)
   p4 = loc(var4)
   p5 = loc(var5)
   p6 = loc(var6)

!* start of tests

  open(1, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
   write(1 , iostat=ios, err=200) p1
   rewind(1, iostat=ios, err=500)
   read (1, iostat=ios, err=400) var11

   if (var1 .ne. var11) error stop 111
   close(1, status='delete')

  open(1, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
   write(1 , iostat=ios, err=200) p2
   rewind(1, iostat=ios, err=500)
   read (1, iostat=ios, err=400) var21

   if (var1 .ne. var11) error stop 2
   close(1, status='delete')

  open(1, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
   write(1 , iostat=ios, err=200) p3
   rewind(1, iostat=ios, err=500)
   read (1, iostat=ios, err=400) var31

   if (var3 .ne. var31) error stop 3
   close(1, status='delete')

   open(1, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
   write(1 , iostat=ios, err=200) p4
   rewind(1, iostat=ios, err=500)
   read (1, iostat=ios, err=400) var41

   if (var4 .ne. var41) error stop 4
   close(1, status='delete')

  open(1, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
   write(1 , iostat=ios, err=200) p5
   rewind(1, iostat=ios, err=500)
   read (1, iostat=ios, err=400) var51

   if (var5 .neqv. var51) error stop 5
   close(1, status='delete')

  open(1, access='stream', form='unformatted', action='readwrite', &
     iostat=ios, err=100)
   write(1 , iostat=ios, err=200) p6
   rewind(1, iostat=ios, err=500)
   read (1, iostat=ios, err=400) var61

   if (var6 .ne. var61) error stop 6
   close(1, status='delete')

stop

100 print *, "open error: iostat = ", ios
    error stop 100
200 print *, "write error: iostat = ", ios
    error stop 200
400 print *, "read error: iostat = ", ios
    error stop 400
500 print *, "rewind error: iostat = ", ios
    error stop 500
end

