! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:
! %GROUP: fxstio008.f
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
!*  Description                : Test intrinsic data types with
!*                               parameter attribute
!*
!=======================================================================

!* Declare Variables.

  implicit none

  integer id(2), ios
  integer iarr(3)
  integer ivar1 /-20000000/, ivar2
  integer, parameter :: ipararr(3) = (/-2147483648, 0, 2147483647/)

  real rarr(3)
  real rvar1 /-1.2e-30/, rvar2
  real, parameter :: rpararr(3) = (/-1.175494e-38, 0.0e0, 3.402823e38/)

  complex carr(3)
  complex cvar1 /(-1.175494e-38,3.402823e38)/, cvar2
  complex, parameter :: cpararr(3) = (/(-3.402823e38,3.402823e38), &
 & (0.0e0, -1.0e-1), (1.175494e-38,-1.175494e-38)/)

  logical larr(2)
  logical lvar1 /.true./, lvar2
  logical, parameter :: lpararr(2) = (/.true.,.false./)

  character harr(3)
  character hvar1 /'a'/, hvar2
  character, parameter :: hpararr(3) = (/'x','1','z'/)

  byte barr(3)
  byte bvar1 /'i'/, bvar2
  byte, parameter :: bpararr(3) = (/'1', '2', 't'/)

!* TEST1 : integer

   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100)
   write(1, id=id(1), iostat=ios, err=200) ivar1
   write(1, id=id(2), iostat=ios, err=200) ipararr

   wait(id =id(1))
   wait(id =id(2))
   rewind(1, iostat=ios, err=500)

   read(1, id=id(1), iostat=ios, err=400) ivar2
   read(1, id=id(2), iostat=ios, err=400) iarr

   wait(id = id(1))
   if (ivar1 .ne. ivar2) error stop 10
   wait(id = id(2))
   if (iarr(1) .ne. ipararr(1)) error stop 11
   if (iarr(2) .ne. ipararr(2)) error stop 12
   if (iarr(3) .ne. ipararr(3)) error stop 13

   close(1, status='delete')

!* TEST2 : real
   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100)
   write(1, id = id(1), iostat=ios, err=200) rvar1
   write(1, id = id(2), iostat=ios, err=200) rpararr
   wait(id =id(1))
   wait(id =id(2))
   rewind(1, iostat=ios, err=500)

   read(1, id=id(1), iostat=ios, err=400) rvar2
   read(1, id=id(2), iostat=ios, err=400) rarr

   wait(id = id(1))
   if (rvar1 .ne. rvar2) error stop 2
   wait(id = id(2))
   if (rarr(1) .ne. rpararr(1)) error stop 21
   if (rarr(2) .ne. rpararr(2)) error stop 22
   if (rarr(3) .ne. rpararr(3)) error stop 23

   close(1, status='delete')

!* TEST3 : complex
   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100)
   write(1, id=id(1), iostat=ios, err=200) cvar1
   write(1, id=id(2), iostat=ios, err=200) cpararr

   wait(id =id(1))
   wait(id =id(2))
   rewind(1, iostat=ios, err=500)

   read(1, id=id(1), iostat=ios, err=400) cvar2
   read(1, id=id(2), iostat=ios, err=400) carr

   wait(id = id(1))
   if (cvar1 .ne. cvar2) error stop 3
   wait(id = id(2))
   if (carr(1) .ne. cpararr(1)) error stop 31
   if (carr(2) .ne. cpararr(2)) error stop 32
   if (carr(3) .ne. cpararr(3)) error stop 33

   close(1, status='delete')

!* TEST4 : logical
   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100)
   write(1, id=id(1), iostat=ios, err=200) lvar1
   write(1, id=id(2), iostat=ios, err=200) lpararr

   wait(id =id(1))
   wait(id =id(2))
   rewind(1, iostat=ios, err=500)

   read(1, id=id(1), iostat=ios, err=400) lvar2
   read(1, id=id(2), iostat=ios, err=400) larr

   wait(id = id(1))
   if (lvar1 .neqv. lvar2) error stop 4
   wait(id = id(2))
   if (larr(1) .neqv. lpararr(1)) error stop 41
   if (larr(2) .neqv. lpararr(2)) error stop 42
   close(1, status='delete')

!* TEST5 : character
   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100)
   write(1, id=id(1), iostat=ios, err=200) hvar1
   write(1, id=id(2), iostat=ios, err=200) hpararr

   wait(id =id(1))
   wait(id =id(2))
   rewind(1, iostat=ios, err=500)

   read(1, id=id(1), iostat=ios, err=400) hvar2
   read(1, id=id(2), iostat=ios, err=400) harr

   wait(id = id(1))
!  print *, hvar1, havr2
   if (hvar1 .ne. hvar2) error stop 5
   wait(id = id(2))
   if (harr(1) .ne. hpararr(1)) error stop 51
   if (harr(2) .ne. hpararr(2)) error stop 52
   if (harr(3) .ne. hpararr(3)) error stop 53
   close(1, status='delete')

!* TEST6 : byte

   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100)
   write(1, id=id(1), iostat=ios, err=200) bvar1
   write(1, id=id(2), iostat=ios, err=200) bpararr

   wait(id =id(1))
   wait(id =id(2))
   rewind(1, iostat=ios, err=500)

   read(1, id=id(1), iostat=ios, err=400) bvar2
   read(1, id=id(2), iostat=ios, err=400) barr

   wait(id = id(1))
   if (bvar1 .ne. bvar2) error stop 10
   wait(id = id(2))
   if (barr(1) .ne. bpararr(1)) error stop 11
   if (barr(2) .ne. bpararr(2)) error stop 12
   if (barr(3) .ne. bpararr(3)) error stop 13

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

