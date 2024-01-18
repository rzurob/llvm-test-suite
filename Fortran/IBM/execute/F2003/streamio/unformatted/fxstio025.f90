! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f  fort.*
! %COMPOPTS:
! %GROUP: fxstio025.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 19, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test NUM in unformatted stream
!*                               access READ/WRITE statements.
!*                               Default integer will be used to
!*                               test this specifier.
!*
!=======================================================================

!* Declare Variables.

   implicit none

   integer position /0/, filesize /0/
   integer number /0/, iol /0/
   integer iarr1(3)
   integer ivar1 /-20000000/, ivar2
   integer, parameter :: ipararr(3) = (/-2147483648, 0, 2147483647/)

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

   byte barr1(3), barr2(3)
   byte bvar1 /'i'/, bvar2, bvar3
   byte, parameter :: bpararr(3) = (/'1', '2', 't'/)

!* TEST1 : integer

   open(1, access='stream', form='unformatted', &
      err=100, status='unknown')

   inquire( iolength=iol ) ivar1
   write(1, num=number, err=200) ivar1
   if ( number .ne. iol )   error stop 11

   inquire( iolength=iol ) ipararr
   write(1, num=number, err=200) ipararr
   if ( number .ne. iol )   error stop 12

   rewind(1)
   inquire( iolength=iol ) ivar2
   read(1, num=number, err=300) ivar2
   if ( number .ne. iol )   error stop 13

   inquire( iolength=iol ) iarr1
   read(1, num=number, err=300) iarr1
   if ( number .ne. iol)    error stop 14

   if (ivar1 .ne. ivar2) error stop 15
   if (iarr1(1) .ne. ipararr(1)) error stop 16
   if (iarr1(2) .ne. ipararr(2)) error stop 17
   if (iarr1(3) .ne. ipararr(3)) error stop 18

   close(1, status='delete')

!* TEST2 : real

   open(1, access='stream', form='unformatted', &
      err=100, status='unknown')

   inquire( iolength=iol ) rvar1
   write(1, num=number, err=200) rvar1
   if ( number .ne. iol )   error stop 21

   inquire( iolength=iol ) ipararr
   write(1, num=number, err=200) rpararr
   if ( number .ne. iol )   error stop 22

   rewind(1)
   inquire( iolength=iol ) rvar2
   read(1, num=number, err=300) rvar2
   if ( number .ne. iol )   error stop 23

   inquire( iolength=iol ) rarr1
   read(1, num=number, err=300) rarr1
   if ( number .ne. iol)    error stop 24

   if (rvar1 .ne. rvar2) error stop 25
   if (rarr1(1) .ne. rpararr(1)) error stop 26
   if (rarr1(2) .ne. rpararr(2)) error stop 27
   if (rarr1(3) .ne. rpararr(3)) error stop 28

!* TEST3 : logical

   open(1, access='stream', form='unformatted', &
      err=100, status='unknown')

   inquire( iolength=iol ) lvar1
   write(1, num=number, err=200) lvar1
   if ( number .ne. iol )   error stop 31

   inquire( iolength=iol ) lpararr
   write(1, num=number, err=200) lpararr
   if ( number .ne. iol )   error stop 32

   rewind(1)
   inquire( iolength=iol ) lvar2
   read(1, num=number, err=300) lvar2
   if ( number .ne. iol )   error stop 33

   inquire( iolength=iol ) larr1
   read(1, num=number, err=300) larr1
   if ( number .ne. iol)    error stop 34

   if (lvar1 .neqv. lvar2) error stop 35
   if (larr1(1) .neqv. lpararr(1)) error stop 36
   if (larr1(2) .neqv. lpararr(2)) error stop 37

   close(1, status='delete')

!* TEST4 : character

   open(1, access='stream', form='unformatted', &
      err=100, status='unknown')

   inquire( iolength=iol ) hvar1
   write(1, num=number, err=200) hvar1
   if ( number .ne. iol )   error stop 41

   inquire( iolength=iol ) hpararr
   write(1, num=number, err=200) hpararr
   if ( number .ne. iol )   error stop 42

   rewind(1)
   inquire( iolength=iol ) hvar2
   read(1, num=number, err=300) hvar2
   if ( number .ne. iol )   error stop 43

   inquire( iolength=iol ) harr1
   read(1, num=number, err=300) harr1
   if ( number .ne. iol)    error stop 44

   if (hvar1 .ne. hvar2) error stop 45
   if (harr1(1) .ne. hpararr(1)) error stop 46
   if (harr1(2) .ne. hpararr(2)) error stop 47
   if (harr1(3) .ne. hpararr(3)) error stop 48

   close(1, status='delete')

stop

100 print *, "open error."
    error stop 100
200 print *, "inquire error."
    error stop 200
300 print *, "read error."
    error stop 300
end

