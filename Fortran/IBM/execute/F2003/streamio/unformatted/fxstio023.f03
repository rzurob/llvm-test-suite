! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 18, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Open a record file with unformatted
!*                               stream access with position=append/
!*                               rewind/asis, then read and write
!*                               some units.
!*
!=======================================================================

!* Declare Variables.

  implicit none

  integer position, filesize
  integer id(10), ios
  integer iarr1(3), iarr2(3)
  integer ivar1 /-20000000/, ivar2 , ivar3
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
      iostat=ios, err=100, status='unknown')
   write(1, iostat=ios, err=200) ivar1
   write(1, iostat=ios, err=200) ipararr
   close(1)

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='append')
   write(1, iostat=ios, err=200) ivar1
   write(1, iostat=ios, err=200) ipararr
   close(1)

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='rewind')
  inquire( 1, pos=position, size=filesize, iostat=ios, err=300)
  print *, position, filesize

   read(1, iostat=ios, err=400) ivar2
   read(1, iostat=ios, err=400) iarr1
   read(1, iostat=ios, err=400) ivar3
   read(1, iostat=ios, err=400) iarr2
  inquire( 1, pos=position, size=filesize, iostat=ios, err=300)
  print *, position, filesize

   if (ivar1 .ne. ivar2) error stop 1
   if (iarr1(1) .ne. ipararr(1)) error stop 11
   if (iarr1(2) .ne. ipararr(2)) error stop 12
   if (iarr1(3) .ne. ipararr(3)) error stop 13
   if (ivar1 .ne. ivar3) error stop 14
   if (iarr1(1) .ne. ipararr(1)) error stop 15
   if (iarr1(2) .ne. ipararr(2)) error stop 16
   if (iarr1(3) .ne. ipararr(3)) error stop 17
!  rewind(1)
  inquire( 1, pos=position, size=filesize)
  print *, position, filesize

  close(1)

  open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='asis')

  inquire( 1, pos=position, size=filesize, iostat=ios, err=300)
  print *, position, filesize

  read(1) ivar2
  read(1) iarr1
  if (ivar1 .ne. ivar2) error stop 11
  if (iarr1(1) .ne. ipararr(1)) error stop 111
  if (iarr1(2) .ne. ipararr(2)) error stop 121
  if (iarr1(3) .ne. ipararr(3)) error stop 131

   close(1, status='delete')

!* TEST2 : real
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='unknown')
   write(1, iostat=ios, err=200) rvar1
   write(1, iostat=ios, err=200) rpararr
   close(1)

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='append')
   write(1, iostat=ios, err=200) rvar1
   write(1, iostat=ios, err=200) rpararr
   close(1)

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='rewind')
   read(1, iostat=ios, err=400) rvar2
   read(1, iostat=ios, err=400) rarr1
   read(1, iostat=ios, err=400) rvar3
   read(1, iostat=ios, err=400) rarr2

   if (rvar1 .ne. rvar2) error stop 2
   if (rarr1(1) .ne. rpararr(1)) error stop 21
   if (rarr1(2) .ne. rpararr(2)) error stop 22
   if (rarr1(3) .ne. rpararr(3)) error stop 23
   if (rvar1 .ne. rvar3) error stop 24
   if (rarr2(1) .ne. rpararr(1)) error stop 25
   if (rarr2(2) .ne. rpararr(2)) error stop 26
   if (rarr2(3) .ne. rpararr(3)) error stop 27
   rewind(1, iostat=ios, err=500)
   close(1)
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='asis')
   read(1, iostat=ios, err=400) rvar2
   read(1, iostat=ios, err=400) rarr1
   if (rvar1 .ne. rvar2) error stop 22
   if (rarr1(1) .ne. rpararr(1)) error stop 212
   if (rarr1(2) .ne. rpararr(2)) error stop 222
   if (rarr1(3) .ne. rpararr(3)) error stop 232

   close(1, status='delete')

!* TEST3 : complex
   open(1, access='stream', form='unformatted', &
      iostat=ios, err=100, status='unknown')
   write(1, iostat=ios, err=200) cvar1
   write(1, iostat=ios, err=200) cpararr
   close(1)

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='append')
   write(1, iostat=ios, err=200) cvar1
   write(1, iostat=ios, err=200) cpararr
   close(1)

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='rewind')
   read(1, iostat=ios, err=400) cvar2
   read(1, iostat=ios, err=400) carr1
   read(1, iostat=ios, err=400) cvar3
   read(1, iostat=ios, err=400) carr2

   if (cvar1 .ne. cvar2) error stop 3
   if (carr1(1) .ne. cpararr(1)) error stop 31
   if (carr1(2) .ne. cpararr(2)) error stop 32
   if (carr1(3) .ne. cpararr(3)) error stop 33
   if (cvar1 .ne. cvar3) error stop 34
   if (carr2(1) .ne. cpararr(1)) error stop 35
   if (carr2(2) .ne. cpararr(2)) error stop 36
   if (carr2(3) .ne. cpararr(3)) error stop 37
   rewind(1, iostat=ios, err=500)
   close(1)
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='asis')
   read(1, iostat=ios, err=400) cvar2
   read(1, iostat=ios, err=400) carr1

   if (cvar1 .ne. cvar2) error stop 33
   if (carr1(1) .ne. cpararr(1)) error stop 313
   if (carr1(2) .ne. cpararr(2)) error stop 323
   if (carr1(3) .ne. cpararr(3)) error stop 333

   close(1, status='delete')

!* TEST4 : logical
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='unknown')
   write(1, iostat=ios, err=200) lvar1
   write(1, iostat=ios, err=200) lpararr
   close(1)

   open(1, access='stream', form='unformatted', &
      iostat=ios, err=100, status='old', position='append')
   write(1, iostat=ios, err=200) lvar1
   write(1, iostat=ios, err=200) lpararr
   close(1)

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='rewind')
   read(1, iostat=ios, err=400) lvar2
   read(1, iostat=ios, err=400) larr1
   read(1, iostat=ios, err=400) lvar3
   read(1, iostat=ios, err=400) larr2

   if (lvar1 .neqv. lvar2) error stop 4
   if (larr1(1) .neqv. lpararr(1)) error stop 41
   if (larr1(2) .neqv. lpararr(2)) error stop 42
   if (lvar1 .neqv. lvar3) error stop 43
   if (larr2(1) .neqv. lpararr(1)) error stop 44
   if (larr2(2) .neqv. lpararr(2)) error stop 45
   rewind(1, iostat=ios, err=500)
   close(1)
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='asis')
   read(1, iostat=ios, err=400) lvar2
   read(1, iostat=ios, err=400) larr1
   if (lvar1 .neqv. lvar2) error stop 44
   if (larr1(1) .neqv. lpararr(1)) error stop 414
   if (larr1(2) .neqv. lpararr(2)) error stop 424

   close(1, status='delete')

!* TEST5 : character
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='unknown')
   write(1, iostat=ios, err=200) hvar1
   write(1, iostat=ios, err=200) hpararr
   close(1)

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='append')
   write(1, iostat=ios, err=200) hvar1
   write(1, iostat=ios, err=200) hpararr
   close(1)

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='rewind')
   read(1, iostat=ios, err=400) hvar2
   read(1, iostat=ios, err=400) harr1
   read(1, iostat=ios, err=400) hvar3
   read(1, iostat=ios, err=400) harr2

   if (hvar1 .ne. hvar2) error stop 5
   if (harr1(1) .ne. hpararr(1)) error stop 51
   if (harr1(2) .ne. hpararr(2)) error stop 52
   if (harr1(3) .ne. hpararr(3)) error stop 53
   if (hvar1 .ne. hvar3) error stop 54
   if (harr2(1) .ne. hpararr(1)) error stop 55
   if (harr2(2) .ne. hpararr(2)) error stop 56
   if (harr2(3) .ne. hpararr(3)) error stop 57
   rewind(1, iostat=ios, err=500)
   close(1)
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='asis')
   read(1, iostat=ios, err=400) hvar2
   read(1, iostat=ios, err=400) harr1
   if (hvar1 .ne. hvar2) error stop 55
   if (harr1(1) .ne. hpararr(1)) error stop 515
   if (harr1(2) .ne. hpararr(2)) error stop 525
   if (harr1(3) .ne. hpararr(3)) error stop 535

   close(1, status='delete')

!* TEST6 : byte

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='unknown')
   write(1, iostat=ios, err=200) bvar1
   write(1, iostat=ios, err=200) bpararr
   close(1)

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='append')
   write(1, iostat=ios, err=200) bvar1
   write(1, iostat=ios, err=200) bpararr
   close(1)

   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='rewind')
   read(1, iostat=ios, err=400 ) bvar2
   read(1, iostat=ios, err=400) barr1
   read(1, iostat=ios, err=400) bvar3
   read(1, iostat=ios, err=400) barr2

   if (bvar1 .ne. bvar2) error stop 6
   if (barr1(1) .ne. bpararr(1)) error stop 61
   if (barr1(2) .ne. bpararr(2)) error stop 62
   if (barr1(3) .ne. bpararr(3)) error stop 63
   if (bvar1 .ne. bvar3) error stop 64
   if (barr2(1) .ne. bpararr(1)) error stop 65
   if (barr2(2) .ne. bpararr(2)) error stop 66
   if (barr2(3) .ne. bpararr(3)) error stop 67

   rewind(1, iostat=ios, err=500)
   close(1)
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='asis')
   read(1, iostat=ios, err=400) bvar2
   read(1, iostat=ios, err=400) barr1
   if (bvar1 .ne. bvar2) error stop 66
   if (barr1(1) .ne. bpararr(1)) error stop 616
   if (barr1(2) .ne. bpararr(2)) error stop 626
   if (barr1(3) .ne. bpararr(3)) error stop 636

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

