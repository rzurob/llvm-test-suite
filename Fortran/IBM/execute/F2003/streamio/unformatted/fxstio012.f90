! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:  rm -f fort.*
! %COMPOPTS: -qposition=appendold 
! %GROUP: fxstio012.f 
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : fxstio012.f 
!*
!*  PROGRAMMER                 : Catherine Sun
!*  
!*  Creation Date              : Mar 07, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test Unformatted Stream I/O on an OLD file 
!*                               created with Direct access I/O
!*
!=======================================================================

!* Declare Variables.
   
  implicit none

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

   open(1, access='direct', form='unformatted', asynch='yes', &
      recl=100000, iostat=ios, err=100, status='unknown')
   write(1, id=id(1), iostat=ios, err=200, rec=1) ivar1
   write(1, id=id(2), iostat=ios, err=200, rec=2) ipararr
   
   wait(id =id(1))
   wait(id =id(2))
   close(1)

   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100, status='old', position='append')

   write(1,id=id(3), iostat=ios, err=200) ivar1
   write(1,id=id(4), iostat=ios, err=200) ipararr

   wait(id =id(3))
   wait(id =id(4))

!   rewind(1, iostat=ios, err=500)
   read(1, id=id(5), iostat=ios, err=400, POS=1) ivar2
   read(1, id=id(6), iostat=ios, err=400, POS=100001) iarr1
   read(1, id=id(7), iostat=ios, err=400, POS=200001) ivar3
   read(1, id=id(8), iostat=ios, err=400) iarr2

   wait(id = id(5))
   if (ivar1 .ne. ivar2) error stop 10

   wait(id = id(6))
   if (iarr1(1) .ne. ipararr(1)) error stop 11
   if (iarr1(2) .ne. ipararr(2)) error stop 12
   if (iarr1(3) .ne. ipararr(3)) error stop 13

   wait(id = id(7))
   if (ivar1 .ne. ivar3) error stop 14

   wait(id = id(8))
   if (iarr1(1) .ne. ipararr(1)) error stop 15
   if (iarr1(2) .ne. ipararr(2)) error stop 16
   if (iarr1(3) .ne. ipararr(3)) error stop 17

   close(1, status='delete')

!* TEST2 : real

   open(1, access='direct', form='unformatted', asynch='yes', &
      recl=100000, iostat=ios, err=100, status='unknown')

   write(1, id = id(1), iostat=ios, err=200, rec=1) rvar1
   write(1, id = id(2), iostat=ios, err=200, rec=2) rpararr
   wait(id =id(1))
   wait(id =id(2))
   close(1)

   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100, status='old', position='append')

   write(1, id = id(3), iostat=ios, err=200) rvar1
   write(1, id = id(4), iostat=ios, err=200) rpararr
   wait(id =id(3))
   wait(id =id(4))

   rewind(1, iostat=ios, err=500)
   read(1, id=id(5), iostat=ios, err=400, POS=1) rvar2
   read(1, id=id(6), iostat=ios, err=400, POS=100001) rarr1
   read(1, id=id(7), iostat=ios, err=400, POS=200001) rvar3
   read(1, id=id(8), iostat=ios, err=400) rarr2

   wait(id = id(5))
   if (rvar1 .ne. rvar2) error stop 20

   wait(id = id(6))
   if (rarr1(1) .ne. rpararr(1)) error stop 21
   if (rarr1(2) .ne. rpararr(2)) error stop 22
   if (rarr1(3) .ne. rpararr(3)) error stop 23

   wait(id = id(7))
   if (rvar1 .ne. rvar3) error stop 24

   wait(id = id(8))
   if (rarr2(1) .ne. rpararr(1)) error stop 25
   if (rarr2(2) .ne. rpararr(2)) error stop 26
   if (rarr2(3) .ne. rpararr(3)) error stop 27

   close(1, status='delete')

!* TEST3 : complex
   open(1, access='direct', form='unformatted', asynch='yes',&
      recl=100000, iostat=ios, err=100, status='unknown')
   write(1, id=id(1), iostat=ios, err=200, rec=1) cvar1
   write(1, id=id(2), iostat=ios, err=200, rec=2) cpararr
   wait(id =id(1))
   wait(id =id(2))
   close(1)

   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100, status='old', position='append')
   write(1, id=id(3), iostat=ios, err=200) cvar1
   write(1, id=id(4), iostat=ios, err=200) cpararr
   wait(id =id(3))
   wait(id =id(4))

   rewind(1, iostat=ios, err=500)
   read(1, id=id(5), iostat=ios, err=400) cvar2
   read(1, id=id(6), iostat=ios, err=400, POS=100001) carr1
   read(1, id=id(7), iostat=ios, err=400, POS=200001) cvar3
   read(1, id=id(8), iostat=ios, err=400) carr2

   wait(id = id(5))
   if (cvar1 .ne. cvar2) error stop 3

   wait(id = id(6))
   if (carr1(1) .ne. cpararr(1)) error stop 31
   if (carr1(2) .ne. cpararr(2)) error stop 32
   if (carr1(3) .ne. cpararr(3)) error stop 33

   wait(id = id(7))
   if (cvar1 .ne. cvar3) error stop 34

   wait(id = id(8))
   if (carr2(1) .ne. cpararr(1)) error stop 35
   if (carr2(2) .ne. cpararr(2)) error stop 36
   if (carr2(3) .ne. cpararr(3)) error stop 37

   close(1, status='delete')


!* TEST4 : logical
   open(1, access='direct', form='unformatted', asynch='yes', &
      recl=100000, iostat=ios, err=100, status='unknown')
   write(1, id=id(1), iostat=ios, err=200, rec=1) lvar1
   write(1, id=id(2), iostat=ios, err=200, rec=2) lpararr

   wait(id =id(1))
   wait(id =id(2))
   close(1)

   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100, status='old', position='append')
   write(1,id=id(3), iostat=ios, err=200) lvar1
   write(1,id=id(4), iostat=ios, err=200) lpararr
   wait(id =id(3))
   wait(id =id(4))

!   rewind(1, iostat=ios, err=500)
   read(1, id=id(5), iostat=ios, err=400, POS=1) lvar2
   read(1, id=id(6), iostat=ios, err=400, POS=100001) larr1
   read(1, id=id(7), iostat=ios, err=400, POS=200001) lvar3
   read(1, id=id(8), iostat=ios, err=400) larr2

   wait(id = id(5))
   if (lvar1 .neqv. lvar2) error stop 4

   wait(id = id(6))
   if (larr1(1) .neqv. lpararr(1)) error stop 41
   if (larr1(2) .neqv. lpararr(2)) error stop 42

   wait(id = id(7))
   if (lvar1 .neqv. lvar3) error stop 43

   wait(id = id(8))
   if (larr2(1) .neqv. lpararr(1)) error stop 44
   if (larr2(2) .neqv. lpararr(2)) error stop 45

   close(1, status='delete')


!* TEST5 : character

   open(1, access='direct', form='unformatted', asynch='yes', &
      recl=100000, iostat=ios, err=100, status='unknown')
   write(1, id=id(1), iostat=ios, err=200, rec=1) hvar1
   write(1, id=id(2), iostat=ios, err=200, rec=2) hpararr

   wait(id =id(1))
   wait(id =id(2))
   close(1)

   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100, status='old', position='append')
   write(1,id=id(3), iostat=ios, err=200) hvar1
   write(1,id=id(4), iostat=ios, err=200) hpararr
   wait(id =id(3))
   wait(id =id(4))

   rewind(1, iostat=ios, err=500)
   read(1, id=id(5), iostat=ios, err=400) hvar2
   read(1, id=id(6), iostat=ios, err=400, POS=100001) harr1
   read(1, id=id(7), iostat=ios, err=400, POS=200001) hvar3
   read(1, id=id(8), iostat=ios, err=400) harr2

   wait(id = id(5))
!  print *, hvar1, havr2
   if (hvar1 .ne. hvar2) error stop 5

   wait(id = id(6))
   if (harr1(1) .ne. hpararr(1)) error stop 51
   if (harr1(2) .ne. hpararr(2)) error stop 52
   if (harr1(3) .ne. hpararr(3)) error stop 53

   wait(id = id(7))
!  print *, hvar1, havr2
   if (hvar1 .ne. hvar3) error stop 54

   wait(id = id(8))
   if (harr2(1) .ne. hpararr(1)) error stop 55
   if (harr2(2) .ne. hpararr(2)) error stop 56
   if (harr2(3) .ne. hpararr(3)) error stop 57

   close(1, status='delete') 

!* TEST6 : byte

   open(1, access='direct', form='unformatted', asynch='yes', &
      recl=100000, iostat=ios, err=100, status='unknown')
   write(1, id=id(1), iostat=ios, err=200, rec=1) bvar1
   write(1, id=id(2), iostat=ios, err=200, rec=2) bpararr

   wait(id =id(1))
   wait(id =id(2))
   close(1)

   open(1, access='stream', form='unformatted', asynch='yes', &
      iostat=ios, err=100, status='old', position='append')
   write(1,id=id(3), iostat=ios, err=200) bvar1
   write(1,id=id(4), iostat=ios, err=200) bpararr

   wait(id =id(3))
   wait(id =id(4))

   rewind(1, iostat=ios, err=500)
   read(1, id=id(5), iostat=ios, err=400, POS=1) bvar2
   read(1, id=id(6), iostat=ios, err=400, POS=100001) barr1
   read(1, id=id(7), iostat=ios, err=400, POS=200001) bvar3
   read(1, id=id(8), iostat=ios, err=400) barr2

   wait(id = id(5))
   if (bvar1 .ne. bvar2) error stop 6

   wait(id = id(6))
   if (barr1(1) .ne. bpararr(1)) error stop 61
   if (barr1(2) .ne. bpararr(2)) error stop 62
   if (barr1(3) .ne. bpararr(3)) error stop 63

   wait(id = id(7))
   if (bvar1 .ne. bvar3) error stop 64

   wait(id = id(8))
   if (barr2(1) .ne. bpararr(1)) error stop 65
   if (barr2(2) .ne. bpararr(2)) error stop 66
   if (barr2(3) .ne. bpararr(3)) error stop 67

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

