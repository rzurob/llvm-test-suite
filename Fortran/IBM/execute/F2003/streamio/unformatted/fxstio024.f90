! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.* 
! %COMPOPTS: 
! %GROUP: fxstio024.f   
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
!*  TEST CASE TITLE            : fxstio024.f 
!*
!*  PROGRAMMER                 : Catherine Sun
!*  
!*  Creation Date              : Mar 19, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Open a record file with unformatted
!*                               stream access. Test endfile & rewind
!*                               statement as well as end-of-file condition.
!*
!=======================================================================

!* Declare Variables.
    
  integer ios 
  integer filesize /0/, filesize1 /0/, position /0/, poin /0/
  integer ivar1 /2000/, ivar2 
  integer,parameter :: ipararr(3) = (/2147, 0000, 2147/)

  real rvar1 /-1.2e-30/, rvar2, rvar3
  real, parameter :: rpararr(3) = (/-1.175494e-38, 0.0e0, 3.402823e38/)

  logical lvar1 /.true./, lvar2
  logical, parameter :: lpararr(2) = (/.true.,.false./)

  character hvar1 /'a'/, hvar2
  character, parameter :: hpararr(3) = (/'x','1','z'/)
  character*1  char1

!* TEST1 : integer

!* open an unit with formatted stream I/O 
   open(1, access='stream', form='formatted', &
      iostat=ios, err=100, status='unknown')
   write(1, fmt='(I10)', iostat=ios, err=200) ivar1
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   filesize1 = filesize
   print *, position, filesize
   close(1)

!* open the same unit with unformatted stream I/O
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='append')
   write(1, iostat=ios, err=200) ipararr

!* test rewind statments
   rewind(1, iostat=ios, err=500)
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   print *, position, filesize
   if(position .ne. 1) error stop 1
   read(1, iostat=ios, err=400)  ivar2

!* test end-of-record symbol
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   print *, position, filesize
   read (1, pos=11, iostat=ios, err=400) char1
   print *, char1
   if ( char1 .ne. "\n" )    error stop 12

!* test endfile statment
   endfile(1)
   inquire( 1,pos=position, size=filesize1, iostat=ios, err=300)

   if ( filesize1 .ne. 11 ) error stop 122
   if ( position .ne. 12 ) error stop 123

!* test the terminal-point
  caseid =7
  read (1, pos=filesize, err=400, end=50, iostat=ios) ivar2 
  error stop 14
50 continue
  if (ios .ne. -1)  error stop 15
  print *, ios

  close(1, status='delete')

!* TEST2 : real
!* open an unit with formatted stream I/O
   open(1, access='stream', form='formatted', &
      iostat=ios, err=100, status='unknown')
   write(1, fmt='(F9.6)', iostat=ios, err=200) rvar1
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   filesize1 = filesize
   print *, position, filesize
   close(1)

!* open the same unit with unformatted stream I/O
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='append')
   write(1, iostat=ios, err=200) rpararr

!* test rewind statments
   rewind(1, iostat=ios, err=500)
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   print *, position, filesize
   if(position .ne. 1) error stop 2
   read(1, iostat=ios, err=400) rvar2


!* test end-of-record symbol
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   print *, position, filesize
   poin = position
   read (1, pos=10, iostat=ios, err=400) char1
   print *, char1
   if ( char1 .ne. "\n" )    error stop 22

!* test endfile statment
   endfile(1)
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   print *, position, filesize
   if(filesize .ne. filesize1) error stop 23

!* test the terminal-point
  read (1, pos=filesize, err=400, end=51, iostat=ios) ivar2
  error stop 24
51 continue
  if (ios .ne. -1)  error stop 25
  print *, ios

    close(1, status='delete')

!* TEST3 : logical 
!* open an unit with formatted stream I/O
   open(1, access='stream', form='formatted', &
      iostat=ios, err=100, status='unknown')
   write(1, fmt='(L2)', iostat=ios, err=200) lvar1
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   filesize1 = filesize
   print *, position, filesize
   close(1)

!* open the same unit with unformatted stream I/O
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='append')
   write(1, iostat=ios, err=200) lpararr

!* test rewind statments
   rewind(1, iostat=ios, err=500)
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   print *, position, filesize
   if(position .ne. 1) error stop 3
   read(1, iostat=ios, err=400) lvar2

!* test end-of-record symbol
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   print *, position, filesize
   poin = position
   read (1, pos=3, iostat=ios, err=400) char1
   print *, char1
   if ( char1 .ne. "\n" )    error stop 32

!* test endfile statment
   endfile(1)
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   print *, position, filesize
   if(filesize .ne. filesize1) error stop 33

!* test the terminal-point
  read (1, pos=filesize, err=400, end=52, iostat=ios) ivar2
  error stop 24
52 continue
  if (ios .ne. -1)  error stop 35
  print *, ios

   close(1, status='delete')

!* TEST4 : character 
!* open an unit with formatted stream I/O
   open(1, access='stream', form='formatted', &
      iostat=ios, err=100, status='unknown')
   write(1, FMT='(A)', iostat=ios, err=200) hvar1
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   filesize1 = filesize
   print *, position, filesize
   close(1)

!* open the same unit with unformatted stream I/O
   open(1, access='stream', form='unformatted',  &
      iostat=ios, err=100, status='old', position='append')
   write(1, iostat=ios, err=200) hpararr

!* test rewind statments
   rewind(1, iostat=ios, err=500)
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   print *, position, filesize
   if(position .ne. 1) error stop 4
   read(1, iostat=ios, err=400) hvar2
   if (hvar1 .ne. hvar2) error stop 41

!* test end-of-record symbol
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   print *, position, filesize
   poin = position
   read (1, pos=poin, iostat=ios, err=400) char1
   print *, char1
   if ( char1 .ne. "\n" )    error stop 44442

!* test endfile statment
   endfile(1)
   inquire( 1,pos=position, size=filesize, iostat=ios, err=300)
   print *, position, filesize
   if(filesize .ne. filesize1) error stop 43

!* test the terminal-point
  read (1, pos=filesize, err=300, end=53, iostat=ios) ivar2
  error stop 44
53 continue
  if (ios .ne. -1)  error stop 35
  print *, ios

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

