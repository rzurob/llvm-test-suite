! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 22, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test NUM=specifier in READ, WRITE
!*                               statments with -qintsize=2
!*                               stream I/O
!*
!=======================================================================

!* Declare and initialize Variables.

  integer*4 ios /0/
! integer*4 position /0/, position1 /0/
  integer*4 filesize /0/, filesize1 /0/
  integer*4 iol /0/, number(2) /2*0/
  integer*4 iol1 /0/, number1(2) /2*0/

  integer iarr1(10) /10*0/, iarr2(10) /10*0/    !default intsize is 2
  integer iarr3(10) /10*0/                      !default intsize is 2
  integer*1 iarr11(10) /10*0/, iarr12(10) /10*0/
  integer*1 iarr13(10) /10*0/

  logical   log1 /.TRUE./ , log2 /.TRUE./       !default intsize is 2
  logical   log3 /.TRUE./                       !default intsize is 2
  logical*1 log11 /.TRUE./, log12 /.TRUE./
  logical*1 log13 /.TRUE./

!* TEST1 : integer with unformatted stream I/O

   open(1, access='stream', form='unformatted', iostat=ios, err=100)
   open(2, access='stream', form='unformatted', iostat=ios, err=100)

   inquire(iolength=iol) iarr1
   write(1, num=number(1), iostat=ios, err=200) iarr2
   inquire(1, size=filesize, iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, num=number(2), iostat=ios, err=400)  iarr3

   inquire(iolength=iol1) iarr11
   write(2, num=number1(1), iostat=ios, err=200) iarr12
   inquire(2, size=filesize1, iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
   read(2, num=number1(2), iostat=ios, err=400)  iarr13

!  print *, iol, iol1
!  print *, number(1), number1(1)
!  print *, number(2), number1(2)
   print *, position, position1
   print *, filesize, filesize1

   if ( iol .ne. 2*iol1 )                      error stop 13
   if ( number(1) .ne. 2*number1(1))           error stop 14
   if ( number(2) .ne. 2*number1(2))           error stop 15
   if ( filesize  .ne. 2*filesize1)            error stop 17

   close(1, status='delete')
   close(2, status='delete')

!* TEST2 : integer with formatted I/O

   open(1, access='stream', form='formatted', iostat=ios, err=100)
   open(2, access='stream', form='formatted', iostat=ios, err=100)

   inquire(iolength=iol) iarr1
   write(1, FMT='(I5)', iostat=ios,  err=200) iarr2
   inquire(1, size=filesize, iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, FMT='(I5)', iostat=ios, err=400)  iarr3

   inquire(iolength=iol1) iarr11
   write(2, FMT='(I2)', iostat=ios, err=200) iarr12
   inquire(2, size=filesize1, iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
   read(2, FMT='(I2)', iostat=ios, err=400)  iarr13

   if ( iol .ne. 2*iol1 )                      error stop 23
   if ( filesize  .ne. 2*filesize1)            error stop 27

   close(1, status='delete')
   close(2, status='delete')

!* TEST3 : logical with unformatted I/O

   open(1, access='stream', form='unformatted', iostat=ios, err=100)
   open(2, access='stream', form='unformatted', iostat=ios, err=100)

   inquire( iolength=iol) log1
   write(1, num=number(1), iostat=ios, err=200) log2
   inquire(1, size=filesize, iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, num=number(2), iostat=ios, err=400)  log3

   inquire( iolength=iol1) log11
   write(2, num=number1(1), iostat=ios, err=200) log12
   inquire(2, size=filesize1, iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
   read(2, num=number1(2), iostat=ios, err=400)  log13

!  print *, iol, iol1
!  print *, number(1), number1(1)
!  print *, number(2), number1(2)
!  print *, position, position1
!  print *, filesize, filesize1
   if ( iol .ne. 2*iol1 )      error stop 33
   if ( number(1) .ne. 2*number1(1))   error stop 34
   if ( number(2) .ne. 2*number1(2))   error stop 35
   if ( filesize .ne.  2*filesize1)    error stop 37

   close(1, status='delete')
   close(2, status='delete')

!* TEST4 : logical with formatted I/O

   open(1, access='stream', form='formatted', iostat=ios, err=100)
   open(2, access='stream', form='formatted', iostat=ios, err=100)

   inquire(iolength=iol) log1
   write(1, FMT='(L5)', iostat=ios,  err=200) log2
   inquire(1, size=filesize, iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, FMT='(L5)', iostat=ios, err=400)  log3
   inquire(iolength=iol1) log11
   write(2, FMT='(L2)', iostat=ios, err=200) log12
   inquire(2, size=filesize1, iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
   read(2, FMT='(L2)', iostat=ios, err=400)  log13

   if ( iol .ne. 2*iol1 )                      error stop 43
   if ( number(1) .ne. 2*number1(1))           error stop 44
   if ( number(2) .ne. 2*number1(2))           error stop 45
   if ( filesize  .ne. 2*filesize1)            error stop 47

   close(1, status='delete')
   close(2, status='delete')

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
