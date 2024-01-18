! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS: -qintsize=2
! %GROUP: fxstio205.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 22, 2003
!*
!*  Primary Function Tested    : options with stream I/O
!*
!*  Description                : Test SIZE=specifier in READ, WRITE &
!*                               INQUIRE statments with -qintsize=2
!*                               stream I/O
!*
!=======================================================================

!* Declare Variables.

  integer*4 ios /0/
! integer*4 position /0/, position1 /0/
  integer*4 filesize(3) /3*0/, filesize1(3) /3*0/
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

   inquire(1, size=filesize(1),  iostat=ios, err=300)
   inquire(2, size=filesize1(1), iostat=ios, err=300)

   if ( filesize(1)  .ne. 0 )     error stop 11
   if ( filesize1(1) .ne. 0 )     error stop 12

   inquire(iolength=iol) iarr1

   write(1, iostat=ios, err=200) iarr2
   inquire(1, size=filesize(2), iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, iostat=ios, err=400)  iarr3
   inquire(1, size=filesize(3), iostat=ios, err=300)

   inquire(iolength=iol1) iarr11
!  write(2, size=filesize(1), iostat=ios, err=200) iarr12
   write(2, iostat=ios, err=200) iarr12
   inquire(2, size=filesize1(2), iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
!  read(2, size=filesiz3(3), iostat=ios, err=400)  iarr13
   read(2, iostat=ios, err=400)  iarr13
   inquire(2, size=filesize1(3), iostat=ios, err=300)

   if ( iol .ne. 2*iol1 )                      error stop 13
   if ( filesize(2) .ne. 2*filesize1(2))       error stop 15
   if ( filesize(3) .ne. 2*filesize1(3))       error stop 16
   if ((iol .ne. 20) .or.  &
       (filesize(2) .ne. 20) .or. (filesize(3) .ne. 20))   &
   error stop 17

   close(1, status='delete')
   close(2, status='delete')

!* TEST2 : integer with formatted I/O

   open(1, access='stream', form='formatted', iostat=ios, err=100)
   open(2, access='stream', form='formatted', iostat=ios, err=100)

   inquire(1, size=filesize(1),  iostat=ios, err=300)
   inquire(2, size=filesize1(1), iostat=ios, err=300)

   if ( filesize(1)  .ne. 0 )     error stop 21
   if ( filesize1(1) .ne. 0 )     error stop 22

   inquire(iolength=iol) iarr1
   write(1, FMT='(I5)', iostat=ios,  err=200) iarr2
   inquire(1, size=filesize(2), iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, FMT='(I5)', iostat=ios, err=400)  iarr3
   inquire(1, size=filesize(3), iostat=ios, err=300)

   inquire(iolength=iol1) iarr11
   write(2, FMT='(I2)', iostat=ios, err=200) iarr12
   inquire(2, size=filesize1(2), iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
   read(2, FMT='(I2)', iostat=ios, err=400)  iarr13
   inquire(2, size=filesize1(3), iostat=ios, err=300)

   if ( iol .ne. 2*iol1 )                      error stop 23
   if ( filesize(2) .ne. 2*filesize1(2))       error stop 25
   if ( filesize(3) .ne. 2*filesize1(3))       error stop 26
   if ((iol .ne. 20) .or.  &
       (filesize(2) .ne. 60) .or. (filesize(3) .ne. 60))   &
   error stop 27

   close(1, status='delete')
   close(2, status='delete')

!* TEST3 : logical with unformatted I/O

   open(1, access='stream', form='unformatted', iostat=ios, err=100)
   open(2, access='stream', form='unformatted', iostat=ios, err=100)

   inquire(1, size=filesize(1), iostat=ios, err=300)
   inquire(2, size=filesize1(1), iostat=ios, err=300)

   if ( filesize(1)  .ne. 0 )     error stop 31
   if ( filesize1(1) .ne. 0 )     error stop 32

   inquire( iolength=iol) log1
   write(1, iostat=ios, err=200) log2
   inquire(1, size=filesize(2), iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, iostat=ios, err=400)  log3
   inquire(1, size=filesize(3), iostat=ios, err=300)

   inquire(iolength=iol1) log11
   write(2, iostat=ios, err=200) log12
   inquire(2, size=filesize1(2), iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
   read(2, iostat=ios, err=400)  log13
   inquire(2, size=filesize1(3), iostat=ios, err=300)

   if ( iol .ne. 2*iol1 )                      error stop 33
   if ( filesize(2) .ne. 2*filesize1(2))       error stop 35
   if ( filesize(3) .ne. 2*filesize1(3))       error stop 36
   if ((iol .ne. 2) .or.  &
       (filesize(2) .ne. 2) .or. (filesize(3) .ne. 2))   &
   error stop 37

   close(1, status='delete')
   close(2, status='delete')

!* TEST4 : logical with formatted I/O

   open(1, access='stream', form='formatted', iostat=ios, err=100)
   open(2, access='stream', form='formatted', iostat=ios, err=100)

   inquire(1, size=filesize(1),  iostat=ios, err=300)
   inquire(2, size=filesize1(1), iostat=ios, err=300)

   if ( filesize(1)  .ne. 0 )     error stop 41
   if ( filesize1(1) .ne. 0 )     error stop 42

   inquire(iolength=iol) log1
   write(1, FMT='(L5)', iostat=ios,  err=200) log2
   inquire(1, size=filesize(2), iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, FMT='(L5)', iostat=ios, err=400)  log3
   inquire(1, size=filesize(3), iostat=ios, err=300)

   inquire(iolength=iol1) log11
   write(2, FMT='(L2)', iostat=ios, err=200) log12
   inquire(2, size=filesize1(2), iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
   read(2, FMT='(L2)', iostat=ios, err=400)  log13
   inquire(2, size=filesize1(3), iostat=ios, err=300)

   if ( iol .ne. 2*iol1 )                      error stop 43
   if ( filesize(2) .ne. 2*filesize1(2))       error stop 45
   if ( filesize(3) .ne. 2*filesize1(3))       error stop 46
   if ((iol .ne. 2) .or.  &
       (filesize(2) .ne. 6) .or. (filesize(3) .ne. 6))   &
   error stop 47

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

