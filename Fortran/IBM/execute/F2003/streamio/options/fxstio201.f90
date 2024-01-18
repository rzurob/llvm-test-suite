! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS: -qintsize=2
! %GROUP: fxstio201.f
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
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test POS=specifier in READ, WRITE &
!*                               INQUIRE statments with -qintsize=2
!*                               stream I/O
!*
!=======================================================================

!* Declare Variables.

  integer*4 pos1 /0/, pos2 /0/, ios /0/
  integer*4 position /0/, position1 /0/
  integer*4 filesize /0/, filesize1 /0/
  integer*4 iol /0/, number(2) /2*0/
  integer*4 iol1 /0/, number1(2) /2*0/

  integer iarr1(10) /10*0/, iarr2(10) /10*0/   !default intsize is 2
  integer iarr3(10) /10*0/                     !default intsize is 2
  integer*1 iarr11(10) /10*0/, iarr12(10) /10*0/
  integer*1 iarr13(10) /10*0/

  logical   log1 /.TRUE./ , log2 /.TRUE./       !default intsize is 2
  logical   log3 /.TRUE./                       !default intsize is 2
  logical*1 log11 /.TRUE./, log12 /.TRUE./
  logical*1 log13 /.TRUE./

  hkin = kin/2

!* TEST1 : integer with unformatted stream I/O

   open(1, access='stream', form='unformatted', iostat=ios, err=100)
   open(2, access='stream', form='unformatted', iostat=ios, err=100)

   inquire(1, pos=pos1, iostat=ios, err=300)
   inquire(2, pos=pos2, iostat=ios, err=300)
   if ( pos1 .ne. 1 )     error stop 11
   if ( pos2 .ne. 1 )     error stop 12

   inquire(iolength=iol) iarr1
   write(1, num=number(1), iostat=ios, err=200) iarr2
   inquire(1, pos=position, size=filesize, iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, num=number(2), iostat=ios, err=400)  iarr3

   inquire(iolength=iol1) iarr11
!* specify the pos=iol1+1 in write statment
   write(2, num=number1(1), pos=iol1+1, iostat=ios, err=200) iarr12
   inquire(2, pos=position1, size=filesize1, iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
!* specify the pos=iol1+1 in read statment
   read(2, num=number1(2), pos=iol1+1, iostat=ios, err=400)  iarr13

!* The file sizes and positions of the two units should be eqeal
   if ( iol .ne. 2*iol1 )                      error stop 13
   if ( number(1) .ne. 2*number1(1))           error stop 14
   if ( number(2) .ne. 2*number1(2))           error stop 15
   if ( position  .ne. position1)              error stop 16
   if ( filesize  .ne. filesize1)            error stop 17

   close(1, status='delete')
   close(2, status='delete')

!* TEST2 : integer with formatted I/O

   open(1, access='stream', form='formatted', iostat=ios, err=100)
   open(2, access='stream', form='formatted', iostat=ios, err=100)

   inquire(1, pos=pos1, iostat=ios, err=300)
   inquire(2, pos=pos2, iostat=ios, err=300)
   if ( pos1 .ne. 1 )     error stop 21
   if ( pos2 .ne. 1 )     error stop 22

   inquire(iolength=iol) iarr1
   write(1, FMT='(I5)', iostat=ios,  err=200) iarr2
   inquire(1, pos=position, size=filesize, iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, FMT='(I5)', iostat=ios, err=400)  iarr3

   inquire(iolength=iol1) iarr11
!* specify the pos=iol1+1 in write statment
   write(2, FMT='(I2)', pos=iol1+1, iostat=ios, err=200) iarr12
   inquire(2, pos=position1, size=filesize1, iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
!* specify the pos=iol1+1 in read statment
   read(2, FMT='(I2)', pos=iol1+1, iostat=ios, err=400)  iarr13

!* The file sizes and positions of the two units should be eqeal
   if ( iol .ne. 2*iol1 )                      error stop 23
   if ( number(1) .ne. 2*number1(1))           error stop 24
   if ( number(2) .ne. 2*number1(2))           error stop 25

   close(1, status='delete')
   close(2, status='delete')

!* TEST3 : logical with unformatted I/O
   open(1, access='stream', form='unformatted', iostat=ios, err=100)
   open(2, access='stream', form='unformatted', iostat=ios, err=100)

   inquire(1, pos=pos1, iostat=ios, err=300)
   inquire(2, pos=pos2, iostat=ios, err=300)

   if ( pos1 .ne. 1 )     error stop 31
   if ( pos2 .ne. 1 )     error stop 32

   inquire( iolength=iol) log1
   write(1, num=number(1), iostat=ios, err=200) log2
   inquire(1, pos=position, size=filesize, iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, num=number(2), iostat=ios, err=400)  log3

   inquire( iolength=iol1) log11
!* specify the pos=iol1+1 in write statment
   write(2, num=number1(1), pos=iol1+1, iostat=ios, err=200) log12
   inquire(2, pos=position1, size=filesize1, iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
!* specify the pos=iol1=1 in read statment
   read(2, num=number1(2), pos=iol1+1, iostat=ios, err=400)  log13

   if ( iol .ne. 2*iol1 )      error stop 33
   if ( number(1) .ne. 2*number1(1))   error stop 34
   if ( number(2) .ne. 2*number1(2))   error stop 35
   if ( position  .ne. position1)      error stop 36
   if ( filesize .ne.  filesize1)    error stop 37

   close(1, status='delete')
   close(2, status='delete')

!* TEST4 : logical with formatted I/O

   open(1, access='stream', form='formatted', iostat=ios, err=100)
   open(2, access='stream', form='formatted', iostat=ios, err=100)

   inquire(1, pos=pos1, iostat=ios, err=300)
   inquire(2, pos=pos2, iostat=ios, err=300)

   if ( pos1 .ne. 1 )     error stop 41
   if ( pos2 .ne. 1 )     error stop 42

   inquire(iolength=iol) log1
   write(1, FMT='(L5)', iostat=ios,  err=200) log2
   inquire(1, pos=position, size=filesize, iostat=ios, err=300)
   rewind(1, iostat=ios, err=500)
   read(1, FMT='(L5)', iostat=ios, err=400)  log3
   inquire(iolength=iol1) log11
!* specify the pos=iol1+1 in write statment
   write(2, FMT='(L2)', pos=iol1+1, iostat=ios, err=200) log12
   inquire(2, pos=position1, size=filesize1, iostat=ios, err=300)
   rewind(2, iostat=ios, err=500)
!* specify the pos=iol1+1 in read statment
   read(2, FMT='(L2)', pos=iol1+1, iostat=ios, err=400)  log13

!* The file sizes and positions of the two units should be eqeal
   if ( iol .ne. 2*iol1 )                      error stop 43
   if ( number(1) .ne. 2*number1(1))           error stop 44
   if ( number(2) .ne. 2*number1(2))           error stop 45

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

