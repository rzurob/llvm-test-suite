! %START
! %MAIN: YES
! %PRECMD:  rm -f fort.*
! %COMPOPTS:
! %GROUP: fxstio022.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!--===================================================================
!*
!*  PROGAMMER                 : Catherine Sun
!*
!*  Creation Date             : March 18, 2003
!*
!*  Primary Function Tested   : Unformatted stream access I/O
!*
!*  Description               : Open a record file with stream access
!*                              and test access to initial-point,
!*                              terminal-point and end-of-recods.
!*
!=======================================================================

  implicit none

  integer position /0/, filesize /0/
  integer filesize1 /0/
  integer caseid /0/, ios(11) /11*0/
  character*3 a /'100'/
  character*2  b
  character*1  c
  character*5  d

!* open a formatted stream unit.
  open(1, form='formatted', access='stream', err=100, iostat=ios(1))
  caseid = 1
  if (ios(1) .ne. 0) call zzrc(caseid)

  inquire( 1,pos=position, size=filesize, err=300, iostat=ios(2))
  caseid = 2
  if (position .ne. 1) call zzrc(caseid)

  write(1, fmt='(A3)', iostat=ios(11), err=200)  a
  close(1)

!* open the file with unformatted stream access
  open(1, form='unformatted', access='stream',err=100, iostat=ios(3))
  caseid = 3
  if (ios(1) .ne. 0) call zzrc(caseid)

!* test  the initial point
  inquire( 1,pos=position, size=filesize, err=300, iostat=ios(4))
  caseid = 4
  if (position .ne. 1) call zzrc(caseid)

  read (1, pos=position, err=400, iostat=ios(5)) b
  caseid = 5
  if ( b .ne. "10") call zzrc(caseid)

!* test the end-of-record symbol "\n"
  read (1, pos=4, err=400, iostat=ios(6)) c
  caseid = 6
  if ( c .ne. "\n" )    call zzrc(caseid)

!* test the terminal-point
  caseid =7
  read (1, pos=10, err=400, end=50, iostat=ios(7)) b
  call zzrc(caseid)

50 continue
  caseid = 8
  if (ios(7) .ne. -1)  call zzrc(caseid)

!* test the terminal-point has been extended when using POS specifer
!* with an empty output list
  caseid =9
  write (1, pos=20, err=200, iostat=ios(8))
  if (ios(8) .ne. 0)  call zzrc(caseid)

  inquire( 1, pos = position, size=filesize1, err=300, iostat=ios(9))
  print *, position, filesize1
  caseid = 10
  if ((position .ne. 20) .or. (filesize1 .ne. filesize)) call zzrc(caseid)

!* test the terminal-point has been extended when using POS specifer
!* with an output list

  write (1, pos=30, err=200, iostat=ios(10)) a
  inquire( 1, pos = position, size=filesize1, err=300, iostat=ios(9))
  print *, position, filesize1
  caseid =11
  if((position .ne. 33) .or. (filesize1 .ne. 32 )) call zzrc(caseid)

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
