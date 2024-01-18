! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*; $TR_SRC/fxstio307.presh fxstio307
! %COMPOPTS: 
! %GROUP: redherring.f 
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
!*  TEST CASE TITLE            : fxstio307.f 
!*
!*  PROGRAMMER                 : Catherine Sun
!*  
!*  Creation Date              : Mar 31, 2003
!*
!*  Primary Function Tested    : sequential access devices 
!*
!*  Description                : Test stream access I/O on sequential 
!*                               access devices.
!*
!=======================================================================
!* modify this testcase, since the constant failure by using "run"
!* will command out all the statments using /dev/tty

   integer ios /0/
   integer ivar1 /2000/
   complex*8 cvar1 /(-1.175494e-38,3.402823e38)/
   character*1 hvar1 /'a'/

!* TEST1 : integer
!* open a sequential access device with unformatted I/O access
!* open(1, file='/dev/tty',access='stream', form='formatted', iostat=ios,&
!*    err=100)
!* write(1, fmt='(I7)', iostat=ios, err=200) ivar1

!* open a sequential access device with formatted I/O access
   open(2, file='/dev/null',access='stream', form='unformatted', iostat=ios,&
      err=120)
   write(2, iostat=ios, err=200) ivar1
 
!* close(1)
   close(2)

! TEST2 : complex
!* open a sequential access devie with unformatted I/O access
   open(1, file='/dev/null', access='stream', form='unformatted', iostat=ios,&
      err=120)
   write(1, iostat=ios, err=200) cvar1

!* open a sequential access devie with formatted I/O access
!* open(2, file='/dev/tty', access='stream', form='formatted', iostat=ios,&
!*    err=100)
!* write(2, fmt='(2E15.7)', iostat=ios, err=200) cvar1

   close(1)
!* close(2)

!* TEST3: character
!* open a sequential access devie with unformatted I/O access
   open(1, file='/dev/null', access='stream', form='unformatted', iostat=ios,&
      err=120)
   write(1, iostat=ios, err=200) hvar1

!* open a sequential access devie with formatted I/O access
!* open(2, file='/dev/tty', access='stream', form='formatted', iostat=ios,&
!*    err=100)
!* write(2, fmt='(A1)', iostat=ios, err=200) hvar1
   
   close(1)
!* close(2)

stop
100 print *, "open error: iostat = ", ios
    error stop 100
120 print *, "open error: iostat = ", ios
    error stop 120
200 print *, "write error: iostat = ", ios
    error stop 200
end

