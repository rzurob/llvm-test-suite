! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxstio308.presh fxstio308
! %COMPOPTS:
! %GROUP:    redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 31, 2003
!*
!*  Primary Function Tested    : inter-language call
!*
!*  Description                : Test flush_ with stream Access I/O.
!*                               use Fortran program call a C function
!*                               which read the file created by fortran
!*                               , then modify the data and create a new
!*                               file. Then Fortran will read the data
!*                               back.
!*
!=======================================================================

!* Declare Variables.

   integer ios /0/
   integer ivar /123/, ivar1, ivar2
   real rvar /1.234567/, rvar1, rvar2
   character*2 hvar /'ab'/, hvar1, hvar2

!* create  the infile with formatted I/O access
   open(1, file='infile',access='stream', form='formatted', iostat=ios,&
      err=100)
   write(1, fmt='(I10)') ivar
   write(1, fmt='(F9.6)') rvar
   write(1, fmt='(A2)')   hvar
   print *, ivar, rvar, hvar
   call flush_(1)

!* Here we call c function to read the date in "infile", then modify
!* the data and write the new data to "outfile"

   call rwfunc()

!* open the "outfile" created by c function with formatted I/O access

   open(2, file='outfile',access='stream', form='formatted', iostat=ios,&
      err=100)

   read(2, fmt='(I10)', iostat=ios, err=200) ivar1
   read(2, fmt='(F9.6)', iostat=ios, err=200) rvar1
   read(2, fmt='(A2)', iostat=ios, err=200) hvar1

   print *, ivar1, rvar1, hvar1
   close(2)

!* open the "outfile" with sequential access
   open(2, file='outfile',access='sequential', form='formatted', iostat=ios,&
      err=100)

   read(2, fmt='(I10)', iostat=ios, err=200) ivar2
   read(2, fmt='(F9.6)', iostat=ios, err=200) rvar2
   read(2, fmt='(A2)', iostat=ios, err=200) hvar2

   if(ivar1 .ne. ivar2) error stop 111
   if(rvar1 .ne. rvar2) error stop 2
!  if(havr1 .ne. hvar2) error stop 3

stop

100 print *, "open error: iostat = ", ios
    error stop 100
200 print *, "read  error: iostat = ", ios
    error stop 200
300 print *, "write  error: iostat = ", ios
    error stop 300

end

