!*********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 22, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test stream access I/O with NANS,
!*                               NANQ, INF
!*
!=======================================================================
!* Define the values by encoding the bit pattern directly
   real*4 pinf /z'7f800000'/ , minf /z'ff800000'/
   real*4 pnanq /z'7fc00000'/, minus_nanq /z'ffc00000'/
   real*4 pnans /z'7f800001'/, mnans /z'ffbfffff'/

   real*4 rpinf, rminf
   real*4 rpnanq, rmnanq

   integer*4 ipinf, iminf
   integer*4 ipnanq, imnanq
   integer*4 ipnans, imnans
   integer ios

   character*3   pinf1, pinf11
   character*4   minf1, pnanq1, mnanq1, pnans1, mnans1
   character*4   minf11, pnanq11, mnanq11, pnans11, mnans11

   equivalence(ipinf, rpinf)
   equivalence(iminf, rminf)
   equivalence(ipnanq,rpnanq)
   equivalence(imnanq,rmnanq)
   equivalence(ipnans,rpnans)
   equivalence(imnans,rmnans)

!* Test unformatted stream access I/O
   open(1, access='stream', form='unformatted', iostat=ios,&
      err=100)
   write(1, iostat=ios, err=200) pinf, minf, pnanq, mnanq, &
      pnans, mnans
   rewind(1, iostat=ios, err=500)

   read(1, iostat=ios, err=400) rpinf, rminf, rpnanq, rmnanq
   if(ipinf .ne. z"7f800000")     error stop 111
   if(iminf .ne. z'ff800000')     error stop 12
   if(ipnanq .ne. z'7fc00000')    error stop 13
!  if(imnanq .ne. z'ffc00000')    error stop 14
!  if(ipnans .ne. z'7f800001')    error stop 15
!  if(imnans .ne. z'ffbfffff')    error stop 16

   close(1, status='keep')

   open(1, access='sequential', form='unformatted', iostat=ios,&
      status='old', err=100)
   read(1, iostat=ios, err=400) rpinf1, rminf1, rpnanq1, rmnanq1
   if(ipinf .ne. z'7f800000')     error stop 222
   if(iminf .ne. z'ff800000')     error stop 21
   if(ipnanq .ne. z'7fc00000')    error stop 23
!  if(imnanq .ne. z'ffc00000')    error stop 24
!  if(ipnans .ne. z'7f800001')    error stop 25
!  if(imnans .ne. z'ffbfffff')    error stop 26

close(1, status='delete')

!* Test formatted stream I/O access
   open(1, access='stream', form='formatted', iostat=ios,&
      err=100)
   write(1, fmt='(G7.1)', iostat=ios, err=200) pinf, minf, pnanq, mnanq, &
      pnans, mnans
   rewind(1, iostat=ios, err=500)

   read(1,fmt='(G7.1)', end=600) pinf1, minf1, pnanq1, mnanq1, &
      pnans1, mnans1
   close(1, status='keep')

   open(1, access='sequential', form='formatted', iostat=ios,&
      err=100)
   read(1,fmt='(G7.1)', end=600) pinf11, minf11, pnanq11, mnanq11, &
      pnans11, mnans11

   if(pinf1 .ne. pinf11)     error stop 3
   if(minf1 .ne. minf11)     error stop 31
   if(pnanq1 .ne. pnanq11) error stop 32
   if(mnanq1 .ne. mnanq11) error stop 33

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
600 print *, "end-of-file error: iostat = ", ios
    error stop 600

end
