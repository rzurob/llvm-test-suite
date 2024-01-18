! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxstio014.presh   
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
!*  TEST CASE TITLE            : fxstio014.f 
!*
!*  PROGRAMMER                 : Catherine Sun
!*  
!*  Creation Date              : Mar 17, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test unformatted synchronous stream
!*                               I/O in OpenMP parallel region. 
!*
!=======================================================================

!* Declare Variables.
   
  implicit none

  integer i, j, k, ios
  integer, dimension(3,3) :: iarr1/9*0/, iarr2/9*0/
  integer, dimension(3,3) :: iarr3/9*0/, iarr4/9*0/
  integer, dimension(3,3) :: iarr5/9*0/, iarr6/9*0/

  real, dimension(3,3) :: rarr1/9*0.0/, rarr2/9*0.0/
  real, dimension(3,3) :: rarr3/9*0.0/, rarr4/9*0.0/
  real, dimension(3,3) :: rarr5/9*0.0/, rarr6/9*0.0/
 
  complex, dimension(3,3) :: carr1/9*(0.0,0.0)/, carr2/9*(0.0,0.0)/
  complex, dimension(3,3) :: carr3/9*(0.0,0.0)/, carr4/9*(0.0,0.0)/
  complex, dimension(3,3) :: carr5/9*(0.0,0.0)/, carr6/9*(0.0,0.0)/

  logical, dimension(3,3) :: larr1/9*.true./, larr2/9*.true./
  logical, dimension(3,3) :: larr3/9*.true./, larr4/9*.true./
  logical, dimension(3,3) :: larr5/9*.true./, larr6/9*.true./ 

!* TEST1 : integer

   do i=1,4
      open(i, form='unformatted', access='stream', iostat=ios, err=100)
   enddo

   do i = 1,3
      iarr1(:,i) = i
      iarr2(:,i) = iarr1(:,i) + 1
   enddo

   !omp$ parallel do
   do i=1,3
      write (i) iarr1(:,i), iarr2(:,i)
   enddo
   
   !omp$ parallel do
   do i=1,3
      write (4) iarr1(:,i), iarr2(:,i)
   enddo

   do i=1,4
      rewind(i , iostat=ios, err=500)
   enddo
 
   !omp$ paralleldo
   do i=1,3
      read (i) iarr3(:,i), iarr4(:,i)
   enddo

   !omp$ paralleldo
   do i=1,3
      read (4) iarr5(:,i), iarr6(:,i)
   enddo

   do i=1,3
      do j=1,3
         if (iarr3(i,j)<>j) error stop 1
         if (iarr4(i,j)<>j+1) error stop 2
         if (iarr5(i,j)<>j) error stop 3
         if (iarr6(i,j)<>j+1) error stop 4
      enddo
   enddo

   !omp$ paralleldo
   do i=1,4
      close(i, status='delete')
   enddo

!* TEST2 : real

   do i=1,4
      open(i, form='unformatted', access='stream', iostat=ios, err=100)
   enddo

   do i = 1,3 
      rarr1(:,i) = i
      rarr2(:,i) = rarr1(:,i) + 1
   enddo

   !omp$ parallel do
   do i=1,3
      write (i) rarr1(:,i), rarr2(:,i)
   enddo

   !omp$ parallel do
   do i=1,3
      write (4) rarr1(:,i), rarr2(:,i)
   enddo

   do i=1,4
      rewind(i, iostat=ios, err=500)
   enddo

   !omp$ paralleldo
   do i=1,3
      read (i) rarr3(:,i), rarr4(:,i)
   enddo

   !omp$ paralleldo
   do i=1,3
      read (4) rarr5(:,i), rarr6(:,i)
   enddo

   do i=1,3
      do j=1,3
         if (rarr3(i,j)<>j) error stop 5
         if (rarr4(i,j)<>j+1) error stop 6
         if (rarr5(i,j)<>j) error stop 7
         if (rarr6(i,j)<>j+1) error stop 8
      enddo
   enddo
 
   !omp$ paralleldo
   do i=1,4
      close(i, status='delete')
   enddo

!* TEST3 : complex

   do i=1,4
      open(i, form='unformatted', access='stream', iostat=ios, err=100)
   enddo

   do i = 1,3 
      carr1(:,i) = i
      carr2(:,i) = carr1(:,i) + 1
   enddo

   !omp$ parallel do
   do i=1,3
      write (i) carr1(:,i), carr2(:,i)
   enddo

   !omp$ parallel do
   do i=1,3
      write (4) carr1(:,i), carr2(:,i)
   enddo

   do i=1,4
      rewind(i, iostat=ios, err=500)
   enddo

   !omp$ paralleldo
   do i=1,3
      read (i) carr3(:,i), carr4(:,i)
   enddo

   !omp$ paralleldo
   do i=1,3
      read (4) carr5(:,i), carr6(:,i)
   enddo

   do i=1,3
      do j=1,3
         if (carr3(i,j)<>j) error stop 9
         if (carr4(i,j)<>j+1) error stop 2
         if (carr5(i,j)<>j) error stop 11
         if (carr6(i,j)<>j+1) error stop 12
      enddo
   enddo

   !omp$ paralleldo
   do i=1,4
      close(i, status='delete')
   enddo

!* TEST4 : logical

   do i=1,4
      open(i, form='unformatted', access='stream', iostat=ios, err=100)
   enddo

   do i = 1,3 
      larr1(:,i) = .false. 
      larr2(:,i) = .false. 
   enddo

   !omp$ parallel do
   do i=1,3
      write (i) larr1(:,i), larr2(:,i)
   enddo

   !omp$ parallel do
   do i=1,3
      write (4) larr1(:,i), larr2(:,i)
   enddo
 
   do i=1,4
      rewind(i, iostat=ios, err=500)
   enddo

   !omp$ paralleldo
   do i=1,3
      read (i) larr3(:,i), larr4(:,i)
   enddo

   !omp$ paralleldo
   do i=1,3
      read (4) larr5(:,i), larr6(:,i)
   enddo

   do i=1,3
      do j=1,3
         if (larr3(i,j) .neqv. .false.) error stop 13
         if (larr4(i,j) .neqv. .false.) error stop 14
         if (larr5(i,j) .neqv. .false.) error stop 15
         if (larr6(i,j) .neqv. .false.) error stop 16
      enddo
   enddo

   !omp$ paralleldo
   do i=1,4
      close(i, status='delete')
   enddo

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

