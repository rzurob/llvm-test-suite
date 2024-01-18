! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:
! %GROUP: fxstio015.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 17, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test unformatted asynchronous stream
!*                               I/O in OpenMP parallel region.
!*
!=======================================================================

!* Declare Variables.

  implicit none

  integer id(11) /11*0/
  integer i, j, k
  integer, dimension(10,10) :: iarr1/100*0/, iarr2/100*0/
  integer, dimension(10,10) :: iarr3/100*0/, iarr4/100*0/
  integer, dimension(10,10) :: iarr5/100*0/, iarr6/100*0/

  real, dimension(10,10) :: rarr1/100*0.0/, rarr2/100*0.0/
  real, dimension(10,10) :: rarr3/100*0.0/, rarr4/100*0.0/
  real, dimension(10,10) :: rarr5/100*0.0/, rarr6/100*0.0/

  complex, dimension(10,10) :: carr1/100*(0.0,0.0)/, carr2/100*(0.0,0.0)/
  complex, dimension(10,10) :: carr3/100*(0.0,0.0)/, carr4/100*(0.0,0.0)/
  complex, dimension(10,10) :: carr5/100*(0.0,0.0)/, carr6/100*(0.0,0.0)/

  logical, dimension(10,10) :: larr1/100*.true./, larr2/100*.true./
  logical, dimension(10,10) :: larr3/100*.true./, larr4/100*.true./
  logical, dimension(10,10) :: larr5/100*.true./, larr6/100*.true./

!* TEST1 : integer

   do i=1,10
      open(i, form='unformatted', access='stream', asynch='yes', &
         err=100)
   enddo

   do i = 1, 10
      iarr1(:,i) = i
      iarr2(:,i) = iarr1(:,i) + 1
   enddo

   !smp$ parallel do
   do i=1,10
      write (i, id=id(i)) iarr1(:,i), iarr2(:,i)
   enddo

   do i=1,10
      wait (id=id(i))
   enddo

   do i=1,10
      rewind(i)
   enddo

   !smp$ parallel do
   do i=1,10
      write (11, id=id(11)) iarr1(:,i), iarr2(:,i)
   enddo
   wait(id=id(11))
   rewind(11)

   !smp$ paralleldo
   do i=1,10
      read (i, id=id(i)) iarr3(:,i), iarr4(:,i)
   enddo

   !smp$ paralleldo
   do i=1,10
      read (11, id=id(11)) iarr5(:,i), iarr6(:,i)
   enddo

   do i=1,10
      do j=1,10
         if (iarr3(i,j)<>j) error stop 1
         if (iarr4(i,j)<>j+1) error stop 2
         if (iarr5(i,j)<>j) error stop 3
         if (iarr6(i,j)<>j+1) error stop 4
      enddo
   enddo

   !smp$ paralleldo
   do i=1,10
      close(i)
   enddo
   close(11)

!* TEST2 : real

   do i=1,10
      open(i, form='unformatted', access='stream', asynch='yes', &
         err=100)
   enddo

   do i = 1, 10
      rarr1(:,i) = i
      rarr2(:,i) = rarr1(:,i) + 1
   enddo

   !smp$ parallel do
   do i=1,10
      write (i, id=id(i)) rarr1(:,i), rarr2(:,i)
   enddo

   do i=1,10
      wait (id=id(i))
   enddo

   do i=1,10
      rewind(i)
   enddo

   !smp$ parallel do
   do i=1,10
      write (11, id=id(11)) rarr1(:,i), rarr2(:,i)
   enddo
   wait(id=id(11))
   rewind(11)

   !smp$ paralleldo
   do i=1,10
      read (i, id=id(i)) rarr3(:,i), rarr4(:,i)
   enddo

   !smp$ paralleldo
   do i=1,10
      read (11, id=id(11)) rarr5(:,i), rarr6(:,i)
   enddo

   do i=1,10
      do j=1,10
         if (rarr3(i,j)<>j) error stop 5
         if (rarr4(i,j)<>j+1) error stop 6
         if (rarr5(i,j)<>j) error stop 7
         if (rarr6(i,j)<>j+1) error stop 8
      enddo
   enddo

   !smp$ paralleldo
   do i=1,10
      close(i)
   enddo
   close(11)

!* TEST3 : complex

   do i=1,10
      open(i, form='unformatted', access='stream', asynch='yes', &
         err=100)
   enddo

   do i = 1, 10
      carr1(:,i) = i
      carr2(:,i) = carr1(:,i) + 1
   enddo

   !smp$ parallel do
   do i=1,10
      write (i, id=id(i)) carr1(:,i), carr2(:,i)
   enddo

   do i=1,10
      wait (id=id(i))
   enddo

   do i=1,10
      rewind(i)
   enddo

   !smp$ parallel do
   do i=1,10
      write (11, id=id(11)) carr1(:,i), carr2(:,i)
   enddo
   wait(id=id(11))
   rewind(11)

   !smp$ paralleldo
   do i=1,10
      read (i, id=id(i)) carr3(:,i), carr4(:,i)
   enddo

   !smp$ paralleldo
   do i=1,10
      read (11, id=id(11)) carr5(:,i), carr6(:,i)
   enddo

   do i=1,10
      do j=1,10
         if (carr3(i,j)<>j) error stop 9
         if (carr4(i,j)<>j+1) error stop 2
         if (carr5(i,j)<>j) error stop 11
         if (carr6(i,j)<>j+1) error stop 12
      enddo
   enddo

   !smp$ paralleldo
   do i=1,10
      close(i)
   enddo
   close(11)

!* TEST4 : logival

   do i=1,10
      open(i, form='unformatted', access='stream', asynch='yes', &
         err=100)
   enddo

   do i = 1, 10
      larr1(:,i) = .false.
      larr2(:,i) = .false.
   enddo

   !smp$ parallel do
   do i=1,10
      write (i, id=id(i)) larr1(:,i), larr2(:,i)
   enddo

   do i=1,10
      wait (id=id(i))
   enddo

   do i=1,10
      rewind(i)
   enddo

   !smp$ parallel do
   do i=1,10
      write (11, id=id(11)) larr1(:,i), larr2(:,i)
   enddo
   wait(id=id(11))
   rewind(11)

   !smp$ paralleldo
   do i=1,10
      read (i, id=id(i)) larr3(:,i), larr4(:,i)
   enddo

   !smp$ paralleldo
   do i=1,10
      read (11, id=id(11)) larr5(:,i), larr6(:,i)
   enddo

   do i=1,10
      do j=1,10
         if (larr3(i,j) .neqv. .false.) error stop 13
         if (larr4(i,j) .neqv. .false.) error stop 14
         if (larr5(i,j) .neqv. .false.) error stop 15
         if (larr6(i,j) .neqv. .false.) error stop 16
      enddo
   enddo

   !smp$ paralleldo
   do i=1,10
      close(i)
   enddo
   close(11)

stop

100 print *, "error for open. "

end

