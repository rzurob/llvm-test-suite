! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.* 
! %COMPOPTS: 
! %GROUP: fxstio003.f 
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
!*  TEST CASE TITLE            : fxstio003.f 
!*
!*  PROGRAMMER                 : Catherine Sun
!*  
!*  Creation Date              : Mar 07, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test assumed-size array and deferred-shape 
!*                               array with Stream I/O.  
!*
!=======================================================================

!* Declare Variables.
   
  implicit none
  integer i, j, ios
  integer int(3,3), int1(9)
  integer intc1(3), intc2(3), intc3(3)
  integer intc11(3), intc21(3), intc31(3)

  real, allocatable :: real1(:, :), real2(:)
  real, allocatable :: realc1(:), realc2(:), realc3(:)
  real, allocatable :: realc11(:), realc21(:), realc31(:)
 
  complex, allocatable :: comp(:, :), comp1(:)
  complex, allocatable :: compc1(:), compc2(:), compc3(:)
  complex, allocatable :: compc11(:), compc21(:), compc31(:)

  interface
      subroutine sub(int, int1, intc1, intc2, intc3, intc11, intc21, intc31)
         integer int(:,:)
         integer int1(:)
         integer intc1(:), intc2(:), intc3(:)
         integer intc11(:), intc21(:), intc31(:)
      end subroutine sub
  end interface

  allocate(real1(3,3), real2(9))
  allocate(realc1(3), realc2(3), realc3(3))
  allocate(realc11(3), realc21(3), realc31(3))
  allocate(comp(3, 3), comp1(9))
  allocate(compc1(3), compc2(3), compc3(3))
  allocate(compc11(3), compc21(3), compc31(3))



!* initialize the array int(3,3)

  do i=1,3
    do j=1,3
       int(i,j) = i * 100 + j * 10
    end do
  end do

 call sub(int, int1, intc1, intc2, intc3, intc11, intc21, intc31)

!* Test real
  do i=1,3
    do j=1,3
       real1(i,j) = real(i * 10 + j)
    end do
  end do

!* open two sequential and two stream units for synchronous I/O

  open(1, form='unformatted', access='stream', iostat=ios, err=100)
  open(2, form='unformatted', access='sequential' , iostat=ios, err=100)
  open(3, form='unformatted', access='stream', iostat=ios, err=100)
  open(4, form='unformatted', access='sequential', iostat=ios, err=100)

  realc1 = (/(real1(i,1),i=1,3)/)
  realc2 = (/(real1(i,2),i=1,3)/)
  realc3 = (/(real1(i,3),i=1,3)/)

  write (1, iostat=ios, err=200) real1 
  write (2, iostat=ios, err=200) realc1(1), realc1(2), realc1(3)
  write (3, iostat=ios, err=200) (realc2(i), i=1,3)
  write (4, iostat=ios, err=200) realc3

  rewind(1, iostat=ios, err=500)
  read(1,   iostat=ios, err=400) real2
  rewind(2, iostat=ios, err=500)
  read(2,   iostat=ios, err=400) realc11(1), realc11(2), realc11(3)
  rewind(3, iostat=ios, err=500)
  read(3,  iostat=ios, err=400) (realc21(i), i=1,3)
  rewind(4, iostat=ios, err=500)
  read(4,  iostat=ios, err=400) realc31

  do i = 1 , 9
     select case(i)
        case (:3)
           if ((real2(i) .ne. real1(i,1))   .and. &
           &  (realc1(i) .ne. realc11(i))) call zzrc(i+10)
        case (4:6)
           if ((real2(i) .ne. real1(i-3,2)) .and. &
           &  (realc2(i-3) .ne. realc21(i-3))) call zzrc(i+10)
        case (7:9)
           if ((real2(i) .ne. real1(i-6,3)) .and. &
           &  (realc3(i-6) .ne. realc31(i-6))) call zzrc(i+10)
     end select
  end do

  close(1, status='delete')
  close(2, status='delete')
  close(3, status='delete')
  close(4, status='delete')
 
!* Testing complex
    do i=1,3
    do j=1,3
       comp(i,j) = (real(i * 10 + j), real(i*j+j))
    end do
  end do

!* open two sequential and two stream units for synchronous I/O

  open(1, form='unformatted', access='stream', iostat=ios, err=100)
  open(2, form='unformatted', access='sequential' , iostat=ios, err=100)
  open(3, form='unformatted', access='stream', iostat=ios, err=100)
  open(4, form='unformatted', access='sequential', iostat=ios, err=100)

  compc1 = (/(comp(i,1),i=1,3)/)
  compc2 = (/(comp(i,2),i=1,3)/)
  compc3 = (/(comp(i,3),i=1,3)/)

  write (1,  iostat=ios, err=200) comp 
  write (2,  iostat=ios, err=200) compc1(1), compc1(2), compc1(3)
  write (3,  iostat=ios, err=200) (compc2(i), i=1,3)
  write (4,  iostat=ios, err=200) compc3

  rewind(1, iostat=ios, err=500)
  read(1,  iostat=ios, err=400) comp1 
  rewind(2, iostat=ios, err=500)
  read(2,  iostat=ios, err=400) compc11(1), compc11(2), compc11(3)
  rewind(3, iostat=ios, err=500)
  read(3,  iostat=ios, err=400) (compc21(i), i=1,3)
  rewind(4, iostat=ios, err=500)
  read(4,  iostat=ios, err=400) compc31

  do i = 1 , 9
     select case(i)
        case (:3)
           if ((comp1(i) .ne. comp(i,1))   .and. &
           &  (compc1(i) .ne. compc11(i))) call zzrc(i+20)
        case (4:6)
           if ((comp1(i) .ne. comp(i-3,2)) .and. &
           &  (compc2(i-3) .ne. compc21(i-3))) call zzrc(i+20)
        case (7:9)
           if ((comp1(i) .ne. comp(i-6,3)) .and. &
           &  (compc3(i-6) .ne. compc31(i-6))) call zzrc(i+20)
     end select
  end do

  close(1, status='delete')
  close(2, status='delete')
  close(3, status='delete')
  close(4, status='delete')
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

subroutine sub(int, int1, intc1, intc2, intc3, intc11, intc21, intc31)
  integer int(:,:)
  integer int1(:)
  integer intc1(:), intc2(:), intc3(:)
  integer intc11(:), intc21(:), intc31(:)

  open(1, form='unformatted', access='stream', iostat=ios, err=100)
  open(2, form='unformatted', access='sequential' , iostat=ios, err=100)
  open(3, form='unformatted', access='stream', iostat=ios, err=100)
  open(4, form='unformatted', access='sequential', iostat=ios, err=100)

  intc1 = (/(int(i,1),i=1,3)/)
  intc2 = (/(int(i,2),i=1,3)/)
  intc3 = (/(int(i,3),i=1,3)/)

  write (1, iostat=ios, err=200) int
  write (2, iostat=ios, err=200) intc1(1), intc1(2), intc1(3)
  write (3, iostat=ios, err=200) (intc2(i), i=1,3)
  write (4, iostat=ios, err=200) intc3

  rewind(1, iostat=ios, err=500)
  read(1, iostat=ios, err=400) int1
  rewind(2, iostat=ios, err=500)
  read(2, iostat=ios, err=400) intc11(1), intc11(2), intc11(3)
  rewind(3, iostat=ios, err=500)
  read(3, iostat=ios, err=400) (intc21(i), i=1,3)
  rewind(4, iostat=ios, err=500)
  read(4, iostat=ios, err=400) intc31

  do i = 1 , 9
     select case(i)
        case (:3)
           if ((int1(i) .ne. int(i,1))   .and. &
           &  (intc1(i) .ne. intc11(i))) call zzrc(i)
        case (4:6)
           if ((int1(i) .ne. int(i-3,2)) .and. &
           &  (intc2(i-3) .ne. intc21(i-3))) call zzrc(i)
        case (7:9)
           if ((int1(i) .ne. int(i-6,3)) .and. &
           &  (intc3(i-6) .ne. intc31(i-6))) call zzrc(i)
     end select
  end do
  close(1, status='delete')
  close(2, status='delete')
  close(3, status='delete')
  close(4, status='delete')
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

end subroutine

