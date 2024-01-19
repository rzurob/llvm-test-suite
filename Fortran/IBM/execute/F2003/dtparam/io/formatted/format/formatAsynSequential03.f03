!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 24 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. test asynchronous READ & WRITE with sequential access and non-advancing IO
!* 2. derived type have multiple levels of derived type components
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type inner1(k1,l1)
     integer,kind :: k1 ! k1=4
     integer,len  :: l1 ! l1=4
     sequence
     integer(k1)  :: i1(l1)
  end type
  type inner2(k2,l2)
     integer,kind :: k2 ! k2=2
     integer,len  :: l2 ! l2=3
     sequence
     logical(k2)  :: g1(l2)
     type(inner1(2*k2,l2+1)) :: in1
  end type

  type outer(k3,l3)
     integer,kind :: k3 ! k3=2
     integer,len  :: l3 ! l3=3
     sequence
     character(k3+l3) :: c1
     type(inner2(k3,l3)) :: in2
  end type

end module

program formatAsynSequential03
  use m
  implicit none

  integer :: ios,i,idvar(10)
  character(256) :: msg

  type(outer(2,:)),allocatable :: out(:)

  open(10,status='scratch',form='formatted',action='readwrite',&
         access='sequential',asynchronous='yes',recl=100,&
         position='rewind',iostat=ios,iomsg=msg)


  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 11
  end if

  ! trigger asynchornous write
  do i=1,10
     write(10,'(a5,3l2,4i4/)',asynchronous='yes',&
          id=idvar(i),advance='no')  &
         outer(2,3)(c1="hello",in2=inner2(2,3)( &
          g1=[.true.,.false.,.true.],in1=inner1(4,4)(i1=[11,12,-13,-14]) ))
  end do

  ! execuate other statement when data in pending
  allocate(outer(2,3) :: out(10))

  do i=1,10
   ! wait specified transferred data to be finished
    wait(10,id=idvar(i))
  end do

  rewind 10

  do i=1,10

   read(10,'(a2,tr3,3l2,4(tr3,i1)/)',asynchronous='yes',&
        id=idvar(i),advance='no') out(i)
  end do

  do i=1,10
   ! wait specified transferred data to be finished
    wait(10,id=idvar(i))

  end do

  write(*,'(a5,3l2,4i4)') out

  close(10)


end program
