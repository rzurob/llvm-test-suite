!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectIntCompRead01.f
!*
!*  DATE                       : Jan. 14 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test Read statement when ultimate components are integer
!* 2. Input values are different form
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
  type int1(k1,l1)
     integer,kind :: k1 ! k1=2
     integer,len  :: l1 ! l1=3
     sequence
     integer(k1) :: i1(l1)
  end type
  type int2(k2,l2)
     integer,kind :: k2 ! k2=4
     integer,len  :: l2 ! l2=4
     sequence
     integer(k2/2) :: i2(l2/2)
     type(int1(k2/2,l2-1)) :: icomp1
  end type
end module

module m2
use m1
  type int3(k3,l3)
     integer, kind :: k3 ! k3=8
     integer,len   :: l3 ! l3=5
     sequence
     integer(k3/2) :: i3(l3)
     type(int2(k3/2,l3-1)) :: icomp2
  end type

  contains

      subroutine read(unit,ptr1,ptr2,tar)
         integer,intent(in) :: unit
         type(int3(8,:)),pointer,intent(inout) :: ptr1(:)
         type(int3(8,:)),pointer,optional,intent(inout) :: ptr2(:)
         type(int3(8,*)),target,intent(inout) :: tar(:)

         ! semicolon is value separator in place of comma when decimal edit mode is comma
         read(unit,fmt=*) tar(lbound(tar,1))

         read(unit,*,decimal='point') tar(ubound(tar,1))

         if(present(ptr2)) then
            ptr1(0:)=>tar(lbound(tar,1):ubound(tar,1)-1)
            ptr2(ubound(tar,1):)=>tar(ubound(tar,1):ubound(tar,1))
         else
            ptr1(0:)=>tar
         end if
      end subroutine
end module



program listDirectIntCompRead01
  use m2
  implicit none

  integer :: ios,i
  character(256) :: msg

  type(int3(8,:)),allocatable,target :: tar(:)

  type(int3(8,:)),pointer :: ptr1(:)=>null(),ptr2(:)=>null()

  allocate(int3(8,5) :: tar(-1:0) )

  !initialize to default value
  do i=-1,0

    tar(i)%i3=-99
    tar(i)%icomp2%i2=-99
    tar(i)%icomp2%icomp1%i1=-99

  end do

  open(10,file='listDirectIntCompRead01.dat',decimal='comma',&
       status='old',iostat=ios,iomsg=msg)


  if( ios <> 0)  then
      print *,"fail to open the file"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 10
  end if

  ! following are the inputs we want to read:

  !;1234 1*; ; 1*31  ;
  !-7341 +231  ; 0 ; 2*-11 unread value /
  !,2*12,    ,1*0,-000,+101,1*99,2*-1,unread value

  !ptr2 is not present
  call read(10,ptr1,tar=tar)

  ! output data for verification
  do i=lbound(ptr1,1),ubound(ptr1,1)
     write(*,*) ptr1(i)%i3
     write(*,*) ptr1(i)%icomp2%i2
     write(*,*) ptr1(i)%icomp2%icomp1%i1
  end do

  rewind 10

  !ptr2 is present
  call read(10,ptr1,ptr2,tar)

  ! output data for verification
  do i=lbound(ptr1,1),ubound(ptr1,1)
     write(*,*) ptr1(i)%i3
     write(*,*) ptr1(i)%icomp2%i2
     write(*,*) ptr1(i)%icomp2%icomp1%i1
  end do

  do i=lbound(ptr2,1),ubound(ptr2,1)
     write(*,*) ptr2(i)%i3
     write(*,*) ptr2(i)%icomp2%i2
     write(*,*) ptr2(i)%icomp2%icomp1%i1
  end do

  close(10)

end program
