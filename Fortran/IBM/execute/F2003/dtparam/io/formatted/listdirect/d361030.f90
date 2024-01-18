!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361030.f
!*
!*  DATE                       : Jan. 16 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type int1(l1)
     integer,len  :: l1 ! l1=3
     integer :: i1(l1)
  end type
  type int2(l2)
     integer,len  :: l2 ! l2=4
     integer :: i2(l2/2)
     type(int1(l2-1)) :: icomp1
  end type
  type int3(l3)
     integer,len   :: l3 ! l3=5
     integer :: i3(l3)
     type(int2(l3-1)) :: icomp2
  end type
  contains
      subroutine read(unit,obj)
         integer,intent(in) :: unit
         type(int3(*)),intent(inout) :: obj
         print *,obj
         read(unit,fmt=*) obj
         print *,obj
      end subroutine
end module

program d361030

  use m
  implicit none

  integer :: i
  type(int3(5)),allocatable :: obj

  allocate(obj)

  obj%i3=-99
  obj%icomp2%i2=-99
  obj%icomp2%icomp1%i1=-99

  print *,obj
  open(10,file='d361030.dat',decimal='comma')

  call read(10,obj)

  close(10)

end program

