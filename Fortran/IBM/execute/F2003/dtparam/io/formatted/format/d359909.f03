!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 9 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  defect 359909
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
    integer,len  :: l1
    integer      :: i1(l1-1:l1+1)
  end type
  type,extends(base) :: child(l2)
     integer,len  :: l2
     integer      :: i2(l1:l2)
  end type
end module

program d359909
  use m
  implicit none
  class(base(3)),target,allocatable :: tbase1(:)
  allocate(child(3,4) :: tbase1(-1:0))

  open(10,file="d359909.in",action='read',access='sequential',&
       form='formatted',blank='zero')

  select type(tbase1)
    type is(child(*,*))
        read(10,fmt='(3i5/2i5.3,bn,/3i5.1/2i5)') tbase1
        print *,tbase1
    class default
        stop
  end select

  close(10)

end
