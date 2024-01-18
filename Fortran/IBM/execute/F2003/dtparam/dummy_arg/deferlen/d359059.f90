!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359059.f
!*
!*  DATE                       : Nov. 18 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 359059
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type inner(l)
     integer,len  :: l
  end type

  type outer(l2)
      integer,len  :: l2
      type(inner(2*l2)) :: inner1
  end type
end module

program d359059

  use m
  implicit none

  class(outer(:)),allocatable :: obj(:)

  allocate(outer(4) :: obj(0:1))

  select type(obj)
    type is(outer(*))
       obj(0)%inner1 = inner(8)()
       obj(1)%inner1 = inner(8)()
    class default
       error stop 10_4
  end select
end
