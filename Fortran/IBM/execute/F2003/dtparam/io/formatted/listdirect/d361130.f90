!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361130.f
!*
!*  DATE                       : Jan. 24 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type DT1(l2)
     integer,len  :: l2
     real :: r(l2)
  end type
  contains
    function getData(dt)
       type(DT1(*)) :: dt(:)
       type(DT1(:)),allocatable :: getData(:)
       getData=dt
    end function
end module

program d361130

  use m

  type(DT1(1)),allocatable :: d1(:)
  type(DT1(1)) :: t1(2)
  logical,external :: precision_r4

  do i=1,2
     t1(i)%r=0.1
  end do

  allocate(d1(2),source=getData(t1))

  if(.not. precision_r4(d1(1)%r,0.1))         stop 1
  if(.not. precision_r4(d1(2)%r,0.1))         stop 2


end program
