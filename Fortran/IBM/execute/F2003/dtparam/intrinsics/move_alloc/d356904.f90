!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356904.f
!*
!*  DATE                       : Oct. 1 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 356904
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l1)
      integer,kind :: k
      integer,len  :: l1
   end type
   type container(l2)
      integer,len :: l2
      type(dtp(4,l2))    :: dtp3
   end type
end module

program d356904
  use m
  implicit none

  type(container(2)),allocatable   :: contain1
  type(container(:)),allocatable   :: contain2

  allocate(contain1,source=container(2)(dtp(4,2)()) )
  print *,contain1%dtp3%l1,contain1%l2
  call move_alloc(from=contain1,to=contain2)
  print *,contain2%dtp3%l1,contain2%l2

  end program

