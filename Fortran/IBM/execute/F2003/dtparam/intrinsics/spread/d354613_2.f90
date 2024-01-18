!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d354613_2.f
!*
!*  DATE                       : Oct. 28 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 354613
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
  type first(l2)
    integer,len :: l2
    character(l2) :: c1
  end type
   type container(l1)
      integer,len :: l1
      type(first(l1))  :: first2(l1)
   end type
end module

program d354613_2
  use m1
  implicit none

  type(container(2)),target  :: contain1

  contain1=container(2)([first(2)(c1="123"),first(2)(c1="456")] )
  print *,contain1%first2%c1
  call verify(spread(contain1,1,3))
  contains
     subroutine verify(dt)
         type(container(*)),intent(in) :: dt(:)
         integer :: i
         do i=1,3
            print *,dt(i)%first2%c1 /= ["12","45"]
            print *,dt(i)%first2%c1
         end do
     end subroutine
end program
