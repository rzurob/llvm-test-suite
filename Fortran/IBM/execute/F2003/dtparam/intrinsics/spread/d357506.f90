!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357506.f
!*
!*  DATE                       : Oct. 15 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 357506
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type first(l1)
    integer,len :: l1
    character(l1) :: c1
  end type
  type second(l2)
    integer,len :: l2
    type(first(l2)) :: first1
  end type
   type container(l1,l2)
      integer,len :: l1,l2
      type(second(l2)) :: second2(l1:l2)
   end type
end module
program d357506
  use m
  implicit none

  type(container(2,3))  :: contain1
  contain1=container(2,3)( &
          second2=[second(3)(first1=first(3)(c1="aaa") ),&
                   second(3)(first1=first(3)(c1="bbb") ) ]  )

  print *,contain1%second2(2)%first1%c1
  call verify(spread(contain1,1,3))
  contains
     subroutine verify(dt)
         type(container(*,*)),intent(in) :: dt(:)
         integer :: i

         do i=lbound(dt,1),ubound(dt,1)
            print *,dt(i)%second2(2)%first1%l1
            print *,"|",dt(i)%second2(2)%first1%c1,"|"
         end do
     end subroutine

end program

