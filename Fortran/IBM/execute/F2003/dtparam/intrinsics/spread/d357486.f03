!*********************************************************************
!*  ===================================================================
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
!*  1. DEFECT 357486
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type second(l3)
    integer,len :: l3
  end type
   type container(l1,l2)
      integer,len :: l1,l2
      type(second(l2)) :: second2
   end type
end module
program d357486
  use m
  implicit none

  type(container(2,3)),target  :: contain1
  contain1=container(2,3)(second2= second(3)())

  if(contain1%l1 /= 2)                          error stop 1
  if(contain1%l2 /= 3)                          error stop 2
  if(contain1%second2%l3 /= 3)                  error stop 3

  call verify(contain1)

  contains
   subroutine verify(dt)
      type(container(*,*)),intent(in) :: dt
        if(dt%l1 /= 2)                          error stop 4
        if(dt%l2 /= 3)                          error stop 5
        if(dt%second2%l3 /= 3)                  error stop 6

   end subroutine

end program

