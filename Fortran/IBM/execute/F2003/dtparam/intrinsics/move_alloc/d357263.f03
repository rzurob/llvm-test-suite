!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 8 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 357263
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type B(l1)
     integer,len   :: l1
     procedure(fun2),nopass,pointer :: procptr=>null()
  end type
  contains
     function fun2(dt)
        type(B(*)),intent(in) :: dt
        type(B(dt%l1)) :: fun2
         print *,"in fun2"
         fun2=dt
     end function

end module

program d357263

  use m
  implicit none

  type(B(:)),allocatable   :: b1,from1,to1

  allocate(b1,source= B(2)())
  b1%procptr=>fun2

  allocate(from1,source=b1)

  to1=from1%procptr(b1)

  if(.not. allocated(to1))                       error stop 1
  if(to1%l1 /= 2)                                error stop 2
  if(.not. associated(to1%procptr,fun2))         error stop 3

end program
