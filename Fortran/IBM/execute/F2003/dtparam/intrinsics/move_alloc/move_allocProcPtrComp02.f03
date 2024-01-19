
!*  ===================================================================
!*
!*  DATE                       : Oct. 8 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. DERIVED TYPE HAS PROCEDURE POINTER COMPONENT,WHICH POINTS TO FUNCTION
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type B(l1)
     integer,len   :: l1
     integer       :: i1(l1)
     procedure(fun),nopass,pointer :: procptr=>null()
  end type
  contains

     function fun(dt)
        type(B(*)),intent(in) :: dt
        type(B(dt%l1)) :: fun
         print *,"in fun"
         fun=dt
     end function

end module

program move_allocProcPtrComp02

  use m
  implicit none

  type(B(:)),allocatable   :: b1,from1,to1,result1
  type(B(:)),allocatable   :: b2(:),from2(:),to2(:)

  allocate(b1,source= B(2)([4,5]))
  b1%procptr=>fun

  allocate(from1,source=b1)

  if(.not. associated(from1%procptr,fun))           error stop 10_4

  call move_alloc(from1,to1)

  if(allocated(from1))                              error stop 11_4
  if(.not. allocated(to1))                          error stop 12_4
  if(to1%l1 /= 2)                                   error stop 13_4
  if(any(to1%i1 /= [4,5]))                          error stop 14_4
  if(size(to1%i1,1) /= 2)                           error stop 15_4
  if(.not. associated(to1%procptr,fun))             error stop 16_4

  result1=to1%procptr(b1)

  if(.not. allocated(result1))                      error stop 17_4
  if(result1%l1 /= 2)                               error stop 18_4
  if(any(result1%i1 /= [4,5]))                      error stop 19_4
  if(size(result1%i1,1) /= 2)                       error stop 20_4
  if(.not. associated(result1%procptr,fun))         error stop 21_4

  allocate(b2(2),source=[B(2)([-1,-2]),B(2)([-3,-4])])

  b2(1)%procptr=>fun
  b2(2)%procptr=>fun

  allocate(from2(-1:0),source=b2)

  call move_alloc(from2,to2)

  if(allocated(from1))                              error stop 22_4
  if(.not. allocated(to1))                          error stop 23_4
  if(lbound(to2,1) /= -1)                           error stop 24_4
  if(ubound(to2,1) /= 0)                            error stop 25_4
  if(.not. associated(to2(-1)%procptr,fun))         error stop 26_4
  if(.not. associated(to2(0)%procptr,fun))          error stop 27_4
  if(to2%l1 /= 2)                                   error stop 28_4
  if(any(to2(-1)%i1 /= [-1,-2]))                    error stop 29_4
  if(any(to2(0)%i1 /= [-3,-4]))                     error stop 30_4

end program

