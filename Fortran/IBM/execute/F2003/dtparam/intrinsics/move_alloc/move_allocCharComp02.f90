!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocCharComp02.f
!*
!*  DATE                       : Oct. 3 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. PARENT TYPE HAS CHARACTER COMPONENT,CHILD TYPE HAS CHARACTER COMPONENT AND DERIVED TYPE POINTER
!*  3. DEFECT 357082
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1,l2)
      integer,len :: l1
      integer,len :: l2
      character(l1+l2),pointer :: ch1=>null()
      character(2*(l1+l2)),allocatable :: ch2
   end type
   type,extends(base) :: child(l3)
      integer,len :: l3
      character(l1+l2+l3) :: ch3
      class(base(l1,l2)),pointer :: baseComp=>null()
   end type
end module

program move_allocCharComp02

  use m
  implicit none

  class(base(1,2)),target,allocatable :: from1
  class(base(:,:)),target,allocatable :: to1


  allocate(child(1,2,3) :: from1)
  allocate(from1%ch1,source="xlf")
  from1%ch2="test"

  if(.not. allocated(from1))                           error stop 10_4
  select type(from1)
     type is(child(*,*,*))

        from1%ch3="fortran"
        allocate(from1%baseComp,source=from1)    ! defect 357082
        if(.not. associated(from1%baseComp))           error stop 11_4
     class default
         error stop 100_4
  end select

  call move_alloc(from1,to1)

  if(allocated(from1))                                 error stop 12_4
  if(.not. allocated(to1))                             error stop 13_4

  select type(to1)
     type is(child(*,*,*))
        if(to1%l1 /= 1)                                error stop 14_4
        if(to1%l2 /= 2)                                error stop 15_4
        if(to1%l3 /= 3)                                error stop 16_4
        if(to1%ch1 /= "xlf")                           error stop 17_4
        if(to1%ch1%len /= 3)                           error stop 18_4
        if(to1%ch2 /= "test")                          error stop 19_4
        if(to1%ch2%len /= 6)                           error stop 20_4
        if(to1%ch3 /= "fortra")                       error stop 21_4
        if(to1%ch3%len /= 6)                           error stop 22_4
        if(.not. associated(to1%baseComp))             error stop 23_4
        select type(x=>to1%baseComp)
            type is(child(*,*,*))
               if(x%l1 /= 1)                           error stop 24_4
               if(x%l2 /= 2)                           error stop 25_4
               if(x%l2 /= 2)                           error stop 26_4
               if(x%ch1 /= "xlf")                      error stop 27_4
               if(x%ch2 /= "test")                     error stop 28_4
               if(x%ch3 /= "fortra")                  error stop 29_4
             class default
                error stop 102_4
        end select
     class default
         error stop 101_4
  end select

end program

