
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocProcPtrComp03.f
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
!*  2. FROM AND TO ARE POLYMORPHIC TYPE
!*  3. PARENT AND CHILD BOTH HAVE PROCEDURE POINTER COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
    integer,len :: l1
    integer :: i1(l1) ! l1=2
    procedure(fun1),nopass,pointer :: procptr1=>null()
  end type
  type,extends(base) :: child(l2)
    integer,len  :: l2
    integer :: i2(l2)  ! l2=3
    procedure(fun2),nopass,pointer :: procptr2=>null()
  end type

  contains
    function fun1(int)
       integer :: int,fun1
       fun1=int
    end function

    function fun2(dt)
       class(base(*)),intent(in) :: dt
       class(base(dt%l1)),allocatable :: fun2

       select type(dt)
         type is(child(*,*))
            allocate(fun2,source=dt)
         class default
            error stop 100_4
       end select
    end function
end module

program move_allocProcPtrComp03

  use m
  implicit none

  class(base(:)),allocatable   :: from1
  type(child(:,:)),allocatable :: child1
  class(base(:)),allocatable   :: to1
  integer :: result1
  class(*),allocatable :: result2

  allocate(child1,source=child(2,3)(i1=[1,2],i2=[3,4,5]))

  child1%procptr1=>fun1
  child1%procptr2=>fun2

  allocate(from1,source=child1)

  if(.not. allocated(from1))                              error stop 10_4
  select type(from1)
     type is(child(*,*))
       if(from1%l1 /= 2)                                  error stop 11_4
       if(from1%l2 /= 3)                                  error stop 12_4
       if(any(from1%i1 /= [1,2]))                         error stop 13_4
       if(any(from1%i2 /= [3,4,5]))                       error stop 14_4
       if(size(from1%i1,1) /= 2)                          error stop 15_4
       if(size(from1%i2,1) /= 3)                          error stop 16_4
       if(.not. associated(from1%procptr1,fun1))          error stop 17_4
       if(.not. associated(from1%procptr2,fun2))          error stop 18_4
     class default
       error stop 101_4
  end select

  call move_alloc(from1,to1)

  if(allocated(from1))                                    error stop 19_4

  select type(to1)
     type is(child(*,*))
       if(to1%l1 /= 2)                                    error stop 20_4
       if(to1%l2 /= 3)                                    error stop 21_4
       if(any(to1%i1 /= [1,2]))                           error stop 22_4
       if(any(to1%i2 /= [3,4,5]))                         error stop 23_4
       if(size(to1%i1,1) /= 2)                            error stop 24_4
       if(size(to1%i2,1) /= 3)                            error stop 25_4
       if(.not. associated(to1%procptr1,fun1))            error stop 26_4
       if(.not. associated(to1%procptr2,fun2))            error stop 27_4

       result1=to1%procptr1(5)
       allocate(result2,source=to1%procptr2(to1))

       if(result1 /= 5)                                   error stop 28_4
       select type(x=>result2)
           type is(child(*,*))

            if(x%l1 /= 2)                                 error stop 29_4
            if(x%l2 /= 3)                                 error stop 30_4
            if(any(x%i1 /= [1,2]))                        error stop 31_4
            if(any(x%i2 /= [3,4,5]))                      error stop 32_4
            if(size(x%i1,1) /= 2)                         error stop 33_4
            if(size(x%i2,1) /= 3)                         error stop 34_4
            if(.not. associated(x%procptr1,fun1))         error stop 35_4
            if(.not. associated(x%procptr2,fun2))         error stop 36_4
           class default
            error stop 102_4
       end select
      class default
          error stop 103_4
  end select

end program

