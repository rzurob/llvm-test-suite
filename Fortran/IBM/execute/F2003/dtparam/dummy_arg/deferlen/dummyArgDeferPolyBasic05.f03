!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 27 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  test derived type with procedure pointer component which associate with function with pointer or target dummy argument
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
    integer,len :: l1
    integer  :: i1(l1)
    procedure(fun1),nopass,pointer :: procbase=>null()
  end type

  type,extends(base) :: child(l2)
    integer,len :: l2
    integer :: i2(l1:l2)
    procedure(fun2),nopass,pointer   :: procchild=>null()
  end type

  contains

     function fun1(arg)
        class(base(:)),pointer,intent(in) :: arg
        class(base(:)),pointer :: fun1
        fun1=>arg
        fun1%i1=arg%i1*2
     end function

     function fun2(arg)
        class(base(*)),target,intent(in) :: arg
        class(base(:)),pointer :: fun2

        fun2=>arg
        select type(arg)
           type is(child(*,*))
              select type(fun2)
                 type is(child(*,*))
                    fun2%i2 = arg%i2 * 2
                 class default
                    error stop 100_4
              end select
           class default
              error stop 101_4
        end select
     end function
end module

program dummyArgDeferPolyBasic05
  use m
  implicit none

  class(base(:)),pointer :: base1

  allocate(base1,source=child(2,4)(i1=[1,2],i2=[-3,-4,-5]))

  base1%procbase=>fun1

  if(.not. associated(base1%procbase,fun1))           error stop 10_4

  if(.not. associated(base1%procbase(base1),base1))   error stop 11_4

  if(any(base1%i1 /= [2,4]))                          error stop 12_4
  associate(x=>base1%procbase(base1))
     if(x%l1 /= 2)                                    error stop 13_4
     if(any(x%i1 /= [4,8]))                           error stop 14_4
  end associate

  select type(base1)
      type is(child(*,*))
         base1%procchild=>fun2
         if(.not. associated(base1%procchild,fun2))   error stop 15_4
         if(.not. associated( base1%procchild(base1),base1)) &
                  error stop 16_4
         associate(x=>base1%procchild(base1))
            select type(x)
               type is(child(*,*))
                  if(any(x%i2 /= [-12,-16,-20]))      error stop 17_4
               class default
                  error stop 102_4
            end select
         end associate
       class default
          error stop 103_4
  end select

end program
