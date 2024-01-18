!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyOptional02.f
!*
!*  DATE                       : Nov. 21 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  use optional dummy argument as actual argument of spread intrinsic, and result of spread assign to function result.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
       integer,len   :: l1
   end type

   type,extends(base) :: child(l2)
       integer,len    :: l2
       integer        :: i(l1:l2)
   end type

   contains

        function fun1(arg1,arg2)
            class(base(:)),pointer,optional :: arg1
            class(base(:)),pointer,optional :: arg2
            class(base(:)),pointer          :: fun1(:)

            if(present(arg1)) then
               allocate(fun1(2),source=spread(arg1,1,2))
            else if(present(arg2)) then
               allocate(fun1(4),source=spread(arg2,1,4))
            else
               allocate(fun1(1),source=child(3,4)([11,22]))
            end if

        end function
end module

program dummyArgDeferPolyOptional02
  use m
  implicit none

  integer :: i

  class(base(:)),pointer :: pbase1=>null(),pbase2=>null(),pbase3(:)=>null()

  allocate(pbase1,source=child(2,3)(i=[1,2]))

  allocate(pbase2,source=child(4,6)(i=[-1,-2,-3]))

  allocate(pbase3(2:3),source=fun1(pbase1))

  select type(pbase3)
     class is(child(*,*))
        do i=2,3
          if(any(pbase3(i)%i /= [1,2]))                error stop 10_4
        end do
     class default
        error stop 50_4
  end select

  pbase3(-1:)=>fun1(arg2=pbase2)

  select type(pbase3)
     class is(child(*,*))
            do i=-1,2
               if(any(pbase3(i)%i /= [-1,-2,-3]))      error stop 11_4
            end do
     class default
        error stop 51_4
  end select

  pbase3=>fun1()

  select type(pbase3)
     class is(child(*,*))
         if(size(pbase3,1) /= 1)                       error stop 12_4
         if(any(pbase3(1)%i /= [11,22]))               error stop 13_4
     class default
         error stop 52_4
  end select
end program
