!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyTarget10.f
!*
!*  DATE                       : Nov. 20 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. function result is a pointer with deferred length parameter
!*  2. pass function result as actual argument, or dummy argument pointes to function result.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len   :: l1
      character(l1) :: c1
   end type

   type,extends(base) :: child(l2)
      integer,len   :: l2
      character(l2) :: c2
   end type

   contains

     function gettype1(dt)
       type(child(*,*)),target,intent(in) :: dt(:)
       class(base(:)),pointer             :: gettype1(:)

       gettype1=>dt
     end function

end module

program dummyArgDeferPolyTarget10
  use m
  implicit none

  class(base(:)),pointer :: pbase1(:)=>null(),pbase2(:)=>null()

  type(child(3,4)),target :: tar(3)

  tar=[child(3,4)(c1="red",c2="blue"),&
       child(3,4)(c1="123",c2="4567"), &
       child(3,4)(c1="xlf",c2="test")]

  call check1(gettype1(tar))

  call check2(pbase2)

  if(lbound(pbase2,1) /= 2)                    error stop 20_4
  if(ubound(pbase2,1) /= 4)                    error stop 21_4
  select type(pbase2)
     class is(child(*,*))
       if(pbase2(2)%c1 /= "red")               error stop 22_4
       if(pbase2(2)%c2 /= "blue")              error stop 23_4
       if(pbase2(3)%c1 /= "123")               error stop 24_4
       if(pbase2(3)%c2 /= "4567")              error stop 25_4
       if(pbase2(4)%c1 /= "xlf")               error stop 26_4
       if(pbase2(4)%c2 /= "test")              error stop 27_4
     class default
       error stop 51_4
  end select
  contains

     subroutine check1(arg)

       class(base(:)),pointer,intent(in) :: arg(:)

       if(lbound(arg,1) /= 1)                  error stop 10_4
       if(ubound(arg,1) /= 3)                  error stop 11_4
       select type(arg)
         type is(child(*,*))
            if(arg%l1 /= 3)                    error stop 12_4
            if(arg%l2 /= 4)                    error stop 13_4
            if(arg(1)%c1 /= "red")             error stop 14_4
            if(arg(1)%c2 /= "blue")            error stop 15_4
            if(arg(2)%c1 /= "123")             error stop 16_4
            if(arg(2)%c2 /= "4567")            error stop 17_4
            if(arg(3)%c1 /= "xlf")             error stop 18_4
            if(arg(3)%c2 /= "test")            error stop 19_4
         class default
            error stop 50_4
       end select

     end subroutine

     subroutine check2(arg)

       class(base(:)),pointer,intent(out) :: arg(:)

       arg(2:)=>gettype1(tar)

     end subroutine

end program
