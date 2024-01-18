!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyTarget04.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 7 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  TEST POINTER ASSOCIATION WITH ACTUAL OR DUMMY ARGUMENT THROGH FUNCTION RESULT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
     integer,len   :: l1
     character(l1) :: c1 
   end type
   type B(l2)
     integer,len  :: l2
     type(A(:)),allocatable :: a1(:)
   end type
end module

program dummyArgDeferNonPolyTarget04
  use m
  implicit none
  
  type(B(:)),target,allocatable :: b1
  type(B(:)),pointer            :: ptr1

  type(A(3)) :: a1(2)=[A(3)("xlf"),A(3)("xlc")]


  b1=B(3)([A(3)("xlf"),A(3)("xlc")] )

  ptr1=>getDT1(b1)

  call associate1(b1)

  ptr1=>getDT2(b1)

  if(.not. associated(ptr1,b1))                             error stop 14_4
  if(ptr1%l2 /= 3)                                          error stop 15_4
  if(ptr1%a1%l1 /= 3)                                       error stop 16_4
  if(any(ptr1%a1%c1 /= ["12","34"]))                        error stop 17_4

  call associate2(b1)

  if(.not. associated(ptr1,b1))                             error stop 18_4
  if(ptr1%l2 /= 3)                                          error stop 19_4
  if(ptr1%a1%l1 /= 3)                                       error stop 20_4
  if(any(ptr1%a1%c1 /= ["00","11"]))                        error stop 21_4

  contains

    function getDT1(dt)
      type(B(:)),target,allocatable :: dt
      type(B(:)),pointer            :: getDT1
      
      getDT1=>dt
    end function
   
    subroutine associate1(arg)
       type(B(:)),target,allocatable :: arg

       if(.not. associated(ptr1,arg))                       error stop 10_4
       if(ptr1%l2 /= 3)                                     error stop 11_4
       if(ptr1%a1%l1 /= 3)                                  error stop 12_4
       if(any(ptr1%a1%c1 /= ["xlf","xlc"]))                 error stop 13_4
    end subroutine 

    function getDT2(arg)
       type(B(:)),target,allocatable :: arg
       type(B(:)),pointer :: getDT2 

       arg%a1=[A(3)("12"),A(3)("34")]
 
       getDT2=>arg
    end function 

    subroutine associate2(arg)
       type(B(:)),target,allocatable :: arg

       arg%a1=[A(3)("00"),A(3)("11")]
       ptr1=>getDT1(arg)
    end subroutine

end program
