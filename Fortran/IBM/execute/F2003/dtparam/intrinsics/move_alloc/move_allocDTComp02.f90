!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocDTComp02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 1 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. DERIVED TYPE HAS DT ARRAY COMPONENT WHICH HAS CHARACTER ARRAY COMPONENT,THEY ARE ALL ALLOCATABLE
!*  3. CALL MOVE_ALLOC IN SUBROUTINE 
!*  4. DUMMY ARGUMENT HAS ASSUMED LENGTH PARAMETER
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type A(l1)
       integer,len :: l1
       character(l1),allocatable :: c1(:)
    end type  
    type B(l2)
       integer,len :: l2
       type(A(l2+l2)),allocatable :: a1(:)
    end type
    contains
    subroutine sub2(arg2)
        class(*),allocatable    :: arg2(:)
        
         if(.not. allocated(arg2))                            error stop 23_4
         select type(arg2)
            type is(B(*))
             if(arg2%l2 /= 3)                                 error stop 24_4
             if(arg2(1)%a1%l1 /= 6)                           error stop 26_4
             if(arg2(2)%a1%l1 /= 6)                           error stop 27_4
             if(arg2(1)%a1(1)%c1%len /= len(arg2(1)%a1(1)%c1)) error stop 108_4
             if(arg2(1)%a1(1)%c1%len /= 6)                     error stop 28_4
             if(arg2(2)%a1(1)%c1%len /= len(arg2(2)%a1(1)%c1)) error stop 109_4
             if(arg2(2)%a1(1)%c1%len /= 6)                     error stop 29_4
             if(any(arg2(1)%a1(1)%c1 /= ["123","456"]))       error stop 30_4
             if(any(arg2(1)%a1(2)%c1 /= ["aaa","bbb"]))       error stop 31_4
             if(any(arg2(2)%a1(1)%c1 /= ["000","111"]))       error stop 32_4
             if(any(arg2(2)%a1(2)%c1 /= ["ccc","ddd"]))       error stop 33_4
         end select
    end subroutine
 
end module

program move_allocDTComp02

  use m
  implicit none

  type(B(3)),allocatable :: b1(:)
  type(B(:)),allocatable :: b2(:)
  class(*),allocatable   :: b3(:)
  
  allocate(b1(2))

  b1(1)%a1=[A(6)(["123","456"]),A(6)(["aaa","bbb"])]
  b1(2)%a1=[A(6)(["000","111"]),A(6)(["ccc","ddd"])]

  call move_alloc (b1, b2)
  call sub1(b2)

  allocate(b1(2))

  b1(1)%a1=[A(6)(["123","456"]),A(6)(["aaa","bbb"])]
  b1(2)%a1=[A(6)(["000","111"]),A(6)(["ccc","ddd"])]

  call move_alloc(b1, b3)

  call sub2(b3)

  contains 

    subroutine sub1(arg2)
        type(B(:)),allocatable :: arg2(:)
          
         if(.not. allocated(arg2))                        error stop 11_4
         if(arg2%l2 /= 3)                                 error stop 12_4
         if(arg2%l2 /= 3)                                 error stop 13_4
         if(arg2(1)%a1%l1 /= 6)                           error stop 14_4
         if(arg2(2)%a1%l1 /= 6)                           error stop 15_4
         if(arg2(1)%a1(1)%c1%len /= len(arg2(1)%a1(1)%c1)) error stop 106_4
         if(arg2(1)%a1(1)%c1%len /= 6)                     error stop 16_4
         if(arg2(2)%a1(1)%c1%len /= len(arg2(2)%a1(1)%c1)) error stop 107_4
         if(arg2(2)%a1(1)%c1%len /= 6)                    error stop 17_4
         if(any(arg2(1)%a1(1)%c1 /= ["123","456"]))       error stop 18_4
         if(any(arg2(1)%a1(2)%c1 /= ["aaa","bbb"]))       error stop 19_4
         if(any(arg2(2)%a1(1)%c1 /= ["000","111"]))       error stop 20_4
         if(any(arg2(2)%a1(2)%c1 /= ["ccc","ddd"]))       error stop 21_4
    end subroutine
end program

