!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyTarget01.f   
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
!*  1. If the dummy argument has the TARGET attribute, does not have the VALUE attribute, and is either a scalar or an assumed-shape array, and the corresponding actual argument has the TARGET attribute but is not an array section with a vector subscript then
!*
!* 1) Any pointers associated with the actual argument becomes associated with the corresponding dummy argument on invocation of the procedure and
!* 2) When execution of the procedure completes, any pointers that do not become undefined and are associated with the dummy argument remain associated with the actual argument
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k1,l1)
     integer,kind :: k1
     integer,len  :: l1
     integer(k1)  :: int1(l1)
   end type

   type contain(k2,l2) 
     integer,kind     :: k2
     integer(8),len   :: l2
     type(dtp(k2,:)),pointer     :: dtp1=>null()
   end type 

end module

program dummyArgDeferNonPolyTarget01
  use m
  implicit none

  type(dtp(2,:)),target,allocatable      :: dtar1
  
  type(contain(2,:)),target,allocatable  :: contain1
  type(contain(2,:)),pointer             :: pcontain1=>null()

  type(contain(2,:)),target,allocatable  :: contain2(:)
  type(contain(2,:)),pointer             :: pcontain2(:)=>null()

  dtar1=dtp(2,3)([1,2,3])

  contain1=contain(2,3)(dtar1)
  pcontain1=>contain1

  contain2=[contain(2,3)(dtar1),contain(2,3)()]

  pcontain2(-1:)=>contain2

  call associate1(contain1)

  if(.not. associated(pcontain1,contain1))               error stop 15_4
  if(pcontain1%l2 /= 3)                                  error stop 16_4
  if(.not. associated(pcontain1%dtp1,dtar1))             error stop 17_4
  if(pcontain1%dtp1%l1 /= 3)                             error stop 18_4
  if(any(pcontain1%dtp1%int1 /= [-1,-2,-3]))             error stop 19_4

  call associate2(contain2)

  if(.not. associated(pcontain2,contain2))               error stop 28_4
  if(pcontain2%l2 /= 3)                                  error stop 29_4
  if(lbound(pcontain2,1) /= -1)                          error stop 30_4
  if(ubound(pcontain2,1) /= 0)                           error stop 31_4
  if(.not. associated(pcontain2(-1)%dtp1,dtar1))         error stop 32_4
  if(.not. associated(pcontain2(0)%dtp1))                error stop 33_4
  if(pcontain2(-1)%dtp1%l1 /= 3)                         error stop 34_4
  if(pcontain2(0)%dtp1%l1 /= 3)                          error stop 35_4
  if(any(pcontain2(-1)%dtp1%int1 /= [4,5,6]))            error stop 36_4
  if(any(pcontain2(0)%dtp1%int1 /= [-4,-5,-6]))          error stop 37_4

  call associate3(dtar1)

  if(pcontain1%dtp1%l1 /= 3)                             error stop 41_4
  if(any(pcontain1%dtp1%int1 /= [7,8,9]))                error stop 42_4
 
  contains

     subroutine associate1(arg)
        type(contain(2,:)),target,allocatable :: arg
 
        if(.not. associated(pcontain1,arg))              error stop 10_4
        if(arg%l2 /= 3)                                  error stop 11_4
        if(.not. associated(arg%dtp1,dtar1))             error stop 12_4
        if(arg%dtp1%l1 /= 3)                             error stop 13_4
        if(any(arg%dtp1%int1 /= [1,2,3]))                error stop 14_4

        arg%dtp1%int1=[-1,-2,-3]
         
     end subroutine
   
     subroutine associate2(arg)
        type(contain(2,:)),target,allocatable :: arg(:)

        if(.not. associated(pcontain2,arg))              error stop 20_4
        if(arg%l2 /= 3)                                  error stop 21_4
        if(lbound(arg,1) /= 1)                           error stop 22_4
        if(ubound(arg,1) /= 2)                           error stop 23_4
        if(.not. associated(arg(1)%dtp1,dtar1))          error stop 24_4
        if(associated(arg(2)%dtp1))                      error stop 25_4 
        if(arg(1)%dtp1%l1 /= 3)                          error stop 26_4
        if(any(arg(1)%dtp1%int1 /= [-1,-2,-3]))          error stop 27_4

        arg(1)%dtp1%int1=[4,5,6]
        allocate(arg(2)%dtp1,source=dtp(2,3)([-4,-5,-6]) )

     end subroutine   

     subroutine associate3(arg)
        type(dtp(2,:)),target,allocatable :: arg
        if(.not. associated(pcontain1%dtp1,arg))         error stop 38_4
        if(arg%l1 /= 3)                                  error stop 39_4
        if(any(arg%int1 /= [4,5,6]))                     error stop 40_4

        arg%int1=[7,8,9]

     end subroutine
end program
