!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryDTAssign02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : August 23 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3 
!* 2. TYPE PARAMETER INQUIRY
!* 3. INTRINSIC ASSIGNMENT
!* 4. DERIVED TYPE HAS ALLOCATABLE OR POINTER COMPONENT 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l)
      integer,len  :: l
      character(:),allocatable   :: c1(:)
      character(:),pointer       :: c2=>null() 
   end type
end module

program dtParameterInquiryDTAssign02 
  use m
  implicit none

  type(A(3)),target :: a1
  type(A(3))        :: a2
  type(A(:)),allocatable :: a3
  type(A(:)),pointer     :: a4
  type(A(3)),target  :: a5(2)
  type(A(:)),allocatable :: a6(:)
  type(A(:)),pointer     :: a7(:)


  a1%c1=['1230','4560','7890']
  allocate(character(2*a1%l) :: a1%c2)
  a1%c2="xlftest"
  a2=a1
  
  if(a2%l /= 3)                                      error stop 10_4
  if(a2%c1%len /= len(a2%c1) .or. a2%c1%len /= 4)    error stop 11_4  
  if(any(a2%c1 /= ['1230','4560','7890']))           error stop 12_4
  if(a2%c2%len /= len(a2%c2) .or. a2%c2%len /= 6)    error stop 13_4
  if(a2%c2 /= "xlftes")                              error stop 14_4

  a3=a2

  if(a3%l /= 3)                                      error stop 15_4
  if(a3%c1%len /= len(a3%c1) .or. a3%c1%len /= 4)    error stop 16_4
  if(any(a3%c1 /= ['1230','4560','7890']))           error stop 17_4
  if(a3%c2%len /= len(a3%c2) .or. a3%c2%len /= 6)    error stop 18_4
  if(a3%c2 /= "xlftes")                              error stop 19_4

  a4=>a1

  if(a4%l /= 3)                                      error stop 20_4
  if(a4%c1%len /= len(a4%c1) .or. a4%c1%len /= 4)    error stop 21_4
  if(any(a4%c1 /= ['1230','4560','7890']))           error stop 22_4
  if(a4%c2%len /= len(a4%c2) .or. a4%c2%len /= 6)    error stop 23_4
  if(a4%c2 /= "xlftes")                              error stop 24_4

  a5=a1

  if(a5%l /= 3)                                      error stop 25_4
  if(a5(1)%c1%len /= len(a5(1)%c1)  &
     .or. a5(1)%c1%len /= 4)                         error stop 26_4
  if(a5(2)%c1%len /= len(a5(2)%c1)  &
     .or. a5(2)%c1%len /= 4)                         error stop 27_4

  if(any(a5(1)%c1 /= ['1230','4560','7890']))        error stop 28_4
  if(any(a5(2)%c1 /= ['1230','4560','7890']))        error stop 29_4
  if(a5(1)%c2%len /= len(a5(1)%c2) & 
        .or. a5(1)%c2%len /= 6)                      error stop 30_4
  if(a5(2)%c2%len /= len(a5(2)%c2) &
        .or. a5(2)%c2%len /= 6)                      error stop 31_4
  if(a5(1)%c2 /= "xlftes")                           error stop 32_4
  if(a5(2)%c2 /= "xlftes")                           error stop 33_4 

  a6=a5
  if(a6%l /= 3)                                      error stop 34_4
  if(a6(1)%c1%len /= len(a6(1)%c1)  &
     .or. a6(1)%c1%len /= 4)                         error stop 35_4
  if(a6(2)%c1%len /= len(a6(2)%c1)  &
     .or. a6(2)%c1%len /= 4)                         error stop 36_4

  if(any(a6(1)%c1 /= ['1230','4560','7890']))        error stop 37_4
  if(any(a6(2)%c1 /= ['1230','4560','7890']))        error stop 38_4
  if(a6(1)%c2%len /= len(a6(1)%c2) &
        .or. a6(1)%c2%len /= 6)                      error stop 39_4
  if(a6(2)%c2%len /= len(a6(2)%c2) &
        .or. a6(2)%c2%len /= 6)                      error stop 40_4
  if(a6(1)%c2 /= "xlftes")                           error stop 41_4
  if(a6(2)%c2 /= "xlftes")                           error stop 42_4 

  a7=>a5
  if(a7%l /= 3)                                      error stop 43_4
  if(a7(1)%c1%len /= len(a7(1)%c1)  &
     .or. a7(1)%c1%len /= 4)                         error stop 44_4
  if(a7(2)%c1%len /= len(a7(2)%c1)  &
     .or. a7(2)%c1%len /= 4)                         error stop 45_4

  if(any(a7(1)%c1 /= ['1230','4560','7890']))        error stop 46_4
  if(any(a7(2)%c1 /= ['1230','4560','7890']))        error stop 47_4
  if(a7(1)%c2%len /= len(a7(1)%c2) &
        .or. a7(1)%c2%len /= 6)                      error stop 48_4
  if(a7(2)%c2%len /= len(a7(2)%c2) &
        .or. a7(2)%c2%len /= 6)                      error stop 49_4
  if(a7(1)%c2 /= "xlftes")                           error stop 50_4
  if(a7(2)%c2 /= "xlftes")                           error stop 51_4

end
