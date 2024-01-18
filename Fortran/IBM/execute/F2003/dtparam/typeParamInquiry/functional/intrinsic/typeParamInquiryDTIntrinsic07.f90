!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryDTIntrinsic07.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 8 2008 
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE 
!* 3. COMPONENT IS DERIVED TYPE
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   
   type first 
     character(:),allocatable :: c1 
     character(:),pointer     :: p1=>null()
     integer(2)               :: i1=1
   end type
   type second
     character(:),allocatable :: c2
     character(:),pointer     :: p2=>null()
     integer(4)               :: i2=2
     type(first) :: t1
   end type
end module

  program typeParamInquiryDTIntrinsic07
  use m
  implicit none

  type(second) :: t
  
  if(t%i2%kind /= kind(t%i2) .or. t%i2%kind /= 4)          error stop 10_4
  if(t%t1%i1%kind /= kind(t%t1%i1) .or. t%t1%i1%kind /=2)  error stop 11_4

  allocate(t%t1%c1,source="xlftest"(:3))
  allocate(t%t1%p1,source="fortran language test"(8:))
  allocate(t%c2,source="IBM TORONTO")
  allocate(t%p2,source=t%t1%p1(2:))

  if(t%t1%c1%len /= len(t%t1%c1) .or. t%t1%c1%len /= 3)    error stop 12_4
  if(t%t1%p1%len /= len(t%t1%p1) .or. t%t1%p1%len /= 14)   error stop 13_4
  if(t%c2%len /= len(t%c2) .or. t%c2%len /= 11)            error stop 14_4
  if(t%p2%len /= len(t%p2) .or. t%p2%len /= 13)            error stop 15_4 
  
end
