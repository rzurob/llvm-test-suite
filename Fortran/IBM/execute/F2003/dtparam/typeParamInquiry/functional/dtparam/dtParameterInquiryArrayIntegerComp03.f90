!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryArrayIntegerComp03.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 13 2008 
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
!* 2. TYPE PARAMETER INQUIRY FOR DT AND COMPONENT
!* 3. DIFFERENT TYPE PARAMETER
!* 4. INTEGER ARRAY COMPONENT
!* 5. DEFECT 353566
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k,l)
     integer(1),kind  :: k=2
     integer(k),len   :: l=k
     integer(k+k), dimension(k%kind+k%kind) :: i1
     integer(k*2) :: i2(2+l%kind) 
     integer(2)   :: i3(kind(l)+kind(l))
     integer(4),dimension(l%kind) :: i4 
     integer(8),dimension(kind(k+k)+k%kind) :: i5=kind(k+k)+k%kind
   end type 
         
end module

  program dtParameterInquiryArrayIntegerComp03
  use m
  implicit none

  type(base)  :: t

  if(t%k /= 2)                                           error stop 10_4
  if(t%l /= 2)                                           error stop 11_4
  if(t%k%kind /= kind(t%k) .or. t%k%kind /= 1)           error stop 12_4 
  if(t%i1%kind /= kind(t%i1) .or. t%i1%kind /= 4)        error stop 13_4
  if(t%i2%kind /= kind(t%i2) .or. t%i2%kind /= 4)        error stop 14_4
  if(t%i3%kind /= kind(t%i3) .or. t%i3%kind /= 2)        error stop 15_4
  if(t%i4%kind /= kind(t%i4) .or. t%i4%kind /= 4)        error stop 16_4
  if(t%i5%kind /= kind(t%i5) .or. t%i5%kind /= 8)        error stop 17_4
  
  if(lbound(t%i1,1) /=1 .or. ubound(t%i1,1) /= 2)        error stop 18_4
  if(lbound(t%i2,1) /=1 .or. ubound(t%i2,1) /= 4)        error stop 19_4
  if(lbound(t%i3,1) /=1 .or. ubound(t%i3,1) /= 2)        error stop 20_4
  if(lbound(t%i4,1) /=1 .or. ubound(t%i4,1) /= 2)        error stop 21_4
  if(lbound(t%i5,1) /=1 .or. ubound(t%i5,1) /= 2)        error stop 22_4
  if(any(t%i5 /= 2))                                     error stop 23_4

  end
